source("Housing_Insecurity_Functions.R")
libs <- c("dplyr", "ggplot2", "nnet", "gridExtra", "reshape", "tidyr", "stringr", "grid", "matrixStats")
#install.packages(libs)
lapply(libs, require, character.only = T)
remove(libs)

fips <- read.csv(file.path("key_r_files/data", "fips_codes.csv"), header = TRUE, fileEncoding = "UTF-8-BOM")

df <- read.csv(file.path("final_data", "map_processed_data.csv"), row.names = NULL)

neighbors_df <- read.csv(file.path("final_data", "rural_neighbors_ct_data.csv")) %>%
  select(-X) %>%
  # mutate(GEOID = as.character(GEOID)) %>%
  unique()
log_results <- read.csv(file.path("final_data", "log_results.csv"))


neighbors_df.counts <- as.data.frame(table(neighbors_df$state)) %>%
  dplyr::rename(State = Var1, Neighbor_Count = Freq)

### FIGURE 1 ###
ruca.frequency <- ggplot(df, aes(x = factor(RUCA))) +
  geom_bar(stat = "count") +
  labs(title = NULL, x = "RUCA", y = "Frequency") +
  scale_y_continuous(breaks = seq(0, max(3250), by = 250)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the title
  )
ruca.frequency

# ggsave("/home/steve-garcia/thesis2/maine-thesis/main/plots/ruca_frequency.png", plot = ruca.frequency, width = 6, height = 4, dpi = 300)

### FIGURE 2 ###

df.counts <- read.csv("final_data/rural_neighbors_ct_data.csv") %>%
  select(-X) %>%
  unique()

df.counts <- as.data.frame(table(df$STATEFP)) %>%
  dplyr::rename(State = Var1, State_Count = Freq)

df.counts <- merge(df.counts, neighbors_df.counts, by = "State") %>%
  mutate(State = as.character(State))

df.counts$Neighbor_Count <- df.counts$Neighbor_Count - df.counts$State_Count



for (i in 1:length(df.counts$State)) {
  fips_name <- fips$USPS_Code[fips$FIPS_Code == as.numeric(df.counts$State[i])]
  if (length(fips_name) > 0) {
    df.counts$abb[i] <- fips_name
  } else {
    df.counts$abb[i] <- NA
  }
}

# x <- df.counts_long %>%
#   group_by(State) %>%
#   summarise(n())

df.counts_long <- df.counts %>%
  pivot_longer(cols = c(State_Count, Neighbor_Count),
               names_to = "Count_Type",
               values_to = "Count")

df.counts_long$Count_Type[df.counts_long$Count_Type == "State_Count"] <- "State"
df.counts_long$Count_Type[df.counts_long$Count_Type == "Neighbor_Count"] <- "Neighbor"


df.counts.plot <- ggplot(df.counts_long, aes(x = abb, y = Count, fill = Count_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = NULL, x = "State", y = "Count") +
  scale_fill_manual(values = c("State" = "grey50", "Neighbor" = "grey80"),
                    name = "Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 65, vjust = 0.6, size = 8)
  )


ggsave("/home/steve-garcia/thesis2/maine-thesis/main/plots/neighbors.png", plot = df.counts.plot, width = 7, height = 5, dpi = 300)

# write.csv(df.counts_long, file.path("data_long.csv"))

### FIGURE 3 ###
plot_clusters <- na.omit(log_results) %>%
  dplyr::select(matches("_Cluster"))  %>%
  mutate_all(as.numeric) %>%
  gather(key = "Variable", value = "Value") %>%
  group_by(Variable, Value) %>%
  summarize(Frequency = n()) %>%
  arrange(Variable, Value) %>%
  mutate(Value = Value, Variable = str_replace(Variable, "_Cluster", ""))

plot_clusters$Variable[plot_clusters$Variable == "Emp"] <- "Econ"
plot_clusters$Value[plot_clusters$Value == 1] <- "High Risk"
plot_clusters$Value[plot_clusters$Value == 2] <- "Medium Risk"
plot_clusters$Value[plot_clusters$Value == 3] <- "Low Risk"

plot_clusters$Value <- as.factor(plot_clusters$Value)

clusters.bar <- ggplot(plot_clusters, aes(x = Variable, y = Frequency, fill = Value)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = seq(0, 3000, by = 200)) +
  labs(title = NULL, x = "Sector", y = "Frequency", fill = "Risk Level") +  # Modify the legend title
  scale_fill_manual(values = c("#FF0000", "#FFA500", "#008000"),  # Customizing colors
                    labels = c("High Risk", "Medium Risk", "Low Risk")) +  # Customizing legend labels
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle=65, vjust=0.6, size = 10))
clusters.bar

ggsave("/home/ar1stippus/thesis/maine-thesis/main/plots/cluster_distribution.png", plot = clusters.bar, width = 7, height = 5, dpi = 300)



moran <- read.csv(file.path("final_data", "moran_state_results.csv"), header = TRUE,fileEncoding = "UTF-8-BOM")

fips <- read.csv("key_r_files/data/fips_codes.csv", header = TRUE, fileEncoding = "UTF-8-BOM")


moran.plots <- list()

moran.significant <- moran %>%
  filter(p_value < 0.05)

regions.states <- lapply(regions, function(region){
  x <- fips$Name[fips$FIPS_Code %in% region]
})

moran.significant.states <- moran.significant %>%
  subset(., state != "all") %>%
  group_by(abb) %>%
  dplyr::select(-c(var_name, state, sector)) %>%
  summarize_all(~ mean(., na.rm = TRUE))


moran.significant.states <- lapply(names(regions), function(region_name) {
  region <- regions[[region_name]]
  fips_subset <- fips$Name[fips$FIPS_Code %in% region]
  subset_data <- moran.significant[moran.significant$state %in% fips_subset, ]
  return(subset_data)
})

names(moran.significant.states) <- names(regions)

### FIGURE 4 ###

moran.significant$sector[moran.significant$sector == "Transience: Poverty"] <- "RMP"
moran.significant$sector[moran.significant$sector == "Transience: Education"] <- "RME"
moran.significant$sector[moran.significant$sector == "Household Wage/ Aid"] <- "Household Factors"
moran.significant$sector[moran.significant$sector == "Employment"] <- "Economic Diversity"

moran.significant.states.plots <- lapply(names(moran.significant.states), function(region_name) {
  ggplot(moran.significant.states[[region_name]], aes(x = abb, y = Morans_I)) +
    geom_boxplot(width = 0.5, outlier.shape = 16, outlier.size = 3, outlier.colour = "black") +
    scale_y_continuous(breaks = seq(0, 1, by = .1)) +
    labs(title = gsub("_", "", region_name), x = NULL, y = NULL) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
    coord_flip() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      axis.text.y = element_text(size = 8)
    )
})

moran.plots$State <- grid.arrange(grobs = moran.significant.states.plots, left = "State", bottom="Moran's I" )
ggsave("/home/ar1stippus/thesis/maine-thesis/main/plots/moran_state.png", plot = moran.plots$State, width = 7, height = 5, dpi = 300)

### FIGURE 5 ###
moran.plots$Sector <- ggplot(moran.significant, aes(x = sector, y = Morans_I)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.size = 3, outlier.colour = "black") +
  scale_y_continuous(breaks = seq(0, 1, by = .1)) +
  labs(title = NULL, x = "Sector", y = "Moran's I") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  coord_flip() +
  theme_minimal()

moran.plots$Sector
ggsave("/home/ar1stippus/thesis/maine-thesis/main/plots/moran_sector.png", plot = moran.plots$Sector, width = 7, height = 5, dpi = 300)



### FIGURE 10 ###
prob <- log_results %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  dplyr::select(c(matches("_Prob"), abb))

colnames(prob) <- c("Dem_Prob", "RME_Prob", "RMP_Prob", "Cost_Prob", "Qual_Prob",
                    "Hh_Type_Prob", "HH_Factors_Prob", "Econ_Div_Prob", "abb")

prob.sector <- melt(prob)

prob.box <- ggplot(prob.sector, aes(x = variable, y = value)) +
  geom_boxplot(width = 0.5, outlier.shape = 16, outlier.size = 3, outlier.colour = "black") +
  scale_y_continuous(breaks = seq(0, 1, by = .1)) +
  labs(title = NULL, x = "Sector", y = "Probability") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  coord_flip() +
  theme_minimal()
prob.box

ggsave("/home/ar1stippus/thesis/maine-thesis/main/plots/prob_sector.png", plot = prob.box, width = 7, height = 5, dpi = 300)


prob.state <- prob %>%
  group_by(abb) %>%
  summarise_all(~ mean(., na.rm = TRUE)) %>%
  mutate(
    Avg = rowMeans(across(-abb)),
    Median = apply(across(-c(abb, Avg)), 1, median, na.rm = TRUE),
    std = sqrt(rowSums((across(-c(abb, Avg, Median)))^2) / (n() - 1))
  )


prob.state.plot.avg <- ggplot(prob.state, aes(x = factor(abb), y = Avg)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = Avg - std, ymax = Avg + std, color = "Standard Deviation"),
                width = 0.3) +
  # geom_hline(yintercept = mean(prob.state$Avg), color = "red", linetype = "dashed", size = 1) + # Add mean line
  labs(title = NULL, x = "State", y = "Average Probability",
       color = "Legend") +
  scale_y_continuous(breaks = seq(0, 1, by = .1)) +
  scale_color_manual(values = "black", labels = "Standard Deviation") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 8),
    axis.text.x = element_text(angle = 65, vjust = 0.6, size = 8)
  )

prob.state.plot.avg

ggsave("/home/ar1stippus/thesis/maine-thesis/main/plots/prob_state.png", plot = prob.state.plot.avg
  , width = 7, height = 5, dpi = 300)


selected_vars <- log_results %>%
  select(contains("_Prob"))

colnames(selected_vars) <- c("Demographics", "RME", "RMP", "Housing Costs", "Housing Quality", "Housing Type", "Household Factors", "Economic Diversity")

# Calculating correlations
correlation_matrix <- cor(selected_vars, use = "pairwise.complete.obs")

# Reshape the correlation matrix
cor_df <- as.data.frame(as.table(correlation_matrix))
names(cor_df) <- c("Var1", "Var2", "value")

# Plotting the correlation matrix
prob_corr <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  labs(title = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
prob_corr
ggsave("/home/ar1stippus/thesis/maine-thesis/main/plots/prob_corr.png", plot = prob_corr
  , width = 7, height = 5, dpi = 300)


### APPENDIX CORRELATION PLOT ###
corr.plots <- lapply(names(acs_vars), function(sector) {
  x <- cor(log_results[, acs_vars[[sector]]])

  correlation_matrix <- cor(x)

  # Melt the correlation matrix into long format for ggplot
  melted_correlation <- melt(correlation_matrix)
  colnames(melted_correlation) <- c("Var1", "Var2", "value")

  # Create the correlation plot
  ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), vjust = 1) +  # Add text labels
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
    theme_minimal() +
    labs(title = paste("Correlation Plot for", sector),
         x = "Variables",
         y = "Variables") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})


cor_plots <- grid.arrange(grobs = corr.plots, ncol = 2)


ggsave("/home/steve-garcia/thesis2/maine-thesis/main/plots/all_prob_corr.png", plot = cor_plots
  , width = 7, height = 5, dpi = 300)


correlation_plot


x <- cor(log_results[,acs_vars$hh_type_vars])
correlation_matrix <- cor(x)
melted_correlation <- melt(correlation_matrix)
colnames(melted_correlation) <- c("Var1", "Var2", "value")
correlation_plot <- ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  # Add text labels
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = NULL,
       x = "Variables",
       y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

correlation_plot

ggsave("/home/steve-garcia/thesis2/maine-thesis/main/plots/correlations/housing_type_prob_corr.png", plot = correlation_plot
  , width = 7, height = 5, dpi = 300)
