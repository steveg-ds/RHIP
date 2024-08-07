library(xtable)
library(dplyr)
library(stargazer)
library(broom)
library(rpart)
library(caret)
library(knitr)
library(ggcorrplot)

moran <- read.csv(file.path("final_data", "moran_state_results.csv"), header = TRUE,fileEncoding = "UTF-8-BOM")
moran.sig <- moran %>%
    filter(p_value < 0.05)  %>%
  filter(state == "all")


moran_sig_table <- moran %>%
  filter(state == "all") %>%
  # arrange(desc(Morans_I)) %>%
  mutate(p_value = sapply(p_value, function(p_val) {
  if (p_val < 0.001) {
    return("***")
  } else if (p_val < 0.01) {
    return("**")
  } else if (p_val < 0.05) {
    return("*")
  } else {
    return("")
  }
})) %>%
  mutate(sector = ifelse(sector == lag(sector), "", sector),
         Morans_I = paste(round(Morans_I, digits = 2), p_value)) %>%
  select(sector, var_name, Morans_I)
# x$sector[1] <- "RUCA"

latex_table <- xtable(moran_sig_table)
print(latex_table, include.rownames = F)


x <- moran %>%
  filter(p_value < 0.05)






for (sector in names(acs_vars)){
  print(sector)
  create_multinomial_conf_table(x.data[, acs_vars[[sector]][(length(acs_vars[[sector]]) - 1): length(acs_vars[[sector]])]])

}


if (!("Emp_Cluster" %in% acs_vars$emp_vars)) {
  acs_vars <- add_acs_probs(acs_vars)
  acs_vars <- add_acs_clusters(acs_vars)
  acs_vars <- add_acs_preds(acs_vars)
}




x.data <- read.csv("final_data/logistic_regression_results.csv", header = TRUE, fileEncoding = "UTF-8-BOM")
#
# sector <- "emp_vars"
#
# for (sector in names(acs_vars)){
#
# }
#
# for (sector in names(acs_vars)){
#   x <- x.data[,acs_vars[[sector]]] %>%
#     select(-matches("_Prob|_Pred"))
#
#   var <- x %>%
#     select(contains("_Cluster")) %>%
#     colnames()
#
#   formula <- as.formula(paste0(var, " ~ ."))
#
#   model <- multinom(formula, data = x)
#
#   pred <- predict(model, newdata = x, type = "class")
#
#   results <- data.frame(actual = x[[var]], predicted = pred)
#   print(paste0("State ",sector, ":", round(nrow(results[results$actual == results$predicted, ]) / nrow(results),2)))
#
#   x <- x.data[, acs_vars[[sector]][(length(acs_vars[[sector]]) - 1): length(acs_vars[[sector]])]]
#   print(paste0("National ", sector, ":", round(nrow(x[x[,1] == x[,2],]) / nrow(x),2)))
# }

# ___________________________________________________________
# Probability by cluster analysis
# ___________________________________________________________

# x.data <- read.csv("final_data/logistic_regression_results.csv", header = TRUE, fileEncoding = "UTF-8-BOM")
# for (sector in names(acs_vars)){
#   x <- x.data[,acs_vars[[sector]]]
#
#   clus_col <- colnames(x)[grepl("_Cluster", colnames(x))]
#   clus_prob <- colnames(x)[grepl("_Prob", colnames(x))]
#
#   result <- x %>%
#     select(all_of(clus_col), all_of(clus_prob)) %>%
#     group_by(across(all_of(clus_col))) %>%
#     summarise(across(all_of(clus_prob), mean, na.rm = TRUE))
#
#   print(result)
# }

x <- log_results  %>% # created in mapping
  select(contains("_Prob"))

cols <- colnames(x)

above_avg <- data.frame(sector = NA, mean = NA, pct = NA)
for (col in colnames(x)){
  # Filter the dataframe to get values above the upper limit
  outliers <- x[x[[col]] > mean(x[[col]], na.rm = TRUE), ]
  above_avg <- rbind(above_avg, c(col, round(mean(outliers[[col]], na.rm = TRUE),2), round(nrow(outliers)/ nrow(x),2)))
}

above_avg <- na.omit(above_avg)
row.names(above_avg) <- above_avg$sector
xtable(above_avg[,-1])
# x <- x.data %>%
#   group_by(RUCA) %>%
#   select(all_of(vars)) %>%
#   summarize_all(~mean(., na.rm = TRUE))
#
# x <- as.data.frame(t(x))
# x <- x[-1,]
# colnames(x) <- c("R7", "R8", "R9", "R10")
# xtable(x)

vars <- unlist(acs_vars)

cols <- st_drop_geometry(df) %>%
  select(contains("_Cluster")) %>%
  colnames()

col <- "Emp_Cluster"
x <- df

results <- data.frame(sector = NA, RUCA = NA, Pct = NA)
for  (col in cols){
  results <- rbind(results, c(col, 10, round(nrow(x[x[[col]] == 1 & x$RUCA == 10,]) /nrow(x[x[[col]] == 1,]),2)))
  results <- rbind(results, c(col, 9, round(nrow(x[x[[col]] == 1 & x$RUCA == 9,]) /nrow(x[x[[col]] == 1,]),2)))
  results <- rbind(results, c(col, 8, round(nrow(x[x[[col]] == 1 & x$RUCA == 8,]) /nrow(x[x[[col]] == 1,]),2)))
  results <- rbind(results, c(col, 7, round(nrow(x[x[[col]] == 1 & x$RUCA == 7,]) /nrow(x[x[[col]] == 1,]),2)))
  # print()
}
results <- na.omit(results) %>%
  arrange(desc(Pct))

xtable(results)

nrow(x[x$RUCA == 7,]) / nrow(x)

x <- x.data

summary(x[, acs_vars$waid_vars])


x.data <- read.csv("final_data/logistic_regression_results.csv", header = TRUE, fileEncoding = "UTF-8-BOM")
x <- x.data %>%
  select(contains("_Cluster")) %>%
  mutate(total = rowSums(.)) %>%
  filter(total <= 12)

ct <- x.data
library(sf)
ct.high_risk <- ct %>%
  mutate(total = rowSums(select(st_drop_geometry(ct), contains("_Cluster")))) %>%
  filter(total <= 12) #%>%
  summarise_all(~mean(., na.rm = TRUE))


ct.medium_risk <- ct %>%
  mutate(total = rowSums(select(st_drop_geometry(ct), contains("_Cluster"))))  %>%
  filter(total <= 15 & total > 12) %>%
  # group_by(abb) %>%
  # count(abb)
  summarise_all(~mean(., na.rm = TRUE))

ct.low_risk <- ct %>%
  mutate(total = rowSums(select(st_drop_geometry(ct), contains("_Cluster"))))  %>%
  filter(total > 15) %>%
  # group_by(abb) %>%
  summarise_all(~mean(., na.rm = TRUE))


t_test_result <- t.test(t(ct.high_risk), t(ct.low_risk))

stargazer_htest = function (data, ...) {
  summary = data.frame(`Test statistic` = data$statistic,
                       DF = data$parameter,
                       `p value` = data$p.value,
                       `Alternative hypothesis` = data$alternative,
                       check.names = FALSE)
  stargazer(summary, flip = TRUE, summary = FALSE,
            notes = paste(data$method, data$data.name, sep = ': '), ...)
}

stargazer_htest(t_test_result)

x <- as.data.frame(t(rbind(ct.high_risk, ct.medium_risk, ct.low_risk)))
colnames(x) <- c("High_Risk", "Medium_Risk", "Low_Risk")
#
x <- cor(na.omit(x.data[,acs_vars$waid_vars]))
#
ggcorrplot(x,
           hc.order = TRUE, # Hierarchical clustering reordering
           # type = "upper", # Show only upper triangle
           lab = TRUE, # Show correlation coefficients
           method = "circle", # Display using circles
           colors = c("red", "white", "blue")
)
#
#


x <- log_results[,acs_vars$emp_vars] %>%
  select(-matches("_Prob"), -matches("_Cluster"))

stargazer(x, title = "Descriptive Statistics", align = TRUE, type = "latex")



calculate_medoid <- function(data) {
  distances <- as.matrix(dist(data))  # Calculate pairwise distances
  medoid_index <- which.min(rowSums(distances))  # Find the index of the row with the minimum sum of distances
  medoid <- data[medoid_index, ]  # Extract the medoid
  return(medoid)
}

library(ggplot2)

# Generate sample data
set.seed(123)
data <- c(rnorm(100, mean = 10, sd = 2), rnorm(100, mean = 20, sd = 3))

# Calculate mean, median, and medoid
mean_val <- mean(data)
median_val <- median(data)
medoid_val <- calculate_medoid(data)

# Create data frame for plotting
df <- data.frame(x = 1:length(data), y = data)

# Create scatterplot with mean, median, and medoid lines
scatterplot <- ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "darkblue") +
  geom_vline(xintercept = mean_val, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_val, color = "green", linetype = "dashed", size = 1) +
  geom_vline(xintercept = medoid_val, color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Scatterplot of Data with Mean, Median, and Medoid") +
  theme_minimal()

scatterplot

### analyzing association rule presence/ overlap ###
cluster_cols <- list(grep("_Cluster", colnames(x.data), value = TRUE))
library(stargazer)
library(tibble)

hrl <- list()

x <- x.data %>%
  select(abb, contains("_Cluster")) %>%
  mutate_all(~ ifelse(is.na(.), 0, .))


hrl$emp <- x %>%
  group_by(abb) %>%
  summarise(cts = n(),
            emp = round((sum(Emp_Cluster == 1) * 100)  / cts, 2),
            waid = round((sum(Emp_Cluster == 1 & Waid_Cluster == 1) * 100) / cts, 2),
            hhtype = round((sum(Emp_Cluster == 1 & Hhtype_Cluster == 1) * 100) / cts, 2),
            rmp = round((sum(Emp_Cluster == 1 & Trans_EDU_Cluster == 1) * 100) / cts, 2)) %>%
  arrange(abb) %>%
  column_to_rownames(var = "abb")

stargazer(hrl$emp, type = "latex", summary = FALSE, digits= 2, title="Employment High-Risk Rules by Percent of Census Tracts Per State", label="emp_hhr")

hrl$dem <- x %>%
  group_by(abb) %>%
  summarise(cts = n(),
            dem = round((sum(Dem_Cluster == 1) * 100)  / cts, 2),
            rmp = round((sum(Dem_Cluster == 1 & Trans_POV_Cluster == 1) * 100) / cts, 2),
            cost = round((sum(Dem_Cluster == 1 & Cost_Cluster == 1) * 100) / cts, 2),
            qual = round((sum(Dem_Cluster == 1 & Qual_Cluster == 1) * 100) / cts, 2),
            waid = round((sum(Dem_Cluster == 1 & Waid_Cluster == 1) * 100) / cts, 2),
            rme = round((sum(Dem_Cluster == 1 & Trans_EDU_Cluster == 1) * 100) / cts, 2)) %>%
  arrange(abb) %>%
  column_to_rownames(var = "abb")

stargazer(hrl$dem, type = "latex", summary = FALSE, digits= 2, title="Demographics High-Risk Rules by Percent of Census Tracts Per State", label="dem_hhr")

hrl$cost <- x %>%
  group_by(abb) %>%
  summarise(cts = n(),
            cost= round((sum(Cost_Cluster == 1) * 100)  / cts, 2),
            hhtype = round((sum(Cost_Cluster == 1 & Hhtype_Cluster == 1) * 100) / cts, 2),
            qual = round((sum(Cost_Cluster == 1 & Qual_Cluster == 1) * 100) / cts, 2),
            rmp = round((sum(Cost_Cluster == 1 & Trans_POV_Cluster == 1) * 100) / cts, 2),
            waid = round((sum(Cost_Cluster == 1 & Waid_Cluster == 1) * 100) / cts, 2),
            rme = round((sum(Cost_Cluster == 1 & Trans_EDU_Cluster == 1) * 100) / cts, 2),
            dem = round((sum(Cost_Cluster == 1 & Dem_Cluster == 1) * 100) / cts, 2)) %>%
  arrange(abb) %>%
  column_to_rownames(var = "abb")

stargazer(hrl$cost, type = "latex", summary = FALSE, digits= 2, title="Housing Costs High-Risk Rules by Percent of Census Tracts Per State", label="cost_hhr")

hrl$qual <- x %>%
  group_by(abb) %>%
  summarise(cts = n(),
            qual= round((sum(Qual_Cluster == 1) * 100)  / cts, 2),
            cost = round((sum(Qual_Cluster == 1 & Hhtype_Cluster == 1) * 100) / cts, 2),
            rmp = round((sum(Qual_Cluster == 1 & Trans_POV_Cluster == 1) * 100) / cts, 2),
            hhtype = round((sum(Qual_Cluster == 1 & Hhtype_Cluster == 1) * 100) / cts, 2),
            waid = round((sum(Qual_Cluster == 1 & Waid_Cluster == 1) * 100) / cts, 2),
            rme = round((sum(Qual_Cluster == 1 & Trans_EDU_Cluster == 1) * 100) / cts, 2),
            dem = round((sum(Qual_Cluster == 1 & Dem_Cluster == 1) * 100) / cts, 2)) %>%
  arrange(abb) %>%
  column_to_rownames(var = "abb")

stargazer(hrl$qual, type = "latex", summary = FALSE, digits= 2, title="Housing Quality High-Risk Rules by Percent of Census Tracts Per State", label="qual_hhr")

hrl$rme <- x %>%
  group_by(abb) %>%
  summarise(CTs = n(),
            RME= round((sum(Trans_EDU_Cluster == 1) * 100)  / CTs, 2),
            RMP = round((sum(Trans_EDU_Cluster == 1 & Trans_POV_Cluster == 1) * 100) / CTs, 2),
            Employment = round((sum(Trans_EDU_Cluster == 1 & Emp_Cluster == 1) * 100) / CTs, 2),
            cost = round((sum(Trans_EDU_Cluster == 1 & Cost_Cluster == 1) * 100) / CTs, 2),
            RMP = round((sum(Trans_EDU_Cluster == 1 & Trans_POV_Cluster == 1) * 100) / CTs, 2),
            waid = round((sum(Trans_EDU_Cluster == 1 & Waid_Cluster == 1) * 100) / CTs, 2),
            hhtype = round((sum(Trans_EDU_Cluster == 1 & Hhtype_Cluster == 1) * 100) / CTs, 2),
            Demographics = round((sum(Trans_EDU_Cluster == 1 & Dem_Cluster == 1) * 100) / CTs, 2)) %>%
  arrange(abb) %>%
  column_to_rownames(var = "abb")

stargazer(hrl$rme, type = "latex", summary = FALSE, digits= 2, title="RME High-Risk Rules by Percent of Census Tracts Per State", label="rme_hhr")

hrl$rmp <- x %>%
  group_by(abb) %>%
  summarise(CTs = n(),
            RMP= round((sum(Trans_POV_Cluster == 1) * 100)  / CTs, 2),
            cost = round((sum(Trans_POV_Cluster == 1 & Cost_Cluster == 1) * 100) / CTs, 2),
            waid = round((sum(Trans_POV_Cluster == 1 & Waid_Cluster == 1) * 100) / CTs, 2),
            qual = round((sum(Trans_POV_Cluster == 1 & Qual_Cluster == 1) * 100) / CTs, 2),
            hhtype = round((sum(Trans_POV_Cluster == 1 & Hhtype_Cluster == 1) * 100) / CTs, 2),
            rme = round((sum(Trans_POV_Cluster == 1 & Trans_EDU_Cluster == 1) * 100) / CTs, 2),
            Demographics = round((sum(Trans_POV_Cluster == 1 & Dem_Cluster == 1) * 100) / CTs, 2)) %>%
  arrange(abb) %>%
  column_to_rownames(var = "abb")

stargazer(hrl$rmp, type = "latex", summary = FALSE, digits= 2, title="RMP High-Risk Rules by Percent of Census Tracts Per State", label="rmp_hhr")

hrl$waid <- x %>%
  group_by(abb) %>%
  summarise(CTs = n(),
            waid= round((sum(Waid_Cluster == 1) * 100)  / CTs, 2),
            emp = round((sum(Waid_Cluster == 1 & Emp_Cluster == 1) * 100) / CTs, 2),
            cost = round((sum(Waid_Cluster == 1 & Cost_Cluster == 1) * 100) / CTs, 2),
            rmp = round((sum(Waid_Cluster == 1 & Trans_POV_Cluster == 1) * 100) / CTs, 2),
            hhtype = round((sum(Waid_Cluster == 1 & Hhtype_Cluster == 1) * 100) / CTs, 2),
            qual = round((sum(Waid_Cluster == 1 & Qual_Cluster == 1) * 100) / CTs, 2),
            rme = round((sum(Waid_Cluster == 1 & Trans_EDU_Cluster == 1) * 100) / CTs, 2),
            Demographics = round((sum(Waid_Cluster == 1 & Dem_Cluster == 1) * 100) / CTs, 2)) %>%
  arrange(abb) %>%
  column_to_rownames(var = "abb")

stargazer(hrl$waid, type = "latex", summary = FALSE, digits= 2, title="Household Factors High-Risk Rules by Percent of Census Tracts Per State", label="waid_hhr")

hrl$hhtype <- x %>%
  group_by(abb) %>%
  summarise(CTs = n(),
            hhtype= round((sum(Hhtype_Cluster == 1) * 100)  / CTs, 2),
            cost = round((sum(Hhtype_Cluster == 1 & Cost_Cluster == 1) * 100) / CTs, 2),
            emp = round((sum(Hhtype_Cluster == 1 & Emp_Cluster == 1) * 100) / CTs, 2),
            waid = round((sum(Hhtype_Cluster == 1 & Dem_Cluster == 1) * 100) / CTs, 2),
            rmp = round((sum(Hhtype_Cluster == 1 & Trans_POV_Cluster == 1) * 100) / CTs, 2),
            qual = round((sum(Hhtype_Cluster == 1 & Qual_Cluster == 1) * 100) / CTs, 2),
            rme = round((sum(Hhtype_Cluster == 1 & Trans_EDU_Cluster == 1) * 100) / CTs, 2),
            ) %>%
  arrange(abb) %>%
  column_to_rownames(var = "abb")

stargazer(hrl$hhtype, type = "latex", summary = FALSE, digits= 2, title="Housing Type High-Risk Rules by Percent of Census Tracts Per State", label="hhtype_hhr")