source("time_series_processing/TS_Housing_Insecurity_Functions.R")
libs <- c("sf", "spdep", "spatialreg", "leaflet.extras2", "ape", "tigris",
          "dplyr", "tidycensus", "data.table", "stringr", "spgwr", "ggplot2", "nnet",
          "caret", "gridExtra", "stringr", "tidyr", "foreach", "doParallel")

lapply(libs, require, character.only = T)
remove(libs)

options(tigris_use_chache = T)
options(tigris_year = 2019)




if (!("emp_cluster" %in% acs_vars$emp_vars)) {
  acs_vars <- add_acs_clusters(acs_vars)
  print("Clusters Added to acs_vars")
} else{
  print("Clusters already exist in acs_vars")
}

#_____________________________________________________________________________________________________________________
# read in data, create map
#_____________________________________________________________________________________________________________________
fips <- read.csv("key_r_files/data/fips_codes.csv", header = TRUE, fileEncoding = "UTF-8-BOM")

if (!exists("ct_data")) {
  ct_data <- read.csv(file.path("final_data", "relabeled_ct_data.csv"), header = TRUE, fileEncoding = "UTF-8-BOM", row.names = "location")
  print("ct_data loaded")
} else{
  print("ct_data already exists")
}

if (!exists("clean_map")) {
  clean_map <- st_read("census_tracts.shp")
  print("clean_map created")
}
#
if (!exists("all_counties")) {
  all_counties <- st_read("counties.shp")
  print("Counties created")
}

neighbors <- read.csv("time_data/2019rural_neighbors_ct_data.csv")

#_____________________________________________________________________________________________________________________
# merge spatial data with processed data
#_____________________________________________________________________________________________________________________
map <- clean_map %>%
  mutate(GEOID = as.numeric(GEOID))

data <- st_as_sf(left_join(ct_data, map[,"GEOID"], by = "GEOID")) %>%
  select(-X)


clusters <- st_drop_geometry(data) %>%
  dplyr::select(contains("_cluster")) %>%
  mutate(RISK = rowSums(.))

data <- data %>%
  mutate(RISK = as.numeric(clusters$RISK / 24)) %>%
  rename_all(tolower) %>%
  na.omit(.)

# data.urban <- data %>% filter(RUCA >= 1 & RUCA <= 4)
# write.csv(st_drop_geometry(data), file.path("final_data", "map_processed_data.csv"), row.names = F)

#_____________________________________________________________________________________________________________________
# spatial autocorrelation
#_____________________________________________________________________________________________________________________

if (!exists("rural_weights")) {
  rural_weights <- nb2listw(poly2nb(data, queen = TRUE), style = "W", zero.policy = TRUE)
  print("rural weights created")
} else{
  print("rural weights already exists")
}

cluster_cols <- st_drop_geometry(data) %>%
  dplyr::select(contains("_cluster"))%>%
  colnames()
# cluster_cols <- cluster_cols[cluster_cols != "RISK"]

moran_rural <- list()

for (cluster in cluster_cols){
  local <- localmoran(x = data[[cluster]], listw = rural_weights, zero.policy = T)
  colnames(local) <- c("Morans_I", "Expectation", "Variance", "Standard Deviation", "p-value")
  x <- cbind(data[,c(cluster, "geoid")], local)
  moran_rural <- append(moran_rural, list(x))
}
names(moran_rural) <- cluster_cols

for (cluster in cluster_cols){
moran <- moran_rural[[cluster]]
print(paste(cluster, nrow(moran[moran$p.value < 0.05,]), round(nrow(moran[moran$p.value < 0.05,]) / nrow(moran),2), round(mean(moran$Morans_I[moran$p.value < 0.05], na.rm = T),2)))
}

moran <- data.frame(geoid =data$geoid)
for (cluster in cluster_cols){
  x <- st_drop_geometry(moran_rural[[cluster]][,c(1:3,7)])
  prefix <- sub("^(.*?_).*", "\\1", colnames(x)[[1]])
  colnames(x) <- c(colnames(x)[1], "geoid", paste0(cluster,"_", colnames(x)[3]),paste0(cluster, "_",colnames(x)[4]))
  moran <- left_join(moran, x[,2:4], by = "geoid")
}


rural <- lapply(cluster_cols, function(cluster_name) {
  df <- moran_rural[[cluster_name]]
  df <- df[df$p.value < 0.05,]
  data.frame(
    sector = cluster_name,
    c1 = mean(df$Morans_I[df[[cluster_name]] == 1],na.rm = T),
    c2 = mean(df$Morans_I[df[[cluster_name]] == 2],na.rm = T),
    c3 = mean(df$Morans_I[df[[cluster_name]] == 3],na.rm = T)
  )
})
rural <- do.call(rbind, rural)

rural.avg.long <- gather(rural, key = "cluster", value = "Morans_I", c("c1", "c2", "c3"))


latex_table <- xtable(rural)
print(latex_table, include.rownames = F)


# Create a grouped bar plot
clus_avg_plot <- ggplot(rural.avg.long, aes(x = sector, y = Morans_I, fill = cluster)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("c1" = "green", "c2" = "blue", "c3" = "red")) +
  labs(title = "Mean Morans_I by Sector", x = "Sector", y = "Mean Morans_I") +
  theme_minimal()
clus_avg_plot


#_____________________________________________________________________________________________________________________
# map
#_____________________________________________________________________________________________________________________

counties <- all_counties[!all_counties$GEOID %in% substr(as.character(data$geoid),1,5), ]
census_tracts <- clean_map[
  !as.numeric(clean_map$GEOID) %in% data$geoid &
    !substr(as.character(clean_map$GEOID), 1, 5) %in% as.character(counties$GEOID),
]

layered_moran_map <- ggplot() +
  geom_sf(data = counties) +
  geom_sf(data = census_tracts) +
  geom_sf(data = data) +
  geom_sf(data = subset(moran_rural$trans_edu_cluster, p.value < 0.05), aes(fill = Morans_I), color = NA, alpha = 0.125, size = 1) +
  geom_sf(data = subset(moran_rural$trans_pov_cluster, p.value < 0.05), aes(fill = Morans_I), color = NA, alpha = 0.125, size = 1) +
  geom_sf(data = subset(moran_rural$hhtype_cluster, p.value < 0.05), aes(fill = Morans_I), color = NA, alpha = 0.125, size = 1) +
  geom_sf(data = subset(moran_rural$waid_cluster, p.value < 0.05), aes(fill = Morans_I), color = NA, alpha = 0.125, size = 1) +
  geom_sf(data = subset(moran_rural$emp_cluster, p.value < 0.05), aes(fill = Morans_I), color = NA, alpha = 0.125, size = 1) +
  geom_sf(data = subset(moran_rural$qual_cluster, p.value < 0.05), aes(fill = Morans_I), color = NA, alpha = 0.125, size = 1) +
  geom_sf(data = subset(moran_rural$cost_cluster, p.value < 0.05), aes(fill = Morans_I), color = NA, alpha = 0.125, size = 1) +
  geom_sf(data = subset(moran_rural$dem_cluster, p.value < 0.05), aes(fill = Morans_I), color = NA, alpha = 0.125, size = 1) +
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red") +
  labs(title = "Spatial Clusters",
       subtitle = "Statistically Significant Risk Levels",
       fill = "Morans_I") +
  theme_minimal()
layered_moran_map
