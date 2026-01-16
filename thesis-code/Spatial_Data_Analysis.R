source("Housing_Insecurity_Functions.R")
libs <- c("sf", "spdep", "spatialreg", "leaflet.extras2", "ape", "tigris",
          "dplyr", "tidycensus", "data.table", "stringr", "spgwr", "ggplot2", "nnet",
          "caret", "gridExtra", "stringr", "tidyr", "foreach", "doParallel")

lapply(libs, require, character.only = T)
remove(libs)

options(tigris_use_chache = T)
options(tigris_year = 2019)




if (!("Emp_Cluster" %in% acs_vars$emp_vars)) {
  acs_vars <- add_acs_clusters(acs_vars)
  print("Clusters Added to acs_vars")
} else{
  print("Clusters already exist in acs_vars")
}

#_____________________________________________________________________________________________________________________
# read in data, create map
#_____________________________________________________________________________________________________________________
fips <- read.csv(file.path("key_r_files/data", "fips_codes.csv"), header = TRUE, fileEncoding = "UTF-8-BOM")

if (!exists("ct_data")) {
  ct_data <- read.csv(file.path("final_data", "relabeled_ct_data.csv"), header = TRUE, fileEncoding = "UTF-8-BOM", row.names = "location")

  ct_data <- ct_data %>%
    dplyr::select(-X) %>%
    mutate(GEOID = as.character(GEOID))
  print("ct_data loaded")
} else{
  print("ct_data already exists")
}

if (!exists("clean_map")) {
  clean_map <- create_maps(fips)
  print("clean_map created")
}else{
  print("clean_map already stored")
}

if (!exists("state_neighbors")) {
  state_neighbors <- get_neighbors(read.csv(file.path("key_r_files/data", "state_neighbors.csv"), header = TRUE, fileEncoding = "UTF-8-BOM"), fips)
}
#_____________________________________________________________________________________________________________________
# merge spatial data with processed data
#_____________________________________________________________________________________________________________________
map <- clean_map %>%
  mutate(GEOID = ifelse(substr(GEOID, 1, 1) == "0", substr(GEOID, 2, nchar(GEOID)), GEOID))

df <- st_as_sf(left_join(ct_data, map[,"GEOID"], by = "GEOID"))
df$abb <- NULL

for (i in 1:length(df$STATEFP)) {
  if (df$STATEFP[i] != "all") {
    fips_name <- fips$Name[fips$FIPS_Code == as.numeric(df$STATEFP[i])]
    fips_abb <- fips$USPS_Code[fips$FIPS_Code == as.numeric(df$STATEFP[i])]
    if (length(fips_name) > 0) {
      df$abb[i] <- fips_abb
    } else {
      # Handle cases where there's no matching FIPS code
      df$STATEFP[i] <- NA
    }
  }
}
# write.csv(st_drop_geometry(df), file.path("final_data", "map_processed_data.csv"), row.names = F)

#_____________________________________________________________________________________________________________________
# spatial autocorrelation
#_____________________________________________________________________________________________________________________

if (!file.exists(file.path("final_data", "moran_state_results.csv"))){
  states <- c(unique(df$STATEFP), "all")

  moran <- foreach(state = states, .combine = "rbind") %do% {

    if (state != "all") {
      x <- neighbors_df[neighbors_df$state == state,]
      all_data <-  df[df$GEOID %in% x$GEOID,]
    } else{
      all_data <- df
    }
    morans_loop(state_data = all_data, state = state)
  }
  write.csv(moran, file.path("final_data", "moran_state_results.csv"), row.names = F)
} else{
  moran <- read.csv(file.path("final_data", "moran_state_results.csv"), header = TRUE,fileEncoding = "UTF-8-BOM")
  print("Moran State Results already exists")
}
# unlink(file.path("final_data", "moran_state_results.csv")) # use this line to delete moran_state_results.csv

# moran <- process_moran(moran)

moran.significant <- as.data.frame(moran) %>%
  filter(p_value < 0.05)
# lapply(moran.significant, summary)

### calculating outliers in moran.significant ###
# filter_sector <- "Employment"
# moran.outliers <- filter(moran.significant, sector == filter_sector)
# moran.q1 <- quantile(moran.outliers$Morans_I, 0.25)
# moran.q3 <- quantile(moran.outliers$Morans_I, 0.75)
# moran.iqr <- moran.q3 - moran.q1
# moran.lower <- moran.q1 - 1.5 * moran.iqr
# moran.upper <- moran.q3 + 1.5 * moran.iqr
#
# lower_outliers_count <- sum(moran.outliers$Morans_I < moran.lower)
# upper_outliers_count <- sum(moran.outliers$Morans_I > moran.upper)
#
# # Total outliers count
# total_outliers_count <- lower_outliers_count + upper_outliers_count
#
# moran.outliers.filtered <- moran.outliers %>%
#   filter(Morans_I < moran.lower | Morans_I > moran.upper)
#
#
# mean(moran.outliers.filtered$Morans_I)
# mean(moran.outliers$Morans_I)
#
# var_name <- "ag_for_fish_hunt_mining"
# mean(moran.outliers.filtered$Morans_I[moran.outliers.filtered$var_name == var_name])
# mean(moran.outliers$Morans_I[moran.outliers$var_name == var_name])

moran.state_avg <- moran.significant %>%
  subset(., state != "all") %>%
  group_by(abb) %>%
  dplyr::select(-c(var_name, state, sector, N)) %>%
  summarize_all(~ mean(., na.rm = TRUE))
# lapply(moran.state_avg, summary)
latex_table <- xtable(moran.state_avg)
print(latex_table, include.rownames = F)

moran.sector_avg <- moran.significant %>%
  group_by(sector) %>%
  filter(state != "all") %>%
  dplyr::select(-c(state, var_name, N, abb)) %>%
  summarize_all(~ mean(., na.rm = TRUE))

latex_table <- xtable(moran.sector_avg)
print(latex_table, include.rownames = F)


moran.high <- moran.significant %>%
  filter(Morans_I > 0.5 & state != "all")
# lapply(moran.sector_avg, summary)
 x<- data.frame(table(moran.high$state))


moran.var_avg <- moran.significant %>%
  group_by(sector, var_name) %>%
  filter(state != "all") %>%
  dplyr::select(-c(state, N, abb)) %>%
  summarize_all(~ mean(., na.rm = TRUE)) %>%
  arrange(sector, desc(var_name))



stargazer(moran.significant, type = "latex", title = "Moran's I Descriptive Statistics")

