source("Housing_Insecurity_Functions.R")
libs <- c("sf", "mapview", "spdep", "spatialreg", "leaflet.extras2", "ape", "tigris",
          "dplyr", "tidycensus", "data.table", "stringr", "spgwr", "ggplot2", "nnet",
          "caret", "gridExtra", "stringr", 'tidyr', 'foreach', 'doParallel')

lapply(libs, require, character.only = T)
remove(libs)

library(sf)

options(tigris_use_chache = T)
options(tigris_year = 2019)

set.seed(123)


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
  clean_map <- st_read("census_tracts.shp")
  print("clean_map created")
}
# clean_map <- create_maps(fips)

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
      # df$STATEFP[i] <- fips_name
      df$abb[i] <- fips_abb
    } else {
      # Handle cases where there's no matching FIPS code
      df$STATEFP[i] <- NA
    }
  }

}
rm(list = c("fips_abb", "fips_name", "i"))

if (!file.exists(file.path("final_data", "logistic_regression_results.csv"))){
  log_results <- perform_multinomial_analysis(df, acs_vars)%>%
    select(-contains("."))
  write.csv(log_results, file.path("final_data", "logistic_regression_results.csv"), row.names = FALSE)
  print("Logistic Regression Results Created")
} else{
  log_results <- read.csv(file.path("final_data", "logistic_regression_results.csv")) %>%
    mutate(GEOID = as.character(GEOID))
  print("Logistic Regression Results already stored")
}

log_results <- st_as_sf(left_join(log_results, map[, c("GEOID")], by = "GEOID"))
# log_results <- left_join(log_results, st_drop_geometry(df[,c("GEOID", "abb")]), by = "GEOID")

# write.csv(st_drop_geometry(log_results), file.path("final_data", "logistic_regression_results.csv"), row.names = FALSE)


clusters <- st_drop_geometry(log_results) %>%
  dplyr::select(matches("(_Cluster|_Prob|GEOID)"))  %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate_at(vars(contains("_Prob")),
            list(~ifelse(!is.na(.), (.-min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE)), .)))

if (!exists("blended_colors")) {
  color_lists <- list(
    emp =  lapply(sector_colors("Emp_Cluster", "Emp_Prob"), function(vals) {
      saturate_colors(vals[1:3], vals[4])
    }),
    dem = lapply(sector_colors("Dem_Cluster", "Dem_Prob"), function(vals) {
      saturate_colors(vals[1:3], vals[4])
    }),
    trans_edu = lapply(sector_colors("Trans_EDU_Cluster", "Trans_edu_Prob"), function(vals) {
      saturate_colors(vals[1:3], vals[4])
    }),
    trans_pov = lapply(sector_colors("Trans_POV_Cluster", "Trans_pov_Prob"), function(vals) {
      saturate_colors(vals[1:3], vals[4])
    }),
    qual = lapply(sector_colors("Qual_Cluster", "Qual_Prob"), function(vals) {
      saturate_colors(vals[1:3], vals[4])
    }),
    hh_type = lapply(sector_colors("Hhtype_Cluster", "Hh_type_Prob"), function(vals) {
      saturate_colors(vals[1:3], vals[4])
    }),
    waid = lapply(sector_colors("Waid_Cluster", "Waid_Prob"), function(vals) {
      saturate_colors(vals[1:3], vals[4])
    }),
    cost = lapply(sector_colors("Cost_Cluster", "Cost_Prob"), function(vals) {
      saturate_colors(vals[1:3], vals[4])
    })

  )
  for (sector in names(color_lists)){
    names(color_lists[[sector]]) <- log_results$GEOID
  }

blended_colors <- color_lists[[1]]

for (i in 2:length(color_lists)) {
  blended_colors <- lapply(seq_along(blended_colors), function(j) {
    blend_rgb_lists(blended_colors[[j]], color_lists[[i]][[j]], 0.125)
  })
}
blended_colors <- lapply(blended_colors, convert_to_hex)
  names(blended_colors) <- log_results$GEOID
}

if (!exists("all_counties")) {
  all_counties <- counties(fips$FIPS_Code, cb = FALSE)
}

counties <- all_counties[!all_counties$GEOID %in% substr(log_results$GEOID,1,5), ]

census_tracts <- clean_map[!substr(clean_map$GEOID,1,5) %in% counties$GEOID & !clean_map$GEOID %in% log_results$GEOID, ]



# Maine <- create_state_map(23)
# Louisiana <- create_state_map(22)
# # Maine
# # Louisiana
#
### FIGURE 9 ###
national_map <-
  ggplot() +
  geom_sf(data = counties)  +
  geom_sf(data = census_tracts) +
  geom_sf(data = log_results, aes(fill = blended_colors), alpha = 0.5) +
  labs(title = "Rural Housing Insecurity Risk Levels") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 0)),
    plot.margin = margin(0, 0, 0, 0),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
national_map
#


plots_list <- lapply(names(regions), function(region_name) {
  region <- regions[[region_name]]
  ne_ct <- log_results[as.numeric(log_results$STATEFP) %in% region,]
  ne_color <- blended_colors[ne_ct$GEOID]
  ne_counties <- all_counties[!all_counties$GEOID %in% substr(ne_ct$GEOID,1,5), ]
  ne_counties <- ne_counties[as.numeric(ne_counties$STATEFP) %in% region,]

  ne_census_tracts <- clean_map[!substr(clean_map$GEOID,1,5) %in% ne_counties$GEOID & !clean_map$GEOID %in% ne_ct$GEOID,]
  ne_census_tracts <- ne_census_tracts[as.numeric(ne_census_tracts$STATEFP) %in% region,]
  ggplot() +
    geom_sf(data = ne_counties, color = NA) +
    geom_sf(data = ne_census_tracts, color = NA) +
    geom_sf(data = ne_ct, aes(fill = ne_color), alpha = 0.5, color = NA) +
    labs(title = paste(gsub("_", "", region_name))) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 0)),
      plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 0)),
      plot.margin = margin(0, 0, 0, 0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank()
    )

})

regional_map <- grid.arrange(grobs = plots_list,
             ncol = 2,
             shared_axis = TRUE)
regional_map

ggsave("/home/ar1stippus/thesis/maine-thesis/main/plots/regional_map.png", plot = regional_map, width = 8, height = 6, dpi = 300)
#


### Create rural population map
og_data <- read.csv("key_r_files/data/ct_data.csv")
og_data <- og_data[,c("GEOID", "population")]

log_results$GEOID <- as.numeric(log_results$GEOID)
# df$GEOID <- as.numeric(df$GEOID)

pop.ct <- left_join(log_results, og_data, by = "GEOID")

colnames(og_data) <- c("GEOID", "Population")

pop.other_ct <- clean_map[!clean_map$GEOID %in% pop.ct$GEOID,]

pop.map <- ggplot() +
  # geom_sf(data = pop.other_ct, color = 'black') +
  geom_sf(data = pop.ct, aes(fill = population), color = NA) +
  scale_fill_gradientn(colours = rev(gray.colors(100)), na.value = "gray80") +  # Reverse the color scaling and adjust the na.value color
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 0)),
    plot.margin = margin(0, 0, 0, 0),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
pop.map


ggsave("/home/steve-garcia/thesis2/maine-thesis/main/plots/pop_map.png", plot = pop.map, width = 6, height = 4, dpi = 300)
sum(pop.ct$population, na.rm = T)
mean(pop.ct$population, na.rm = T)


ruca_map <- ggplot() +
  geom_sf(data = pop.other_ct, color = NA) +
  geom_sf(data = df, aes(fill = RUCA),color = NA) +
  scale_fill_gradientn(colours = gray.colors(100)) +
  theme_minimal()
ruca_map

# ggsave("/home/steve-garcia/thesis2/maine-thesis/main/plots/ruca_map.png", plot = ruca_map, width = 6, height = 4, dpi = 300)

# region <- regions[[1]]
# rm(region_name)
high_risk.map <- lapply(names(regions), function(region_name) {
  region <- regions[[region_name]]
  ne_ct <- log_results[as.numeric(log_results$STATEFP) %in% region, ]
  ct <- log_results[as.numeric(log_results$STATEFP) %in% region, ]

  ct.high_risk <- ct %>%
    mutate(total = rowSums(select(st_drop_geometry(ct), contains("_Cluster"))))  %>%
    filter(total <= 12)

  ct.medium_risk <- ct %>%
    mutate(total = rowSums(select(st_drop_geometry(ct), contains("_Cluster"))))  %>%
    filter(total <= 15 & total > 12)

  ct.low_risk <- ct %>%
    mutate(total = rowSums(select(st_drop_geometry(ct), contains("_Cluster"))))  %>%
    filter(total > 15)

  ct <- ct[!ct$GEOID %in% c(ct.high_risk$GEOID, ct.medium_risk$GEOID, ct.low_risk$GEOID), ]

  ct.counties <- all_counties[!all_counties$GEOID %in% substr(ct$GEOID, 1, 5) & all_counties$STATEFP %in% region, ]
  # ct.counties <- ct.counties[as.numeric(ne_counties$STATEFP) %in% region,]

  ct.other_tracts <- clean_map[!substr(clean_map$GEOID,1,5) %in% ct.counties$GEOID &
                                 !clean_map$GEOID %in% ct$GEOID &
                                 as.numeric(clean_map$STATEFP) %in% region &
                                 !clean_map$GEOID %in% c(ct.high_risk$GEOID, ct.medium_risk$GEOID, ct.low_risk$GEOID),]


  ggplot() +
    geom_sf(data = ct.counties, color = NA) +
    geom_sf(data = ct.other_tracts, color = NA) +
    geom_sf(data = ct, color = NA) +
    geom_sf(data = ct.high_risk, fill = "red", color = NA) +
    geom_sf(data = ct.medium_risk, fill = "yellow", color = NA) +
    labs(title = paste(gsub("_", "", region_name))) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 0)),
      plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 0)),
      plot.margin = margin(0, 0, 0, 0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank()
    )

})

high_risk.map.grid <- grid.arrange(grobs = high_risk.map,
                          ncol = 2, top = NULL,
                          shared_axis = TRUE)

high_risk.map.grid

ggsave("/home/ar1stippus/thesis/maine-thesis/main/plots/regional_risk_map.png", plot = high_risk.map.grid, width = 6, height = 4, dpi = 300)

