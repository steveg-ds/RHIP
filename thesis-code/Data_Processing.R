libs <- c("MatrixModels", "nloptr", "quantreg", "lme4", "car", "rstatix", 
          "FactoMineR", "ggpubr", "factoextra", "data.table", "dplyr", 
          "matrixStats", "cowplot", "corrplot", "cluster", "rpart", 
          "rpart.plot", "nnet", "kknn", "MASS", "class", "e1071", 
          "gridExtra", "plyr", "tibble", "NeuralNetTools", "caret", 
          "broom", "sf", "mapview", "spdep", "leaflet.extras2", "tigris", 
          "Hmisc", "PerformanceAnalytics", "ape", 'doParallel', 'foreach')

install.packages(libs)
lapply(libs, require, character.only=T)
source("Housing_Insecurity_Functions.R")
rm("libs")

options(tigris_use_chache = T)
options(tigris_year = 2019)

install.packages("factoextra")

#_____________________________________________________________________________________________________________________
# read in data
#_____________________________________________________________________________________________________________________
set.seed(123)



ruca <- fread(file.path("data", "ruca2010revised.csv"), header = TRUE)[, 3:4] %>%
  mutate(FIPS = as.character(FIPS))

ct_data <- fread(file.path("data", "ct_data.csv"), header = TRUE)  %>%
  mutate(GEOID = as.character(GEOID))

ct_data <- ct_data[ruca, on = c("GEOID" = "FIPS")]

if (!exists("state_neighbors")) {
  state_neighbors_df <- read.csv(file.path("key_r_files/data", "state_neighbors.csv"), header = TRUE, fileEncoding = "UTF-8-BOM")
  state_neighbors <- get_neighbors(state_neighbors_df, fips)
  state_neighbors <- state_neighbors[rowSums(is.na(state_neighbors)) != ncol(state_neighbors), ]
}


if (!file.exists(file.path("final_data", "rural_neighbors_ct_data.csv"))) {
  if (!exists("clean_map")) {
    clean_map <- create_maps(fips)
  }
  print("Running neighbors algorithm")
  states_list <- sample(state_neighbors$State,2)
  cl <- makeCluster(6)
  registerDoParallel(cl)
  clusterEvalQ(cl, lapply(c("dplyr", "sf"), require, character.only=T))

  neighbors_df <- foreach(state = state_neighbors$State, .combine = "rbind") %dopar% {
    print(state)
    state <- state_neighbors[state_neighbors$State == state,]
    state <- sprintf("%02d", state[!is.na(state)])

    map <- clean_map[clean_map$STATEFP %in% state,] %>%
      mutate(GEOID = ifelse(substr(GEOID, 1, 1) == "0", substr(GEOID, 2, nchar(GEOID)), GEOID))

    map <- left_join(map, ct_data, by = "GEOID")
    map <- map[map$RUCA >= 7 & map$RUCA <= 10, ] # filter by RUCA code
    x <- filter_data_by_neighbors(map, state)
  }
  stopCluster(cl)
  write.csv(neighbors_df, file.path("final_data", "rural_neighbors_ct_data.csv"))
} else{
  print("neighbors data already exists ")
  neighbors_df <- read.csv(file.path("final_data", "rural_neighbors_ct_data.csv"), row.names = NULL)
}

x <- neighbors_df %>%
  distinct() %>%
  group_by(STATEFP) %>%
  summarise()

neighbors_df <- read.csv(file.path("final_data", "rural_neighbors_ct_data.csv"), row.names = NULL)
neighbors_df <- neighbors_df %>%
  dplyr::select(-X) %>%
  mutate(GEOID = as.character(GEOID))

# write.csv(neighbors_df, file.path("rural_neighbors_ct_data.csv"))

state <- state_neighbors[state_neighbors$State == 32,]
state <- sprintf("%02d", state[!is.na(state)])

state_data <- neighbors_df[substr(neighbors_df$GEOID,1,2) %in% "32",] %>% dplyr::select(-state)

state_data <- unique(state_data[complete.cases(state_data), ])
processed_data <- process_state(state_data)
fin_cluster_df <- rbind(fin_cluster_df, processed_data[[1]])


states <- state_neighbors$State

fin_cluster_df <- data.frame()

time.total <- Sys.time()
for (state in states){
  time.start <- Sys.time()
  state <- state_neighbors[state_neighbors$State == state,]
  state <- sprintf("%02d", state[!is.na(state)])
  state_data <- neighbors_df[substr(neighbors_df$GEOID,1,2) %in% state,] %>% dplyr::select(-state)
  state_data <- unique(state_data[complete.cases(state_data), ])
  processed_data <- process_state(state_data)
  fin_cluster_df <- rbind(fin_cluster_df, processed_data[[1]])
  print((paste("State: ", state[[1]], "iteration time: ", round(Sys.time() - time.start, 2), "total time: ", round(Sys.time() - time.total, 2))))
}

write.csv(fin_cluster_df, file.path("key_r_files/final_data", "processed_ct_data.csv"))
