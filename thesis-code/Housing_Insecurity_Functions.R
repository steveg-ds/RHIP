# functions for cleaning and analyzing ACS data


#' Create Maps Function
#'
#' This function retrieves and combines spatial census tract data for each FIPS code provided.
#' @param fips A data frame where each row contains FIPS codes, with the FIPS code for
#'             each state located in the second column.
#' @return A data frame combining census tract data for all FIPS codes provided.
#' @examples
#' # Example usage:
#' combined_tracts <- create_maps(fips_data)
create_maps <- function(fips) {
  # Initialize an empty list to store data frames
  data_frames_list <- list()

  # Iterate over each row in the FIPS data frame
  for (i in 1:nrow(fips)) {
    # Extract the FIPS code for the current state from the second column
    state_fips <- fips[i, 2]

    # Use tryCatch to handle potential errors during data retrieval
    tryCatch({
      # Retrieve census tracts for the current FIPS code
      tracts_data <- tracts(state_fips, cb = FALSE)

      # Add the retrieved data frame to the list
      data_frames_list[[i]] <- tracts_data
      cat("Retrieved tracts for FIPS #", state_fips, "\n")
    }, error = function(e) {
      # Print error message if data retrieval fails
      cat("Error occurred while retrieving data for", state_fips, "\n")
      print(e)
    })
  }

  # Combine the list of data frames into a single data frame
  clean_map <- do.call(rbind, data_frames_list)

  # Return the combined data frame
  return(clean_map)
}

#' Retrieve Neighboring State FIPS Codes
#'
#' This function processes a data frame of state neighbors, converting state names to their FIPS codes and organizing them into a structured format where each row represents a state and columns list its neighboring states' FIPS codes.
#'
#' @param state_neighbors A data frame where each row represents a state, with columns containing the names of neighboring states. Empty strings are replaced with NA.
#' @param fips A data frame mapping state names to their FIPS codes.
#' @return A data frame where the row names are state names and columns contain the FIPS codes of neighboring states.
#' @examples
#' fips <- data.frame(
#'   Name = c("Alabama", "Alaska", "Arizona", "Arkansas"),
#'   FIPS_Code = c("01", "02", "04", "05")
#' )
#' state_neighbors <- data.frame(
#'   State = c("Alabama", "Alaska", "Arizona"),
#'   Neighbor1 = c("Georgia", "Washington", "New Mexico"),
#'   Neighbor2 = c("Mississippi", NA, "California")
#' )
#' get_neighbors(state_neighbors, fips)
get_neighbors <- function(state_neighbors, fips) {
  # Convert empty strings to NA and trim whitespace from all elements in one step
  state_neighbors[] <- lapply(state_neighbors, function(x) {
    x <- trimws(x)
    ifelse(x == "", NA, x)
  })
  
  # Set state names as row names and remove the State column
  row.names(state_neighbors) <- state_neighbors$State
  state_neighbors$State <- NULL
  
  # Convert state names to FIPS codes
  state_neighbors <- data.frame(lapply(state_neighbors, function(column) {
    fips$FIPS_Code[match(column, fips$Name)]
  }), stringsAsFactors = FALSE)
  
  # Set row names again after conversion
  row.names(state_neighbors) <- row.names(state_neighbors)
  
  return(state_neighbors)
}

#' Filter Data by Neighboring States
#'
#' This function filters a spatial dataset to include only the state of interest 
#' and tracts within a specified buffer distance around that state. It performs
#' spatial operations to obtain tracts that fall within the buffer zone of the 
#' target state and combines this information into a single spatial object.
#'
#' @param neighbors_map An `sf` (simple features) object containing polygons of states or tracts.
#' @param state A character vector of length 1 representing the FIPS code of the state of interest.
#'
#' @return An `sf` object containing the polygons of the state of interest and the tracts 
#'   within the buffer distance around the state, with geometry dropped for non-spatial operations.
#'
#' @importFrom sf st_buffer st_intersection st_drop_geometry
#' @export
filter_data_by_neighbors <- function(neighbors_map, state) {
  # Validate input parameters
  if (!"sf" %in% class(neighbors_map)) {
    stop("The 'neighbors_map' must be an 'sf' object.")
  }
  
  if (!is.character(state) || length(state) != 1) {
    stop("The 'state' parameter must be a single character vector.")
  }

  # Extract polygons for the specified state and other tracts
  state_polygon <- neighbors_map[neighbors_map$STATEFP == state, ]
  other_tracts <- neighbors_map[neighbors_map$STATEFP != state, ]

  # Define the buffer distance (20 miles in meters)
  buffer_distance <- 32186.9 

  # Apply buffering to the state polygon
  buffered_state <- st_buffer(state_polygon, buffer_distance)

  # Compute intersection of buffered state polygon with other tracts
  nearby_tracts <- st_intersection(other_tracts, buffered_state)

  # Combine the state polygon and intersected tracts into a single `sf` object
  state_df <- rbind(state_polygon, nearby_tracts[, colnames(state_polygon)])
  state_df$state <- as.numeric(state$STATEFP)

  # Drop geometry column for non-spatial operations and return the result
  return(st_drop_geometry(state_df))
}

#' Process State Data for Clustering and Statistical Analysis
#'
#' This function processes state data by applying k-medoids clustering to various
#' data subsets, and generates statistical summaries for each cluster. It includes
#' data cleaning, feature engineering, and statistical calculations for analysis.
#'
#' @param state_data A data frame containing state data with features for clustering.
#' @param acs_vars A list of variables for different sectors used in clustering.
#' @param state A list containing the state identifier.
#'
#' @return A list with two elements:
#' \item{data}{A data frame with processed state data including clustering results.}
#' \item{var_imp}{A data frame with variable importance statistics for each sector.}
#'
#' @importFrom dplyr filter distinct mutate group_by summarise_all
#' @importFrom cluster pam
#' @importFrom stats median
#' @export
process_state <- function(state_data, acs_vars, state) {

  # Helper function to perform k-medoids clustering
  kmedians <- function(df, k) {
    kmeds <- pam(df, k = k, medoids = c(1, 2, 3))
    return(as.factor(kmeds$clustering))
  }

  # Helper function to create sector statistics by clustering
  create_sector_stats <- function(data) {
    data %>%
      group_by(Cluster) %>%
      summarise_all(median)
  }

  # Initialize a list to store error states
  error_states <- list()

  # Error handling for data processing
  tryCatch({
    # Process state data
    df <- state_data %>%
      filter(rowSums(.[, 14:(ncol(.)-1)]) != 0) %>%
      distinct() %>%
      mutate(
        location = sapply(strsplit(location, ":"), "[", 1),
        location = gsub("Census Tract", "CT", location, fixed = TRUE),
        gini_index = ifelse(is.na(gini_index), mean(gini_index, na.rm = TRUE), gini_index),
        renter_owner_ratio = renter_pop / owner_pop,
        owner_computed_mortgage = owner_pop - mortgage_not_computed,
        owner_computed_no_mortgage = owner_pop - no_mortgage_not_computed,
        renter_computed = renter_pop - rent_not_computed,
        mortgage_high_cost = (mhc1 + mhc2 + mhc3 + mhc4) / owner_computed_mortgage,
        no_mortgage_high_cost = (nmhc1 + nmhc2 + nmhc3 + nmhc4) / owner_computed_no_mortgage,
        rent_high_cost = (rhc1 + rhc2 + rhc3 + rhc4) / renter_computed
      )
    
    # Set row names based on GEOID
    row.names(df) <- paste0("X", df$GEOID)

    # Define and process different sectors
    process_sector <- function(vars, sector_name) {
      sector_data <- na.omit(df[, vars] / df$population) %>%
        mutate(!!paste0(sector_name, "_Cluster") := as.numeric(kmedians(scale(.), 3))) %>%
        mutate(GEOID = gsub("X", "", row.names(.)))
      sector_stats <- create_sector_stats(sector_data) %>%
        mutate(Sector = sector_name, State = as.numeric(state[[1]]))
      return(sector_stats)
    }

    # Process each sector
    emp_stats <- process_sector(acs_vars$emp_vars, "emp")
    dem_stats <- process_sector(acs_vars$dem_vars, "dem")
    trans_edu_stats <- process_sector(acs_vars$trans_edu_vars, "trans.edu")
    trans_pov_stats <- process_sector(acs_vars$trans_pov_vars, "trans.pov")
    hhtype_stats <- process_sector(acs_vars$hh_type_vars, "hhtype")
    waid_stats <- process_sector(acs_vars$waid_vars, "waid")
    cost_stats <- process_sector(acs_vars$cost_vars, "cost")
    qual_stats <- process_sector(acs_vars$qual_vars, "qual")

    # Combine all sector statistics
    var_imp_dt <- dplyr::bind_rows(emp_stats, dem_stats, trans_edu_stats, trans_pov_stats, hhtype_stats, waid_stats, cost_stats, qual_stats)

    # Create final cluster data frame
    cluster_df <- state_data[state_data$STATEFP == as.numeric(state[[1]]), c("GEOID", "STATEFP", "location", "RUCA")] %>%
      distinct() %>%
      dplyr::left_join(emp_stats, by = "GEOID") %>%
      dplyr::left_join(dem_stats, by = "GEOID") %>%
      dplyr::left_join(trans_edu_stats, by = "GEOID") %>%
      dplyr::left_join(trans_pov_stats, by = "GEOID") %>%
      dplyr::left_join(cost_stats, by = "GEOID") %>%
      dplyr::left_join(qual_stats, by = "GEOID") %>%
      dplyr::left_join(hhtype_stats, by = "GEOID") %>%
      dplyr::left_join(waid_stats, by = "GEOID")

    return(list(data = cluster_df, var_imp = var_imp_dt))
    
  }, error = function(e) {
    cat("An error occurred for State", state[[1]], "Continuing to the next state.\n")
    error_states <<- c(error_states, state[[1]])
    return(NULL)
  })
}


add_acs_clusters <- function(acs_vars){
  acs_vars$emp_vars <- append(acs_vars$emp_vars, "Emp_Cluster")
  acs_vars$trans_edu_vars <- append(acs_vars$trans_edu_vars, "Trans_EDU_Cluster")
  acs_vars$trans_pov_vars <- append(acs_vars$trans_pov_vars, "Trans_POV_Cluster")
  acs_vars$cost_vars <- append(acs_vars$cost_vars, "Cost_Cluster")
  acs_vars$qual_vars <- append(acs_vars$qual_vars, "Qual_Cluster")
  acs_vars$hh_type_vars <- append(acs_vars$hh_type_vars, "Hhtype_Cluster")
  acs_vars$waid_vars <- append(acs_vars$waid_vars, "Waid_Cluster")
  acs_vars$dem_vars <- append(acs_vars$dem_vars, "Dem_Cluster")
  return(acs_vars)
}

add_acs_probs <- function(acs_vars){
  acs_vars$emp_vars <- append(acs_vars$emp_vars, "Emp_Prob")
  acs_vars$trans_edu_vars <- append(acs_vars$trans_edu_vars, "Trans_edu_Prob")
  acs_vars$trans_pov_vars <- append(acs_vars$trans_pov_vars, "Trans_pov_Prob")
  acs_vars$cost_vars <- append(acs_vars$cost_vars, "Cost_Prob")
  acs_vars$qual_vars <- append(acs_vars$qual_vars, "Qual_Prob")
  acs_vars$hh_type_vars <- append(acs_vars$hh_type_vars, "Hh_type_Prob")
  acs_vars$waid_vars <- append(acs_vars$waid_vars, "Waid_Prob")
  acs_vars$dem_vars <- append(acs_vars$dem_vars, "Dem_Prob")
  return(acs_vars)
}

add_acs_preds <- function(acs_vars){
  acs_vars$emp_vars <- append(acs_vars$emp_vars, "Emp_Pred")
  acs_vars$trans_edu_vars <- append(acs_vars$trans_edu_vars, "Trans_edu_Pred")
  acs_vars$trans_pov_vars <- append(acs_vars$trans_pov_vars, "Trans_pov_Pred")
  acs_vars$cost_vars <- append(acs_vars$cost_vars, "Cost_Pred")
  acs_vars$qual_vars <- append(acs_vars$qual_vars, "Qual_Pred")
  acs_vars$hh_type_vars <- append(acs_vars$hh_type_vars, "Hh_type_Pred")
  acs_vars$waid_vars <- append(acs_vars$waid_vars, "Waid_Pred")
  acs_vars$dem_vars <- append(acs_vars$dem_vars, "Dem_Pred")
  return(acs_vars)
}


summarise_by_variable <- function(sector_imp, sector_stats, statistic = "median"){
  # Calculate the mean importance and accuracy scores for each variable
  temp_imp <- sector_imp %>%
    dplyr::select(-c(Sector, State)) %>%
    group_by(variable) %>%
    summarise(importance = mean(importance, na.rm = TRUE),
              accuracy = mean(accuracy, na.rm = TRUE))

  if (statistic == "average"){
    sector_stats <- na.omit(sector_stats) %>%
      group_by(across(contains("Cluster"))) %>%
      summarise_all(~ mean(., na.rm = TRUE))
  } else {
    sector_stats <- na.omit(sector_stats) %>%
      group_by(across(contains("Cluster"))) %>%
      summarise_all(~ median(., na.rm = TRUE))
  }
  colnames(sector_stats)[1] <- "Cluster"

  temp_imp <- merge(temp_imp, as.data.frame(t(sector_stats)), by = "variable", by.y = "row.names")

  colnames(temp_imp) <- c("variable", "importance", "accuracy", "cluster_1", "cluster_2", "cluster_3")

  return(temp_imp)
}

morans_loop <- function(state_data, state){
  moran_results <- data.frame(
    state = character(),
    sector = character(),
    var_name = character(),
    N = integer(),
    Morans_I = double(),
    std_dev = double(),
    variance = double(),
    expectation = double(),
    p_value = double()
  )

  state_data <- na.omit(state_data)
  sector_data <- list("ruca" = state_data[, "RUCA"], "emp" = state_data[, acs_vars$emp_vars],
                      "dem" = state_data[, acs_vars$dem_vars], "trans.edu" = state_data[, acs_vars$trans_edu_vars],
                      "trans.pov" = state_data[, acs_vars$trans_pov_vars], "hhtype" = state_data[, acs_vars$hh_type_vars],
                      "cost" = state_data[, acs_vars$cost_vars], "waid" = state_data[, acs_vars$waid_vars],
                      "qual" = state_data[, acs_vars$qual_vars])

  lw <- nb2listw(poly2nb(state_data, queen = TRUE), style = "W", zero.policy = TRUE)

  for (sector in names(sector_data)) {
    data <- st_drop_geometry(sector_data[[sector]])
    for (variable in names(data)) {
      tryCatch({
        # Calculate Moran's I
        moran <- moran.test(na.omit(data[[variable]]), lw, zero.policy = TRUE, na.action = na.omit)

        # Create a result data frame row
        result <- data.frame(
          state = as.character(state),
          sector = sector,
          var_name = variable,
          N = nrow(data),
          Morans_I = moran$estimate[1],
          std_dev = moran$statistic,
          variance = moran$estimate[3],
          expectation = moran$estimate[2],
          p_value = moran$p.value
        )
        moran_results <- rbind(moran_results, result)
      }, error = function(e) {
        # Store the error message in the error list and print it
        error_message <- paste("Error in State:", state, ", Sector:", sector, ", Variable:", variable, "\n", e$message)
        cat(error_message, "\n")
      })
    }
  }
  return(moran_results)
}

calculate_morans <- function(states, all_data, state_neighbors){
  moran_results <- data.frame(
    state = character(),
    sector = character(),
    var_name = character(),
    N = integer(),
    Morans_I = double(),
    std_dev = double(),
    variance = double(),
    expectation = double(),
    p_value = double()
  )

  state_count <- 0
  total_time <- 0

  for (state in states){
    start <- Sys.time()
    if (state != "all") {
      x <- state_neighbors[state_neighbors$state == state,]
      all_data <-  all_data[all_data$GEOID %in% x$GEOID,]
    moran_results <- bind_rows(moran_results, morans_loop(state_data = all_data, state = state))
    total_time <- total_time + (Sys.time() - start)
    state_count <- state_count + 1
    cat("\n_____________________________\n", "States finished:", state_count, ", Pct. Finished:", (round((state_count / nrow(state_neighbors)) * 100, 2)),
        "%\nIteration Time:", round(Sys.time() - start,2), "S\nTotal Time: ", round(total_time, 2), "S", "\n_____________________________\n")
    }
  }
  return(moran_results)
}

perform_multinomial_analysis <- function(df, acs_vars) {
  cl <- makeCluster(4)
  registerDoParallel(cl)

  states <- unique(df$STATEFP)
  data <- st_drop_geometry(df) %>%
    mutate_at(vars(contains("_Cluster")), as.factor)

  log_results <- data.frame(GEOID = df$GEOID)

  # Loop through acs_vars using foreach
  results_list <- foreach(sector = names(acs_vars), .packages = c("dplyr", "nnet")) %dopar% {
    acs_sector <- acs_vars[[sector]]
    sector <- gsub("_vars", "", sector)
    cat(paste0(gsub("_vars", "", sector), "\n"))
    results_df <- data.frame()

    for (state in states) {
      train_data <- data[data$STATEFP != state, acs_sector]

      target_variable <- names(train_data)[length(train_data)]
      formula_str <- paste(target_variable, "~ .")
      formula_obj <- as.formula(formula_str)

      test_data <- data[data$STATEFP == state, acs_sector]

      train_data[[target_variable]] <- as.factor(train_data[[target_variable]])
      test_data[[target_variable]] <- as.factor(test_data[[target_variable]])

      model <- multinom(formula_obj, data = train_data)

      predicted_probs <- predict(model, newdata = test_data, type = "probs")
      predicted_classes <- predict(model, newdata = test_data)

      prob_var <- paste0(toupper(substr(paste0(sector, "_Prob"), 1, 1)), substr(paste0(sector, "_Prob"), 2, nchar(paste0(sector, "_Prob"))))
      pred_var <- paste0(toupper(substr(paste0(sector, "_Pred"), 1, 1)), substr(paste0(sector, "_Pred"), 2, nchar(paste0(sector, "_Pred"))))
      test_data <- cbind(test_data, predicted_probs)

      test_data[[pred_var]] <- predicted_classes
      test_data[[prob_var]] <- ifelse(test_data[[target_variable]] == 1, test_data[["1"]],
                                      ifelse(test_data[[target_variable]] == 2, test_data[["2"]],
                                             ifelse(test_data[[target_variable]] == 3, test_data[["3"]], 0)))

      test_data[[prob_var]] <- round(test_data[[prob_var]], 3)


      test_data <- test_data %>% dplyr::select(-c("1", "2", "3"))

      test_data[, "GEOID"] <- data[data$STATEFP == state, "GEOID"]
      results_df <- rbind(results_df, test_data)
    }

    results_df
  }
  stopCluster(cl)

  log_results <- bind_cols(results_list)

  return(log_results)
}

create_multinomial_conf_table <- function(data){
  actual <- data[, 1]
  predictions <- data[, 2]
  conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(actual), mode = "prec_recall")
  conf_matrix_data <- as.matrix(conf_matrix$table)


  # cat(stargazer(model, type = "text" ))
  cat("\\begin{table}[!htbp]\n",
      "    \\small\n",
      "    \\centering\n",
      "    \\caption{Confusion Matrix and Statistics}\n",
      "    \\label{tab:confusion}\n",
      "    \\begin{tabular}{lccc}\n",
      "        \\toprule\n",
      "        & \\textbf{High Risk} & \\textbf{Medium Risk} & \\textbf{Low Risk} \\\\\n",
      "        \\midrule\n",
      "        \\textbf{High Risk} &", paste(conf_matrix_data[1, ], collapse = " & "), "\\\\\n",
      "        \\textbf{Medium Risk} &", paste(conf_matrix_data[2, ], collapse = " & "), "\\\\\n",
      "        \\textbf{Low Risk} &", paste(conf_matrix_data[3, ], collapse = " & "), "\\\\\n",
      "        \\bottomrule\n",
      "        \\midrule\n",
      "        Precision &", paste(round(conf_matrix$byClass[,"Precision"],2), collapse = " & "), "\\\\\n",
      "        Recall &", paste(round(conf_matrix$byClass[,"Recall" ],2), collapse = " & "), "\\\\\n",
      "        F1 &", paste(round(conf_matrix$byClass[,"F1" ],2), collapse = " & "), "\\\\\n",
      "        Prevalence &", paste(round(conf_matrix$byClass[,"Prevalence" ],2), collapse = " & "), "\\\\\n",
      "        Detection Rate &", paste(round(conf_matrix$byClass[,"Detection Rate" ],2), collapse = " & "), "\\\\\n",
      "        Detection Prevalence &", paste(round(conf_matrix$byClass[,"Detection Prevalence"],2), collapse = " & "), "\\\\\n",
      "        Balanced Accuracy &", paste(round(conf_matrix$byClass[,"Balanced Accuracy" ],2), collapse = " & "), "\\\\\n",
      "        \\bottomrule\n",
      "    \\end{tabular}\n",
      "\\end{table}\n")
}

process_moran <- function(temp_moran){
  temp_moran$abb <- 0
  for (i in 1:length(temp_moran$state)) {
    if (temp_moran$state[i] != "all") {
      fips_name <- fips$Name[fips$FIPS_Code == as.numeric(temp_moran$state[i])]
      fips_abb <- fips$USPS_Code[fips$FIPS_Code == as.numeric(temp_moran$state[i])]
      if (length(fips_name) > 0) {
        temp_moran$state[i] <- fips_name
        temp_moran$abb[i] <- fips_abb
      } else {
        temp_moran$state[i] <- NA
      }
    }
  }

  temp_moran$sector[temp_moran$sector == "waid"] <- "Household Wage/ Aid"
  temp_moran$sector[temp_moran$sector == "cost"] <- "Housing Cost"
  temp_moran$sector[temp_moran$sector == "hhtype"] <- "Housing Type"
  temp_moran$sector[temp_moran$sector == "emp"] <- "Employment"
  temp_moran$sector[temp_moran$sector == "trans.edu"] <- "Transience: Education"
  temp_moran$sector[temp_moran$sector == "trans.pov"] <- "Transience: Poverty"
  temp_moran$sector[temp_moran$sector == "dem"] <- "Demographics"
  temp_moran$sector[temp_moran$sector == "qual"] <- "Housing Quality"
  temp_moran$sector[temp_moran$sector == "ruca"] <- "RUCA"

  return(temp_moran)
}

blend_rgb_lists <- function(rgb_list1, rgb_list2, weight) {
  if (weight < 0.0 || weight > 1.0) {
    stop("The blending weight should be between 0.0 and 1.0")
  }

  if (length(rgb_list1) != length(rgb_list2)) {
    stop("Input lists must have the same length")
  }

  blended_rgb <- numeric(length(rgb_list1))
  for (i in 1:length(rgb_list1)) {
    # Calculate the blended RGB value for each channel
    blended_channel <- (1 - weight) * rgb_list1[i] + weight * rgb_list2[i]
    blended_rgb[i] <- blended_channel
  }
  return(blended_rgb)
}

saturate_colors <- function(rgb_vector, probability = 0) {
  color <- rgb_vector + (c(1, 1, 1) - rgb_vector / 255) * probability
  color <- pmin(1, color)  # Ensure that color values are in [0, 1]
  return(color)
}

sector_colors <- function(sector_clusters, sector_probability){
  colors <- list(
    red = c(255, 0, 0),
    orange = c(255, 165, 0),
    green = c(0, 128, 0),
    white = c(255, 255, 255)
  )

  temp_list <- lapply(1:nrow(clusters), function(i) {
    temp_list_cluster <- clusters[i, sector_clusters]
    temp_list_prob <- clusters[i, sector_probability]

    if (temp_list_cluster == 1) {
      return(c(colors$red, temp_list_prob))
    } else if (temp_list_cluster == 2) {
      return(c(colors$orange, temp_list_prob))
    } else if (temp_list_cluster == 3) {
      return(c(colors$green, temp_list_prob))
    } else {
      return(c(colors$white,1))
    }
  })
    return(temp_list)
  }

convert_to_hex <- function(rgb_vector) {
  # Scale the RGB values to 255 range and round to integers
  scaled_rgb <- round(rgb_vector * 255)
  hex_color <- rgb(scaled_rgb[1], scaled_rgb[2], scaled_rgb[3], maxColorValue = 255)
  return(hex_color)
}

filter_map_data <- function(data, state = NULL){
  if (!is.null(state)){
      data <- data[data$STATEFP == state,]
  }
  return(data)
}

filter_ct_data <- function(data, state = NULL, sector = NULL){
  if(is.null(sector)){
    data <- data[data$STATEFP == state,]
    print(paste0("Filtered By State: ", data$Name[1], " Observations: ", nrow(data)))
  } else if (is.null(state)){
    data <- data[, c("Name", "GEOID", acs_vars[[sector]])]
    print(paste0("Filtered By sector: ", sector, ", Observations: ", nrow(data)))
  } else{
    data <- data[data$STATEFP == state, c("Name", "GEOID", acs_vars[[sector]])]
    print(paste0("Filtered By State:", data$Name[1], ", Filtered By sector: ", sector , ", Observations: ", nrow(data)))
  }
  st_geometry(data) <- "geometry"
  return(data)
}

create_state_map <- function(state){
  temp_ct_data <- filter_map_data(log_results, state)
  temp_counties <- filter_map_data(all_counties, state)
  temp_census_tracts <- filter_map_data(census_tracts, state)
  temp_colors <- blended_colors[which(names(blended_colors) %in% temp_ct_data$GEOID)]
  temp_map <- ggplot() +
    geom_sf(data = temp_counties)  +
    geom_sf(data = temp_census_tracts) +
    geom_sf(data = temp_ct_data, aes(fill = temp_colors), alpha = 0.5) +
    labs(title = temp_ct_data$Name[1], subtitle = "All Sectors") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 0)),
      plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 0)),
      plot.margin = margin(0, 0, 0, 0)
    )

  return(temp_map)
}

create_sector_map <- function(state = NULL, sector = NULL){
  # sector <- names(acs_vars)[[6]]
  sector <- "dem_vars"
  state <- NULL
  temp_census_tracts <- filter_map_data(census_tracts, state)
  temp_counties <- filter_map_data(all_counties, state)
  temp_ct_data <- final_ct_data[final_ct_data$Dem_Prob >= 0.5,]
  temp_ct_data <- filter_ct_data(temp_ct_data, state = state, sector = sector)
  temp_ct_data <- temp_ct_data[temp_ct_data$Dem_Prob >= 0.5,]

  if (is.null(sector)){
    temp_colors <- blended_colors[which(names(blended_colors) %in% temp_ct_data$GEOID)]
    sector <- "All Sectors"
  }else{
    sector_colors <- color_lists[[substr(sector, 1, nchar(sector) - 5)]]
    temp_colors <-sector_colors[which(names(sector_colors) %in% temp_ct_data$GEOID)]
    temp_colors <- lapply(temp_colors, convert_to_hex)
  }

  if (is.null(state)){
    name <- "All"
  } else{
    name <- temp_ct_data$Name[1]
  }

  temp_map <- ggplot() +
    geom_sf(data = temp_counties)  +
    geom_sf(data = temp_census_tracts) +
    geom_sf(data = temp_ct_data, aes(fill = temp_colors), alpha = 0.5) +
    labs(title = name, subtitle = sector) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(b = 0)),
      plot.subtitle = element_text(hjust = 0.5, margin = margin(t = 0)),
      plot.margin = margin(0, 0, 0, 0)
    )

  return(temp_map)
}

set.seed(123)

acs_vars <- list(
  dem_vars = c("white", "black", "am_in_ala_nat", "asian", "haw_pac", "other", "hisp_lat", "male_u18", "female_u18", "male_o18", "female_o18"),
  trans_edu_vars = c("same_house_less_than_hs", "same_house_hs", "moved_in_county_less_than_hs", "moved_in_county_hs", "moved_diff_county_less_than_hs", "moved_diff_county_hs", "moved_diff_state_less_than_hs", "moved_diff_state_hs"),
  trans_pov_vars = c("same_house_p1", "same_house_p2", "moved_in_county_p1", "moved_in_county_p2", "moved_diff_county_p1", "moved_diff_county_p2", "moved_diff_state_p1", "moved_diff_state_p2"),
  cost_vars = c("mortgage_high_cost", "no_mortgage_high_cost", "rent_high_cost"),
  qual_vars = c("all_incomplete_plumb", "all_incomplete_kitchen", "occ_incomplete_plumb", "occ_incomplete_kitchen"),
  hh_type_vars = c("owner_single", "owner_2to4", "owner_5plus", "owner_mobile", "owner_unconvent", "renter_single", "renter_2to4", "renter_5plus", "renter_mobile", "renter_unconvent"),
  waid_vars = c("hh_no_wage", "hh_no_other_income", "hh_no_investment_income", "hh_public_assistance", "hh_ssi", "hh_3plus_worker", "hh_worker_no_vehicle", "hh_no_vehicle", "gini_index"),
  emp_vars = c("ag_for_fish_hunt_mining", "construction", "manufacturing", "wholesale_trade", "retail_trade", "trans_warehouse_util", "information", "fin_re_insur", "prof_sci_mgmt_waste", "edu_health_social", "arts_rec_food", "othersvcs", "public_admin")
)

regions <- list(Mid_west = c(17, 18, 19, 20, 26, 27, 29, 31, 38,  39, 46, 55),
                North_east = c(09, 11, 23, 25, 33, 34, 36, 42, 44, 50),
                West = c(4, 6, 16, 30, 32, 35, 41, 49, 53, 56, 8),
                South =  c(01, 05, 21, 22, 28, 40, 47, 48, 10, 12, 13, 24, 37, 45, 51, 54)
                )







