source("Housing_Insecurity_Functions.R")

library(xtable)
libs <- c("GGally", "corrplot", "moments", "stats", "purrr", "readr", "kableExtra", "dplyr")
lapply(libs, require, character.only = T)
remove(libs)

acs_vars <- add_acs_clusters(acs_vars)
#_____________________________________________________________________________________________________________________
# read in all data
#_____________________________________________________________________________________________________________________
df <- read.csv(file.path("final_data", "processed_ct_data.csv"), header = TRUE, fileEncoding = "UTF-8-BOM")
df$GEOID <- as.character(df$GEOID)
df <- df %>%
  mutate(GEOID = ifelse(substr(GEOID, 1, 1) == "1", paste0("0", GEOID), GEOID))
nat_df <- df[,3:ncol(df)]

fips <- read.csv(file.path("key_r_files/data", "fips_codes.csv"), header = TRUE, fileEncoding = "UTF-8-BOM")

df$X <- fips$Name[match(df$STATEFP, fips$FIPS_Code)]
colnames(df)[1] <- "State"
rm(fips)

var_imp <- read.csv(file.path("final_data", "variable_importance.csv"), header = TRUE, fileEncoding = "UTF-8-BOM") %>%
  select(-X)

# temp <- df[substr(df$GEOID,1,2) == "32"]
#____________________________________________________________________________________________________________________
# calculate cluster means and medians
#_____________________________________________________________________________________________________________________

###  EMP ###
median.emp <- summarise_by_variable(var_imp[var_imp$Sector == "emp",], nat_df[, acs_vars$emp_vars], "median")
row.names(median.emp) <- median.emp$variable
median.emp <- median.emp %>% select(-importance, -accuracy, -variable)
median.emp <- median.emp * 100
latex_table <- xtable(round(median.emp, digits = 4))
print(latex_table, include.rownames = TRUE)


# mean.emp <- summarise_by_variable(var_imp[var_imp$Sector == "emp",], nat_df[, acs_vars$emp_vars], "average")
# print_tree_stats(median.emp, mean.emp, sector = "Employment")

### DEM ###
median.dem <- summarise_by_variable(var_imp[var_imp$Sector == "dem",], nat_df[, acs_vars$dem_vars], "median")
row.names(median.dem) <- median.dem$variable
median.dem <- median.dem %>% select(-importance, -accuracy, -variable)
median.dem <- median.dem * 100
latex_table <- xtable(round(median.dem, digits = 4))
print(latex_table, include.rownames = TRUE)

# median.dem <- summarise_by_variable(var_imp[var_imp$Sector == "dem",], nat_df[, acs_vars$dem_vars], "median")
# mean.dem <- summarise_by_variable(var_imp[var_imp$Sector == "dem",], nat_df[, acs_vars$dem_vars], "average")
# print_tree_stats(median.dem, mean.dem, sector = "Demographics")

### COST  ###
median.cost <- summarise_by_variable(var_imp[var_imp$Sector == "cost",], nat_df[, acs_vars$cost_vars], "median")
row.names(median.cost) <- median.cost$variable
median.cost <- median.cost %>% select(-importance, -accuracy, -variable)
median.cost <- median.cost * 100
latex_table <- xtable(round(median.cost, digits = 4))
print(latex_table, include.rownames = TRUE)

# median.cost <- summarise_by_variable(var_imp[var_imp$Sector == "cost",], nat_df[, acs_vars$cost_vars], "median")
# mean.cost <- summarise_by_variable(var_imp[var_imp$Sector == "cost",], nat_df[, acs_vars$cost_vars], "average")
# print_tree_stats(median.cost, mean.cost, sector = "Housing cost")

###  QUAL ###
median.qual <- summarise_by_variable(var_imp[var_imp$Sector == "qual",], nat_df[, acs_vars$qual_vars], "median")
row.names(median.qual) <- median.qual$variable
median.qual <- median.qual %>% select(-importance, -accuracy, -variable)
median.qual <- median.qual * 100
latex_table <- xtable(round(median.qual, digits = 4))
print(latex_table, include.rownames = TRUE)

# mean.qual <- summarise_by_variable(var_imp[var_imp$Sector == "qual",], nat_df[, acs_vars$qual_vars], "average")
# print_tree_stats(median.qual, mean.qual, sector = "Housing Quality")

### TRANS.EDU ###
median.trans.edu <- summarise_by_variable(var_imp[var_imp$Sector == "trans.edu",], nat_df[, acs_vars$trans_edu_vars], "median")
row.names(median.trans.edu) <- median.trans.edu$variable
median.trans.edu <- median.trans.edu %>% select(-importance, -accuracy, -variable)
median.trans.edu <- median.trans.edu * 100
latex_table <- xtable(round(median.trans.edu, digits = 4))
print(latex_table, include.rownames = TRUE)

# mean.trans.edu <- summarise_by_variable(var_imp[var_imp$Sector == "trans.edu",], nat_df[, acs_vars$trans_edu_vars], "average")
# print_tree_stats(median.trans.edu, mean.trans.edu, sector = "Transience - Education")

### TRANS.POV ###
median.trans.pov <- summarise_by_variable(var_imp[var_imp$Sector == "trans.pov",], nat_df[, acs_vars$trans_pov_vars], "median")
row.names(median.trans.pov) <- median.trans.pov$variable
median.trans.pov <- median.trans.pov %>% select(-importance, -accuracy, -variable)
median.trans.pov <- median.trans.pov * 100
latex_table <- xtable(round(median.trans.pov, digits = 4))
print(latex_table, include.rownames = TRUE)
# mean.trans.pov <- summarise_by_variable(var_imp[var_imp$Sector == "trans.pov",], nat_df[, acs_vars$trans_pov_vars], "average")
# print_tree_stats(median.trans.pov, mean.trans.pov, sector = "Transience - Poverty")

### WAID ###
median.waid <- summarise_by_variable(var_imp[var_imp$Sector == "waid",], nat_df[, acs_vars$waid_vars], "median")
row.names(median.waid) <- median.waid$variable
median.waid <- median.waid %>% select(-importance, -accuracy, -variable)
median.waid <- median.waid * 100
latex_table <- xtable(round(median.waid, digits = 4))
print(latex_table, include.rownames = TRUE)
# mean.waid <- summarise_by_variable(var_imp[var_imp$Sector == "waid",], nat_df[, acs_vars$waid_vars], "average")
# print_tree_stats(median.waid, mean.waid, sector = "Household Wage/Aid")

### HHTYPE ###
median.hhtype <- summarise_by_variable(var_imp[var_imp$Sector == "hhtype",], nat_df[, acs_vars$hh_type_vars], "median")
row.names(median.hhtype) <- median.hhtype$variable
median.hhtype <- median.hhtype %>% select(-importance, -accuracy, -variable)
median.hhtype <- median.hhtype * 100
latex_table <- xtable(round(median.hhtype, digits = 4))
print(latex_table, include.rownames = TRUE)
# mean.hhtype <- summarise_by_variable(var_imp[var_imp$Sector == "hhtype",], nat_df[, acs_vars$hh_type_vars], "average")
# print_tree_stats(median.hhtype, mean.hhtype, sector = "Housing Type")

# all_objects <- ls()
# all_objects <- all_objects[grep("mean|median", all_objects)]
# if (length(all_objects > 0)){
#   rm(list = all_objects)
#   rm(all_objects)
# }

#____________________________________________________________________________________________________________________
# relabel clusters
#_____________________________________________________________________________________________________________________
# df$Emp_Cluster <- ifelse(df$Emp_Cluster == 1, 3, ifelse(df$Emp_Cluster == 2, 1, ifelse(df$Emp_Cluster == 3,2,0)))
# df$Dem_Cluster <- ifelse(df$Dem_Cluster == 1, 2, ifelse(df$Dem_Cluster == 2, 3,  ifelse(df$Dem_Cluster == 3, 1, 0)))
# df$Cost_Cluster <- ifelse(df$Cost_Cluster == 1, 2, ifelse(df$Cost_Cluster == 2, 3, ifelse(df$Cost_Cluster == 3, 1, 0)))
# df$Qual_Cluster <- ifelse(df$Qual_Cluster == 1, 3, ifelse(df$Qual_Cluster == 2, 2, ifelse(df$Qual_Cluster == 3, 1, 0)))
# df$Trans_EDU_Cluster <- ifelse(df$Trans_EDU_Cluster == 1, 3, ifelse(df$Trans_EDU_Cluster == 2, 2, ifelse(df$Trans_EDU_Cluster == 3, 1, 0)))
# df$Trans_POV_Cluster <- ifelse(df$Trans_POV_Cluster == 1, 3, ifelse(df$Trans_POV_Cluster == 2, 2, ifelse(df$Trans_POV_Cluster == 3, 1, 0)))
# df$Waid_Cluster <- ifelse(df$Waid_Cluster == 1, 3, ifelse(df$Waid_Cluster == 2, 2, ifelse(df$Waid_Cluster == 3, 1, 0)))
# df$Hhtype_Cluster <- ifelse(df$Hhtype_Cluster == 1, 3, ifelse(df$Hhtype_Cluster == 2, 2, ifelse(df$Hhtype_Cluster == 3, 1, 0)))
# #
# write.csv(df, file.path("final_data", "relabeled_ct_data.csv"))