# Load necessary libraries
libs <- c("tidycensus", "dplyr", "stargazer")

# install.packages(libs) # uncomment to install packages
lapply(libs, require, character.only=T)
rm("libs")


cen_key <- Sys.getenv("CENSUS_API_KEY")
if (cen_key == "") {
  stop("Census API key is not set. Please set the CENSUS_API_KEY environment variable.")
}
census_api_key(cen_key, install = TRUE)

# Read FIPS codes from CSV file
fips <- read.csv(file.path("data", "fips_codes.csv"), header = TRUE, fileEncoding = "UTF-8-BOM")

# Set the year for the ACS data
year <- 2021

# Load ACS variables for the specified year
acs_variables <- as.data.frame(load_variables(year, "acs5", cache = TRUE))

# Define a named vector of ACS variables and their codes
all_variables <- c(
  vacant_housing_units = "B25002_003",
  occupied_housing_units = "B25002_002",
  population = "B01003_001",
  white = "B01001A_001",
  black = "B01001B_001",
  am_in_ala_nat = "B01001C_001",
  asian = "B01001D_001",
  haw_pac = "B01001E_001",
  other = "B01001F_001",
  hisp_lat = "B01001I_001",
  male_u18 = "B05003_003",
  female_u18 = "B05003_014",
  male_o18 = "B05003_008",
  female_o18 = "B05003_019",
  same_house_less_than_hs = "B07009_008",
  same_house_hs = "B07009_009",
  moved_in_county_less_than_hs = "B07009_014",
  moved_in_county_hs = "B07009_015",
  moved_diff_county_less_than_hs = "B07009_020",
  moved_diff_county_hs = "B07009_021",
  moved_diff_state_less_than_hs = "B07009_026",
  moved_diff_state_hs = "B07009_027",
  same_house_p1 = "B07012_006",
  same_house_p2 = "B07012_007",
  moved_in_county_p1 = "B07012_010",
  moved_in_county_p2 = "B07012_011",
  moved_diff_county_p1 = "B07012_014",
  moved_diff_county_p2 = "B07012_015",
  moved_diff_state_p1 = "B07012_018",
  moved_diff_state_p2 = "B07012_019",
  mhc1 = "B25091_008",
  mhc2 = "B25091_009",
  mhc3 = "B25091_010",
  mhc4 = "B25091_011",
  mortgage_not_computed = "B25091_012",
  nmhc1 = "B25091_019",
  nmhc2 = "B25091_020",
  nmhc3 = "B25091_021",
  nmhc4 = "B25091_022",
  no_mortgage_not_computed = "B25091_023",
  rhc1 = "B25070_007",
  rhc2 = "B25070_008",
  rhc3 = "B25070_009",
  rhc4 = "B25070_010",
  rent_not_computed = "B25070_011",
  owner_pop = "B25008_002",
  renter_pop = "B25008_003",
  all_incomplete_plumb = "B25047_003",
  all_incomplete_kitchen = "B25051_003",
  occ_incomplete_plumb = "B25048_003",
  occ_incomplete_kitchen = "B25052_003",
  owner_single = "B25033_003",
  owner_2to4 = "B25033_004",
  owner_5plus = "B25033_005",
  owner_mobile = "B25033_006",
  owner_unconvent = "B25033_007",
  renter_single = "B25033_009",
  renter_2to4 = "B25033_010",
  renter_5plus = "B25033_011",
  renter_mobile = "B25033_012",
  renter_unconvent = "B25033_013",
  hh_count = "B25026_001",
  hh_no_wage = "B19052_003",
  hh_no_other_income = "B19060_003",
  hh_no_investment_income = "B19054_003",
  hh_public_assistance = "B19058_002",
  hh_ssi = "B19056_002",
  hh_no_workers = "B08202_002",
  hh_3plus_worker = "B08202_005",
  hh_worker_no_vehicle = "B08203_008",
  hh_no_vehicle = "B08201_002",
  gini_index = "B19083_001",
  ag_for_fish_hunt_mining = "C24070_002",
  construction = "C24070_003",
  manufacturing = "C24070_004",
  wholesale_trade = "C24070_005",
  retail_trade = "C24070_006",
  trans_warehouse_util = "C24070_007",
  information = "C24070_008",
  fin_re_insur = "C24070_009",
  prof_sci_mgmt_waste = "C24070_010",
  edu_health_social = "C24070_011",
  arts_rec_food = "C24070_012",
  othersvcs = "C24070_013",
  public_admin = "C24070_014"
)

# Create a table of ACS variables and their descriptions
variable_table <- acs_variables %>%
  filter(name %in% unname(all_variables)) %>%
  left_join(data.frame(col_name = names(all_variables), value = unname(all_variables)), by = c("name" = "value"))

# Fetch ACS data and prepare it for analysis
acs_data <- get_acs(
  geography = "tract",
  variables = all_variables,
  year = year,
  state = as.vector(fips$FIPS_Code),
  survey = "acs5",
  output = "wide"
) %>%
  select(-ends_with("M")) %>%
  rename(location = NAME) %>%
  rename_with(~ gsub("E$", "", .), everything()) %>%
  mutate(GEOID = as.character(GEOID))

write.csv(acs_data, file = file.path("data", "ct_data.csv"), row.names = FALSE)
