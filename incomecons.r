# Loading required libraries for data processing & handling
library(dplyr)
library(tidyr)
library(tidycensus)
library(ggplot2)
library(lubridate)

# Downloading Income constraint data
# Set your Census API key
census_api_key(Sys.getenv("CENSUS_API_KEY"))
# Define the variables to download from the B19037 table
# B19037_001E represents total households, followed by specific income brackets across all age groups
b19037_vars <- paste0("B19037_", sprintf("%03dE", 1:69))

# Download ACS data for Middlesex County for the B19037 table
acs_b19037_data <- get_acs(
  geography = "block group",
  variables = b19037_vars,
  state = "NJ",
  county = "Middlesex",
  year = 2022,
  survey = "acs5"
)
acs_b19037_data <- acs_b19037_data %>%
  mutate(estimate = estimate + 0.5)
acs_b19037_data
acs_b19037_data

income_groups <- list(
  "Less than $25,000" = c("B19037_003","B19037_020","B19037_037","B19037_054","B19037_004","B19037_021","B19037_038","B19037_055","B19037_005","B19037_022","B19037_039","B19037_056","B19037_006","B19037_023","B19037_040","B19037_057"),
  "$25,000 - $34,999" = c("B19037_007","B19037_024","B19037_041","B19037_058","B19037_008","B19037_025","B19037_042","B19037_059"),
  "$35,000 - $49,999" = c("B19037_009","B19037_026","B19037_043","B19037_060","B19037_010","B19037_027","B19037_044","B19037_061","B19037_011","B19037_028","B19037_045","B19037_062"),
  "$50,000 - 74,999" = c("B19037_012","B19037_029","B19037_046","B19037_063","B19037_013","B19037_030","B19037_047","B19037_064"),
  "$75,000 - $99,999" = c("B19037_014","B19037_031","B19037_048","B19037_065"),
  "$100,000 - $149,999"= c("B19037_015","B19037_032","B19037_049","B19037_066","B19037_016","B19037_033","B19037_050","B19037_067"),
  "$150,000 - $199,999" = c("B19037_017","B19037_034","B19037_051","B19037_068"),
  "$200,000 and above" = c("B19037_018","B19037_035","B19037_052","B19037_069")
)


# Function to aggregate data based on race groups
aggregate_income_data <- function(data, groups) {
  aggregated_list <- lapply(names(groups), function(group) {
    vars <- groups[[group]]
    filtered_data <- data %>% filter(variable %in% vars)
    aggregated <- filtered_data %>%
      group_by(GEOID, NAME) %>%
      summarise(estimate = sum(estimate, na.rm = TRUE),
                moe = sqrt(sum((moe)^2, na.rm = TRUE))) %>%
      ungroup() %>%
      mutate(variable = group) %>%
      select(GEOID, NAME, variable, estimate, moe)
    return(aggregated)
  })
  aggregated_data <- bind_rows(aggregated_list)
  return(aggregated_data)
}

# Apply the aggregation function
aggregated_income_data <- aggregate_income_data(acs_b19037_data, income_groups)

# Display the aggregated data
print(aggregated_income_data)

# Save the aggregated data to a new CSV file
# write.csv(aggregated_income_data, "aggregated_B19037_data_Phase4.1(May28-Jun24)2024.csv", row.names = FALSE)
# Pivot the data to make `variable` categories into columns
data_income <- aggregated_income_data
pivoted_data_income <- data_income %>%
  select(-moe) %>% # Remove the `moe` column
  pivot_wider(names_from = variable, values_from = estimate)
pivoted_data_income
# write.csv(pivoted_data_work,"pivoted_data_work_Phase4.1(May28-Jun24)2024.csv",row.names = FALSE)
con_income <- pivoted_data_income[-c(140,143,144,233,482,490,524,558,561,574),-c(1,2)]
# con_work <- pivoted_data_work[-c(194,988,994,1070,1096,1175,1238,1285,1287,1288,1291,1292,1339,1373,1380,1502,1597,1599,1616,1627, 1636,1639,1647,1648,1658,1779,1882,1931,1934,2554,2573,2574,2576,2588,2628,2632,2641,2685,2765,2766,2874,3234,3345,3346,3352,3353,3396,3538,3551,3552,3555,3730,3733,3734,3823,4072,4080,4114,4148,4151,4164,4331,4640,5160,5168,5217,5238,5280, 5284,5289,5319,5336,5362,5363,5364,5789),-c(1,2)]
con_income
write.csv(con_income,"con_income.csv",row.names = FALSE)