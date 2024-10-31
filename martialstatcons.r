# Loading required libraries for data processing & handling
library(dplyr)
library(tidyr)
library(tidycensus)
library(ggplot2)
library(lubridate)

# Downloading Marital Status constraint data
# Set your Census API key
census_api_key(Sys.getenv("CENSUS_API_KEY"))
# Define the variables to download from the B19037 table
# B19037_001E represents total households, followed by specific income brackets across all age groups
b12001_vars <- paste0("B12001_", sprintf("%03dE", 1:19))

# Download ACS data for Middlesex County for the B19037 table
acs_b12001_data <- get_acs(
  geography = "block group",
  variables = b12001_vars,
  state = "NJ",
  county = "Middlesex",
  year = 2022,
  survey = "acs5"
)
acs_b12001_data <- acs_b12001_data %>%
  mutate(estimate = estimate + 0.5)
acs_b12001_data


marriage_groups <- list(
  "Now Married" = c("B12001_005","B12001_014"),
  "Widowed" = c("B12001_009","B12001_018"),
  "Divorced" = c("B12001_010","B12001_019"),
  "Seperated" = c("B12001_007","B12001_016","B12001_008","B12001_017"),
  "Never Married" = c("B12001_003","B12001_012")
)


# Function to aggregate data based on race groups
aggregate_marriage_data <- function(data, groups) {
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
aggregated_marriage_data <- aggregate_marriage_data(acs_b12001_data, marriage_groups)

# Display the aggregated data
print(aggregated_marriage_data)

# Save the aggregated data to a new CSV file
# write.csv(aggregated_income_data, "aggregated_B19037_data_Phase4.1(May28-Jun24)2024.csv", row.names = FALSE)
# Pivot the data to make `variable` categories into columns
data_marriage <- aggregated_marriage_data
pivoted_data_marriage <- data_marriage %>%
  select(-moe) %>% # Remove the `moe` column
  pivot_wider(names_from = variable, values_from = estimate)
pivoted_data_income
# write.csv(pivoted_data_work,"pivoted_data_work_Phase4.1(May28-Jun24)2024.csv",row.names = FALSE)
con_marriage <- pivoted_data_marriage[-c(140,143,144,233,482,490,524,558,561,574),-c(1,2)]
# con_work <- pivoted_data_work[-c(194,988,994,1070,1096,1175,1238,1285,1287,1288,1291,1292,1339,1373,1380,1502,1597,1599,1616,1627, 1636,1639,1647,1648,1658,1779,1882,1931,1934,2554,2573,2574,2576,2588,2628,2632,2641,2685,2765,2766,2874,3234,3345,3346,3352,3353,3396,3538,3551,3552,3555,3730,3733,3734,3823,4072,4080,4114,4148,4151,4164,4331,4640,5160,5168,5217,5238,5280, 5284,5289,5319,5336,5362,5363,5364,5789),-c(1,2)]
con_marriage
write.csv(con_marriage,"con_marraige.csv",row.names = FALSE)