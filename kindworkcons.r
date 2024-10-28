# Loading required libraries for data processing & handling
library(dplyr)
library(tidyr)
library(tidycensus)
library(ggplot2)
library(lubridate)

# Downloading Kindwork constraint data
# Set your Census API key
census_api_key("da537a3657a9c630b4efcd196c0dd4672be2bc5b")

# Define the variables to download from the B15003 table
B24080_vars <- c(
"B24080_004E", "B24080_005E", "B24080_006E", "B24080_007E", "B24080_008E", "B24080_009E", "B24080_010E","B24080_011E", "B24080_012E", "B24080_013E","B24080_014E","B24080_015E", "B24080_016E","B24080_017E","B24080_018E","B24080_019E", "B24080_020E","B24080_021E")

# Download ACS data for Middlesex County for the B01001 table
acs_B24080_data <- get_acs(
  geography = "block group",
  variables = B24080_vars,
  state = "NJ",
  county = "Middlesex",
  year = 2022,
  survey = "acs5"
)
# Haldane-Anscombe Correction factor applied to each cell
acs_B24080_data <- acs_B24080_data %>%
  mutate(estimate = estimate+0.5)
acs_B24080_data
# Save the data to a CSV file (optional)
# write.csv(acs_B24080_data, "acs_B24080_Phase4.1(May28-Jun24)2024.csv")
# for(i in 1:nrow(acs_B24080_data)){
#   if (acs_B24080_data[i,'variable']=='B01001_002'){
#     acs_B24080_data[i,'estimate'] <- acs_B24080_data[i,'estimate'] -(acs_B24080_data[i+1,'estimate']+acs_B24080_data[i+2,'estimate']+acs_B24080_data[i+3,'estimate']+acs_B24080_data[i+4,'estimate'])
#   }
#   if (acs_B24080_data[i,'variable']=='B01001_019'){
#     acs_B24080_data[i,'estimate'] <- acs_B24080_data[i,'estimate'] -(acs_B24080_data[i+1,'estimate']+acs_B24080_data[i+2,'estimate']+acs_B24080_data[i+3,'estimate']+acs_B24080_data[i+4,'estimate'])
#   }
# }
# Display the first few rows of the data
# acs_b01001_data
acs_B24080_data
work_groups <- list(
  "Government" = c("B24080_007","B24080_008","B24080_009","B24080_017","B24080_018","B24080_019"),
  "Private company" = c("B24080_004","B24080_014"),
  "Non-profit organization including tax exempt and charitable organizations" = c("B24080_006","B24080_016"),
  "Self-employed" = c("B24080_005","B24080_010","B24080_015","B24080_020"),
  "Working in a family business" = c("B24080_0011","B24080_021")
)

# Function to aggregate data based on race groups
aggregate_work_data <- function(data, groups) {
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
aggregated_work_data <- aggregate_work_data(acs_B24080_data, work_groups)

# Display the aggregated data
print(aggregated_work_data)

# Save the aggregated data to a new CSV file
# write.csv(aggregated_work_data, "aggregated_B24080_data_Phase4.1(May28-Jun24)2024.csv", row.names = FALSE)
# Pivot the data to make `variable` categories into columns
data_work <- aggregated_work_data
pivoted_data_work <- data_work %>%
  select(-moe) %>% # Remove the `moe` column
  pivot_wider(names_from = variable, values_from = estimate)
pivoted_data_work
# write.csv(pivoted_data_work,"pivoted_data_work_Phase4.1(May28-Jun24)2024.csv",row.names = FALSE)
con_work <- pivoted_data_work[-c(140,143,144,233,482,490,524,558,561,574),-c(1,2)]
# con_work <- pivoted_data_work[-c(194,988,994,1070,1096,1175,1238,1285,1287,1288,1291,1292,1339,1373,1380,1502,1597,1599,1616,1627, 1636,1639,1647,1648,1658,1779,1882,1931,1934,2554,2573,2574,2576,2588,2628,2632,2641,2685,2765,2766,2874,3234,3345,3346,3352,3353,3396,3538,3551,3552,3555,3730,3733,3734,3823,4072,4080,4114,4148,4151,4164,4331,4640,5160,5168,5217,5238,5280, 5284,5289,5319,5336,5362,5363,5364,5789),-c(1,2)]
con_work
write.csv(con_work,"con_work.csv",row.names = FALSE)