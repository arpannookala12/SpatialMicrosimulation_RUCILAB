# Loading required libraries for data processing & handling
library(dplyr)
library(tidyr)
library(tidycensus)
library(ggplot2)
library(lubridate)

# Downloading Education constraint data
# Set your Census API key
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Define the variables to download from the B15003 table
B15003_vars <- c(
  "B15003_002E","B15003_003E", "B15003_004E", "B15003_005E", "B15003_006E", "B15003_007E", "B15003_008E", "B15003_009E", "B15003_010E","B15003_011E", "B15003_012E", "B15003_013E","B15003_014E","B15003_015E", "B15003_016E","B15003_017E","B15003_018E","B15003_019E", "B15003_020E","B15003_021E", "B15003_022E", "B15003_023E", "B15003_024E","B15003_025E")

# Download ACS data for Middlesex County for the B01001 table
acs_B15003_data <- get_acs(
  geography = "block group",
  variables = B15003_vars,
  state = "NJ",
  county = "Middlesex",
  year = 2022,
  survey = "acs5"
)
# Haldane-Anscombe Correction factor applied to each cell
# acs_B15003_data <- acs_B15003_data %>%
#   mutate(estimate = estimate + 0.5)
acs_B15003_data
# Save the data to a CSV file (optional)
# write.csv(acs_B15003_data, "acs_B15003_data_Phase4.1(May28-Jun24)2024.csv")
# for(i in 1:nrow(acs_B15003_data)){
#   if (acs_B15003_data[i,'variable']=='B01001_002'){
#     acs_B15003_data[i,'estimate'] <- acs_B15003_data[i,'estimate'] -(acs_B15003_data[i+1,'estimate']+acs_B15003_data[i+2,'estimate']+acs_B15003_data[i+3,'estimate']+acs_B15003_data[i+4,'estimate'])
#   }
#   if (acs_B15003_data[i,'variable']=='B01001_019'){
#     acs_B15003_data[i,'estimate'] <- acs_B15003_data[i,'estimate'] -(acs_B15003_data[i+1,'estimate']+acs_B15003_data[i+2,'estimate']+acs_B15003_data[i+3,'estimate']+acs_B15003_data[i+4,'estimate'])
#   }
# }
# Display the first few rows of the data
# acs_b01001_data
acs_B15003_data
edu_groups <- list(
  "Less than high school" = c("B15003_002","B15003_003","B15003_004","B15003_005","B15003_006","B15003_007","B15003_008","B15003_009","B15003_010","B15003_011","B15003_012"),
  "Some high school" = c("B15003_013", "B15003_014", "B15003_015","B15003_016"),
  "High school graduate or equivalent (for example GED)" = c("B15003_017", "B15003_018"),
  "Some college, but degree not received or is in progress" = c("B15003_019", "B15003_020"),
  "Associate’s degree (for example AA, AS)" = "B15003_021",
  "Bachelor's degree (for example BA, BS, AB)" = "B15003_022",
  "Graduate degree (for example master's, professional, doctorate)" = c("B15003_023", "B15003_024", "B15003_025")
)

# Function to aggregate data based on race groups
aggregate_edu_data <- function(data, groups) {
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
aggregated_edu_data <- aggregate_edu_data(acs_B15003_data, edu_groups)


# Display the aggregated data
print(aggregated_edu_data)

# Save the aggregated data to a new CSV file
# write.csv(aggregated_edu_data, "aggregated_B15003_data_Phase4.1(May28-Jun24)2024.csv", row.names = FALSE)
# Pivot the data to make `variable` categories into columns
data_edu <- aggregated_edu_data
pivoted_data_edu <- data_edu %>%
  select(-moe) %>% # Remove the `moe` column
  pivot_wider(names_from = variable, values_from = estimate)
pivoted_data_edu
row_sums <- rowSums(pivoted_data_edu[,-c(1,2)])
# row_sums
# Find row numbers where row sums are 0
zero_row_indices <- which(row_sums == 0)
print(zero_row_indices)
con_edu <- pivoted_data_edu[-zero_row_indices,-c(1,2)]
# write.csv(pivoted_data_edu,"pivoted_data_edu_Phase4.1(May28-Jun24)2024.csv",row.names = FALSE)
# con_edu <- pivoted_data_edu[-c(140,143,144,233,482,490,524,558,561,574),-c(1,2)]
# con_edu <- pivoted_data_edu[-c(194,988,994,1070,1096,1175,1238,1285,1287,1288,1291,1292,1339,1373,1380,1502,1597,1599,1616,1627, 1636,1639,1647,1648,1658,1779,1882,1931,1934,2554,2573,2574,2576,2588,2628,2632,2641,2685,2765,2766,2874,3234,3345,3346,3352,3353,3396,3538,3551,3552,3555,3730,3733,3734,3823,4072,4080,4114,4148,4151,4164,4331,4640,5160,5168,5217,5238,5280, 5284,5289,5319,5336,5362,5363,5364,5789),-c(1,2)]
con_edu
con_edu <- con_edu + 0.5
print(con_edu)
write.csv(con_edu,"con_edu.csv",row.names = FALSE)