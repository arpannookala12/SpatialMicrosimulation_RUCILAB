# Loading required libraries for data processing & handling
library(dplyr)
library(tidyr)
library(tidycensus)
library(ggplot2)
library(lubridate)

# Downloading Age & Sex constraint data
# Setting Census API key
census_api_key("da537a3657a9c630b4efcd196c0dd4672be2bc5b")

# Define the variables to download from the B01001 table
b01001_vars <- c(
  "B01001_001E", "B01001_002E","B01001_003E","B01001_004E","B01001_005E", "B01001_006E","B01001_007E","B01001_008E","B01001_009E","B01001_010E","B01001_011E","B01001_012E","B01001_013E","B01001_014E","B01001_015E","B01001_016E","B01001_017E","B01001_018E","B01001_019E","B01001_020E","B01001_021E","B01001_022E","B01001_023E","B01001_024E","B01001_025E","B01001_026E","B01001_027E","B01001_028E","B01001_029E","B01001_030E","B01001_031E","B01001_032E","B01001_033E","B01001_034E","B01001_035E","B01001_036E","B01001_037E","B01001_038E","B01001_039E","B01001_040E","B01001_041E","B01001_042E","B01001_043E","B01001_044E","B01001_045E","B01001_046E","B01001_047E","B01001_048E","B01001_049E"
)

# Download ACS data for Middlesex County for the B01001 table
acs_b01001_data <- get_acs(
  geography = "block group",
  variables = b01001_vars,
  state = "NJ",
  county = "Middlesex",
  year = 2022,
  survey = "acs5"
)
acs_b01001_data
# Save the data to a CSV file (optional)
# write.csv(acs_b01001_data, "acs_b01001_data_Phase4.1(May28-Jun24)2024.csv")
for(i in 1:nrow(acs_b01001_data)){
  if (acs_b01001_data[i,'variable']=='B01001_002'){
    acs_b01001_data[i,'estimate'] <- acs_b01001_data[i,'estimate'] -(acs_b01001_data[i+1,'estimate']+acs_b01001_data[i+2,'estimate']+acs_b01001_data[i+3,'estimate']+acs_b01001_data[i+4,'estimate']+acs_b01001_data[i+5,'estimate']+acs_b01001_data[i+6,'estimate']+acs_b01001_data[i+7,'estimate']+acs_b01001_data[i+8,'estimate'])
  }
  if (acs_b01001_data[i,'variable']=='B01001_026'){
    acs_b01001_data[i,'estimate'] <- acs_b01001_data[i,'estimate'] -(acs_b01001_data[i+1,'estimate']+acs_b01001_data[i+2,'estimate']+acs_b01001_data[i+3,'estimate']+acs_b01001_data[i+4,'estimate']+acs_b01001_data[i+5,'estimate']+acs_b01001_data[i+6,'estimate']+acs_b01001_data[i+7,'estimate']+acs_b01001_data[i+8,'estimate'])
  }
}
# # Haldane-Anscombe Correction factor applied to each cell
acs_b01001_data <- acs_b01001_data %>%
  mutate(estimate = estimate+0.5)
acs_b01001_data
age_groups <- list(
  "Male" = "B01001_002",
  "Female" = "B01001_026",
  # "Under 5 years" = c("B01001_003", "B01001_027"),
  # "5 to 9 years" = c("B01001_004", "B01001_028"),
  # "10 to 14 years" = c("B01001_005", "B01001_029"),
  # "15 to 17 years" = c("B01001_006", "B01001_030"),
  # "18 and 19 years" = c("B01001_007", "B01001_031"),
  # "20 years" = c("B01001_008", "B01001_032"),
  # "21 years" = c("B01001_009", "B01001_033"),
  # "22 to 24 years" = c("B01001_010", "B01001_034"),
  "25 to 29 years" = c("B01001_011", "B01001_035"),
  "30 to 34 years" = c("B01001_012", "B01001_036"),
  "35 to 39 years" = c("B01001_013", "B01001_037"),
  "40 to 44 years" = c("B01001_014", "B01001_038"),
  "45 to 49 years" = c("B01001_015", "B01001_039"),
  "50 to 54 years" = c("B01001_016", "B01001_040"),
  "55 to 59 years" = c("B01001_017", "B01001_041"),
  "60 and 61 years" = c("B01001_018", "B01001_042"),
  "62 to 64 years" = c("B01001_019", "B01001_043"),
  "65 and 66 years" = c("B01001_020", "B01001_044"),
  "67 to 69 years" = c("B01001_021", "B01001_045"),
  "70 to 74 years" = c("B01001_022", "B01001_046"),
  "75 to 79 years" = c("B01001_023", "B01001_047"),
  "80 to 84 years" = c("B01001_024", "B01001_048"),
  "85 years and over" = c("B01001_025", "B01001_049")
)

# Function to aggregate data based on race groups
aggregate_age_data <- function(data, groups) {
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
aggregated_age_data <- aggregate_age_data(acs_b01001_data, age_groups)

# Display the aggregated data
print(aggregated_age_data)

# Save the aggregated data to a new CSV file
# write.csv(aggregated_age_data, "aggregated_b01001_data_Phase4.1(May28-Jun24)2024.csv", row.names = FALSE)
# Pivot the data to make `variable` categories into columns
data_age <- aggregated_age_data
pivoted_data <- data_age %>%
  select(-moe) %>% # Remove the `moe` column
  pivot_wider(names_from = variable, values_from = estimate)
pivoted_data
# write.csv(pivoted_data,"pivoted_data_Phase4.1(May28-Jun24)2024.csv",row.names = FALSE)
con_age <- pivoted_data[-c(140,143,144,233,482,490,524,558,561,574),-c(1,2,3,4)]
# con_age <- pivoted_data[-c(194,988,994,1070,1096,1175,1238,1285,1287,1288,1291,1292,1339,1373,1380,1502,1597,1599,1616,1627, 1636,1639,1647,1648,1658,1779,1882,1931,1934,2554,2573,2574,2576,2588,2628,2632,2641,2685,2765,2766,2874,3234,3345,3346,3352,3353,3396,3538,3551,3552,3555,3730,3733,3734,3823,4072,4080,4114,4148,4151,4164,4331,4640,5160,5168,5217,5238,5280, 5284,5289,5319,5336,5362,5363,5364,5789),-c(1,2,3,4)]
con_age
write.csv(con_age,"con_age.csv",row.names = FALSE)
con_sex <- pivoted_data[-c(140,143,144,233,482,490,524,558,561,574),c(3,4)]
# con_sex <- pivoted_data[-c(194,988,994,1070,1096,1175,1238,1285,1287,1288,1291,1292,1339,1373,1380,1502,1597,1599,1616,1627, 1636,1639,1647,1648,1658,1779,1882,1931,1934,2554,2573,2574,2576,2588,2628,2632,2641,2685,2765,2766,2874,3234,3345,3346,3352,3353,3396,3538,3551,3552,3555,3730,3733,3734,3823,4072,4080,4114,4148,4151,4164,4331,4640,5160,5168,5217,5238,5280, 5284,5289,5319,5336,5362,5363,5364,5789),c(3,4)]
con_sex
write.csv(con_sex,"con_sex.csv",row.names = FALSE)