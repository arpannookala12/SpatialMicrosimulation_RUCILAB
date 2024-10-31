# Loading required libraries for data processing & handling
library(dplyr)
library(tidyr)
library(tidycensus)
library(ggplot2)
library(lubridate)

# Define a function to download and process ACS Age & Sex data
process_acs_age_sex_data <- function(brks, labs, statename, countyname) {
  # Set Census API key
  census_api_key(Sys.getenv("CENSUS_API_KEY"))

  # Define the variables to download from the B01001 table
  b01001_vars <- c(
    "B01001_001E", "B01001_002E", "B01001_003E", "B01001_004E", "B01001_005E", 
    "B01001_006E", "B01001_007E", "B01001_008E", "B01001_009E", "B01001_010E",
    "B01001_011E", "B01001_012E", "B01001_013E", "B01001_014E", "B01001_015E",
    "B01001_016E", "B01001_017E", "B01001_018E", "B01001_019E", "B01001_020E",
    "B01001_021E", "B01001_022E", "B01001_023E", "B01001_024E", "B01001_025E",
    "B01001_026E", "B01001_027E", "B01001_028E", "B01001_029E", "B01001_030E",
    "B01001_031E", "B01001_032E", "B01001_033E", "B01001_034E", "B01001_035E",
    "B01001_036E", "B01001_037E", "B01001_038E", "B01001_039E", "B01001_040E",
    "B01001_041E", "B01001_042E", "B01001_043E", "B01001_044E", "B01001_045E",
    "B01001_046E", "B01001_047E", "B01001_048E", "B01001_049E"
  )

  # Download ACS data for the specified county and state
  acs_b01001_data <- get_acs(
    geography = "block group",
    variables = b01001_vars,
    state = statename,
    county = countyname,
    year = 2022,
    survey = "acs5"
  )

  # Perform the adjustments on the data
  mapping_age_lab <- list(
    "Under 5 years" = 1,
    "5 to 9 years" = 2,
    "10 to 14 years" = 3,
    "15 to 17 years" = 4,
    "18 and 19 years" = 5,
    "20 years" = 6,
    "21 years" = 7,
    "22 to 24 years" = 8,
    "25 to 29 years" = 9,
    "30 to 34 years" = 10,
    "35 to 39 years" = 11,
    "40 to 44 years" = 12,
    "45 to 49 years" = 13,
    "50 to 54 years" = 14,
    "55 to 59 years" = 15,
    "60 and 61 years" = 16,
    "62 to 64 years" = 17,
    "65 and 66 years" = 18,
    "67 to 69 years" = 19,
    "70 to 74 years" = 20,
    "75 to 79 years" = 21,
    "80 to 84 years" = 22,
    "85 years and over" = 23
  )

  # Identify missing labels from the input labs and create a new list
  missing_labs <- setdiff(names(mapping_age_lab), labs)
  extra_indices <- as.numeric(unlist(mapping_age_lab[missing_labs]))

  for(i in 1:nrow(acs_b01001_data)) {
    if (acs_b01001_data[i, 'variable'] == 'B01001_002' || acs_b01001_data[i, 'variable'] == 'B01001_026') {
      adjustment <- 0
      for (x in extra_indices) {
        index <- i + x
        if (index <= nrow(acs_b01001_data)) {
          adjustment <- adjustment + acs_b01001_data[index, 'estimate']
        }
      }
      acs_b01001_data[i, 'estimate'] <- acs_b01001_data[i, 'estimate'] - adjustment
    }
  }

  # Define age groups for aggregation
  age_groups <- list(
    "Male" = "B01001_002",
    "Female" = "B01001_026",
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

  # Create a new mapping based on the input labs that are present in the age_groups list
  new_age_groups <- age_groups[names(age_groups) %in% labs]
  print(new_age_groups)
  # Function to aggregate data based on age groups
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
  aggregated_age_data <- aggregate_age_data(acs_b01001_data, new_age_groups)
  print(aggregated_age_data)
  # Pivot the data to make `variable` categories into columns
  pivoted_data <- aggregated_age_data %>%
    select(-moe) %>% # Remove the `moe` column
    pivot_wider(names_from = variable, values_from = estimate)
  print(pivoted_data)
  # Find row sums and identify rows where sums are 0
  row_sums <- rowSums(pivoted_data[,-c(1,2)])
  zero_row_indices <- which(row_sums == 0)

  # Remove rows where the row sums are 0 and add Haldane-Anscombe correction
  con_age <- pivoted_data[-zero_row_indices, -c(1,2,3,4)] + 0.5
  con_sex <- pivoted_data[-zero_row_indices, c(3,4)] + 0.5
  write.csv(con_age,'con_age.csv')
  write.csv(con_sex,'con_sex.csv')
  # print(data.frame(con_age))
  # print(data.frame(con_sex))
  # Return results as a list
  # return(data.frame(con_age = con_age, con_sex = con_sex))
  return()
}

# Example usage of the function
# brks <- c(25, 30, 35, 40, 45, 50, 55, 60, 62, 65, 67, 70, 75, 80, 85, Inf)
# labs <- c(
#   "25 to 29 years", "30 to 34 years", "35 to 39 years", "40 to 44 years", "45 to 49 years",
#   "50 to 54 years", "55 to 59 years", "60 and 61 years", "62 to 64 years", "65 and 66 years",
#   "67 to 69 years", "70 to 74 years", "75 to 79 years", "80 to 84 years", "85 years and over"
# )
# statename <- "NJ"
# countyname <- "Middlesex"

# results <- process_acs_age_sex_data(brks, labs, statename, countyname)
# # print(results)
# con_age <- results$con_age
# con_sex <- results$con_sex
