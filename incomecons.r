# Loading required libraries for data processing & handling
library(dplyr)
library(tidyr)
library(tidycensus)

# Define a function to download and process ACS Income data
process_income_data <- function(labs,statename, countyname) {
  # Set Census API key
  census_api_key("da537a3657a9c630b4efcd196c0dd4672be2bc5b")

  # Define the variables to download from the B19037 table
  b19037_vars <- paste0("B19037_", sprintf("%03dE", 1:69))

  # Download ACS data for the specified county and state
  acs_b19037_data <- get_acs(
    geography = "block group",
    variables = b19037_vars,
    state = statename,
    county = countyname,
    year = 2022,
    survey = "acs5"
  )


  # Define income groups for aggregation
  income_groups <- list(
    "Less than $25,000" = c("B19037_003", "B19037_020", "B19037_037", "B19037_054", "B19037_004", "B19037_021", "B19037_038", "B19037_055", "B19037_005", "B19037_022", "B19037_039", "B19037_056", "B19037_006", "B19037_023", "B19037_040", "B19037_057"),
    "$25,000 - $34,999" = c("B19037_007", "B19037_024", "B19037_041", "B19037_058", "B19037_008", "B19037_025", "B19037_042", "B19037_059"),
    "$35,000 - $49,999" = c("B19037_009", "B19037_026", "B19037_043", "B19037_060", "B19037_010", "B19037_027", "B19037_044", "B19037_061", "B19037_011", "B19037_028", "B19037_045", "B19037_062"),
    "$50,000 - $74,999" = c("B19037_012", "B19037_029", "B19037_046", "B19037_063", "B19037_013", "B19037_030", "B19037_047", "B19037_064"),
    "$75,000 - $99,999" = c("B19037_014", "B19037_031", "B19037_048", "B19037_065"),
    "$100,000 - $149,999" = c("B19037_015", "B19037_032", "B19037_049", "B19037_066", "B19037_016", "B19037_033", "B19037_050", "B19037_067"),
    "$150,000 - $199,999" = c("B19037_017", "B19037_034", "B19037_051", "B19037_068"),
    "$200,000 and above" = c("B19037_018", "B19037_035", "B19037_052", "B19037_069")
  )

  # Filter education groups based on input labels
  new_income_groups <- income_groups[names(income_groups) %in% labs]
  print(new_income_groups)

  # Function to aggregate data based on income groups
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
  aggregated_income_data <- aggregate_income_data(acs_b19037_data, new_income_groups)

  # Pivot the data to make `variable` categories into columns
  pivoted_data_income <- aggregated_income_data %>%
    select(-moe) %>% # Remove the `moe` column
    pivot_wider(names_from = variable, values_from = estimate)

  # Find row sums and identify rows where sums are 0
  row_sums <- rowSums(pivoted_data_income[,-c(1,2)])
  zero_row_indices <- which(row_sums == 0)
  print(zero_row_indices)
  print(length(zero_row_indices))
  # Remove rows where the row sums are 0 and apply Haldane-Anscombe correction
  con_income <- pivoted_data_income[-zero_row_indices, -c(2)]
  con_income[] <- lapply(con_income, function(column) {
  if (is.numeric(column)) {
  return(column + 0.5)
  } else {
  return(column)
  }
  })

  # Return the processed income data
  return(list(con_income = con_income, zero_row_indices = zero_row_indices))
}
