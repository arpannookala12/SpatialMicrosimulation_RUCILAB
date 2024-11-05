# Loading required libraries for data processing & handling
library(dplyr)
library(tidyr)
library(tidycensus)

# Define a function to download and process ACS Education data
process_edu_data <- function(labs, statename, countyname) {
  # Set Census API key
  census_api_key(Sys.getenv("CENSUS_API_KEY"))

  # Define the variables to download from the B15003 table
  B15003_vars <- c(
    "B15003_002E", "B15003_003E", "B15003_004E", "B15003_005E", "B15003_006E", 
    "B15003_007E", "B15003_008E", "B15003_009E", "B15003_010E", "B15003_011E", 
    "B15003_012E", "B15003_013E", "B15003_014E", "B15003_015E", "B15003_016E", 
    "B15003_017E", "B15003_018E", "B15003_019E", "B15003_020E", "B15003_021E", 
    "B15003_022E", "B15003_023E", "B15003_024E", "B15003_025E"
  )

  # Download ACS data for the specified county and state
  acs_B15003_data <- get_acs(
    geography = "block group",
    variables = B15003_vars,
    state = statename,
    county = countyname,
    year = 2022,
    survey = "acs5"
  )

  # Define education groups for aggregation
edu_groups <- list(
  "Less than high school" = c("B15003_002","B15003_003","B15003_004","B15003_005","B15003_006","B15003_007","B15003_008","B15003_009","B15003_010","B15003_011","B15003_012"),
  "Some high school" = c("B15003_013", "B15003_014", "B15003_015","B15003_016"),
  "High school graduate or equivalent (for example GED)" = c("B15003_017", "B15003_018"),
  "Some college, but degree not received or is in progress" = c("B15003_019", "B15003_020"),
  "Associate's degree (for example AA, AS)" = "B15003_021",
  "Bachelor's degree (for example BA, BS, AB)" = "B15003_022",
  "Graduate degree (for example master's, professional, doctorate)" = c("B15003_023", "B15003_024", "B15003_025")
)

  # Filter education groups based on input labels
  new_edu_groups <- edu_groups[names(edu_groups) %in% labs]
  print(new_edu_groups)
  # Function to aggregate data based on education groups
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
  aggregated_edu_data <- aggregate_edu_data(acs_B15003_data, new_edu_groups)

  # Pivot the data to make `variable` categories into columns
  pivoted_data_edu <- aggregated_edu_data %>%
    select(-moe) %>% # Remove the `moe` column
    pivot_wider(names_from = variable, values_from = estimate)

  # Find row sums and identify rows where sums are 0
  row_sums <- rowSums(pivoted_data_edu[,-c(1,2)])
  zero_row_indices <- which(row_sums == 0)
  print(zero_row_indices)
  print(length(zero_row_indices))
  # Remove rows where the row sums are 0 and apply Haldane-Anscombe correction
  con_edu <- pivoted_data_edu[-zero_row_indices, -c(2)]
  con_edu[] <- lapply(con_edu, function(column) {
  if (is.numeric(column)) {
  return(column + 0.5)
  } else {
  return(column)
  }
  })
  print(con_edu)

  # Return the processed education data
  return(list(con_edu = con_edu, zero_row_indices=zero_row_indices))
}
