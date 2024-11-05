# Loading required libraries for data processing & handling
library(dplyr)
library(tidyr)
library(tidycensus)

# Define a function to download and process ACS Kindwork data
process_work_data <- function(labs, statename, countyname) {
  # Set Census API key
  census_api_key(Sys.getenv("CENSUS_API_KEY"))

  # Define the variables to download from the B24080 table
  B24080_vars <- c(
    "B24080_004E", "B24080_005E", "B24080_006E", "B24080_007E", "B24080_008E", "B24080_009E", 
    "B24080_010E", "B24080_011E", "B24080_012E", "B24080_013E", "B24080_014E", "B24080_015E", 
    "B24080_016E", "B24080_017E", "B24080_018E", "B24080_019E", "B24080_020E", "B24080_021E"
  )

  # Download ACS data for the specified county and state
  acs_B24080_data <- get_acs(
    geography = "block group",
    variables = B24080_vars,
    state = statename,
    county = countyname,
    year = 2022,
    survey = "acs5"
  )


  # Define work groups for aggregation
  work_groups <- list(
    "Government" = c("B24080_007", "B24080_008", "B24080_009", "B24080_017", "B24080_018", "B24080_019"),
    "Private company" = c("B24080_004", "B24080_014"),
    "Non-profit organization including tax exempt and charitable organizations" = c("B24080_006", "B24080_016"),
    "Self-employed" = c("B24080_005", "B24080_010", "B24080_015", "B24080_020"),
    "Working in a family business" = c("B24080_011", "B24080_021")
  )
  # Filter education groups based on input labels
  new_work_groups <- work_groups[names(work_groups) %in% labs]
  print(new_work_groups)
  # Function to aggregate data based on work groups
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
  aggregated_work_data <- aggregate_work_data(acs_B24080_data, new_work_groups)

  # Pivot the data to make `variable` categories into columns
  pivoted_data_work <- aggregated_work_data %>%
    select(-moe) %>% # Remove the `moe` column
    pivot_wider(names_from = variable, values_from = estimate)

  # Find row sums and identify rows where sums are 0
  row_sums <- rowSums(pivoted_data_work[,-c(1,2)])
  zero_row_indices <- which(row_sums == 0)
  print(zero_row_indices)
  print(length(zero_row_indices))
  # Remove rows where the row sums are 0 and apply Haldane-Anscombe correction
  con_work <- pivoted_data_work[-zero_row_indices, -c(2)]
  con_work[] <- lapply(con_work, function(column) {
  if (is.numeric(column)) {
  return(column + 0.5)
  } else {
  return(column)
  }
  })
  # print(con_work)
  # Return the processed work data
  return(list(con_work = con_work, zero_row_indices = zero_row_indices))
}
