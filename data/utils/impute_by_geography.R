# Import libraries
library(dplyr)
library(rlang)

# Define function to impute data by geography
impute_by_geography <- function(data, columns) {
  # Ensure columns is a character vector
  if (is.character(columns)) {
    column_list <- columns
  } else {
    column_list <- eval_tidy(enquo(columns))
  }
  
  for (column_name in column_list) {
    column_sym <- sym(column_name)
    impute_flag <- paste0("impute_flag_", column_name)
    
    data <- data %>%
      # Group by region and impute
      group_by(region) %>%
      mutate(
        !!impute_flag := ifelse(is.na(!!column_sym), "*", ""),
        !!column_sym := if_else(
          !!sym(impute_flag) == "*", median(!!column_sym, na.rm = TRUE), !!column_sym
        )
      ) %>%
      ungroup() %>%
      # Group by continent and impute
      group_by(continent) %>%
      mutate(
        !!impute_flag := ifelse(is.na(!!column_sym), "**", !!sym(impute_flag)),
        !!column_sym := if_else(
          !!sym(impute_flag) == "**", median(!!column_sym, na.rm = TRUE), !!column_sym
        )
      ) %>%
      ungroup()
  }
  
  return(data)
}

# # Example usage with a single column:
# df <- df %>%
#   impute_by_geography("per_capita")

# # Example usage with multiple columns:
# df <- df %>%
#   impute_by_geography(c("per_capita", "another_column"))
