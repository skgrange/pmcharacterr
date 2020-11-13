#' Squash the global variable notes when building a package. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "variable", "value", "method_type", "date_analysis", "date_end",
    "colour", "variable_name", "unit", "unit_converted", "organic_carbon",
    "site_name"
  )
  
  # Squash the notes
  utils::globalVariables(variables)
  
}
