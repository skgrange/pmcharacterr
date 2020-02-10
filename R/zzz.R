#' Squash the global variable notes when building a package. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    ".", "variable", "value", "method_type", "date_analysis", "date_end",
    "colour"
  )
  
  # Squash the notes
  utils::globalVariables(variables)
  
}