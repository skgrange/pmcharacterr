#' Function to select a collection of priority variables from a 
#' \code{`measurements`} \strong{pmcharacter} table. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Tibble from \code{\link{import_measurements}}. 
#' 
#' @return Tibble. 
#' 
#' @export
select_priority_variables <- function(df) {
  
  # What variables to get
  variables <- c(
    "field_campaign", "data_source", "particulate_fraction", "date", "date_end",
    "site", "site_name", "variable", "unit", "value"
  )
  
  # Select
  if (all(variables %in% names(df))) {
    df <- select(df, !!variables)
  } else {
    stop("Selected priority variables are not contained in `df`.", call. = FALSE)
  }
  
  return(df)
  
}
