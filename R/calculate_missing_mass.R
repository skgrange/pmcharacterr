#' Function to calculate particulate matter missing mass. 
#' 
#' \code{\link{calculate_missing_mass}} will add one more observation/row to
#' the input data frame/tibble. 
#' 
#' @param df Input data frame. 
#' 
#' @param digits Number of digits for logic test.
#' 
#' @param add_method_type Should a \code{method_type} variable be added to the 
#' return? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{calculate_contributions}}
#' 
#' @examples
#' 
#' # Example input
#' data_example <- tibble::tribble(
#'  ~variable,        ~value,
#'  "mass",           39.7,  
#'  "elemental_carbon", 6.1,   
#'  "organic_matter", 9.2,   
#'  "nitrate",        3.3,   
#'  "ammonium",       1.4,   
#'  "sulfate",        3.3,   
#'  "mineral_dust",   2.7,   
#'  "trace_elements", 6.8
#'  )
#'  
#' # Determine missing mass
#' calculate_missing_mass(data_example)
#' calculate_missing_mass(data_example, add_method_type = FALSE)
#' 
#' @export
calculate_missing_mass <- function(df, digits = 6, add_method_type = TRUE) {
  
  # Check input
  stopifnot(all(c("variable", "value") %in% names(df)))
  
  if ("method_type" %in% names(df)) {
    df <- select(df, method_type, variable, value)
  } else {
    df <- df %>% 
      mutate(method_type = NA_character_) %>% 
      select(method_type,
             variable, 
             value)
  }
  
  # Get mass value
  value_mass <- df %>% 
    filter(variable == "mass") %>% 
    pull(value)
  
  # Calculate accounted-for mass
  known_mass <- df %>% 
    filter(variable != "mass") %>% 
    pull(value) %>% 
    sum(na.rm = TRUE)
  
  # Calculate missing mass
  if (signif(value_mass, digits = digits) == signif(known_mass, digits = digits)) {
    missing_mass <- 0
  } else {
    missing_mass <- value_mass - known_mass
  }
  
  # Build tibble
  df_missing <- tibble(
    method_type = "calculated", 
    variable = "missing",
    value = missing_mass
  )
  
  # Bind to input which has been selected
  df <- bind_rows(df, df_missing)
  
  # Drop the extra variable
  if (!add_method_type) {
    df <- select(df, -method_type)
  }
  
  return(df)
  
}
