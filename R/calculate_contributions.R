#' Function to calculate particulate matter relative contributions. 
#' 
#' @param df Input data frame. 
#' 
#' @param digits Number of decimal points to display in the 
#' \code{contribution_label} variable. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{calculate_missing_mass}}
#' 
#' @examples
#' 
#' # Example input
#' data_example <- tibble::tribble(
#'  ~variable,        ~value,
#'  "mass",           39.7,  
#'  "ec",             6.1,   
#'  "om",             9.2,   
#'  "nitrage",        3.3,   
#'  "ammonium",       1.4,   
#'  "sulphate",       3.3,   
#'  "mineral_dust",   2.7,   
#'  "trace_elements", 6.8,
#'  "missing",        6.9
#'  )
#'  
#' # Determine missing mass
#' calculate_contributions(data_example)
#' 
#' @export
calculate_contributions <- function(df, digits = 1) {
  
  # Check input
  stopifnot(all(c("variable", "value") %in% names(df)))
  
  # Get mass value
  value_mass <- df %>% 
    filter(variable == "mass") %>% 
    pull(value)
  
  stopifnot(length(value_mass) == 1)
  
  # Calculate contribtions and add a few extra things
  df <- mutate(df, contribution = value / !!value_mass)
  
  # Check for negative contributions and rais waring
  if (any(df$value < 0)) {
    warning("Negative contribitions detected...", call. = FALSE)
  }
  
  return(df)
  
}


#' Function to calculate particulate matter contribution labels.
#' 
#' @param x A numeric vector to format. 
#' 
#' @param digits Number of decimal points to display in the 
#' \code{contribution_label} variable. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector. 
#' 
#' @seealso \code{\link{calculate_missing_mass}}
#' 
#' @export
str_contribution_label <- function(x, digits = 1) {
  
  x %>% 
    round(digits = digits) %>% 
    stringr::str_c(" %")
  
}
