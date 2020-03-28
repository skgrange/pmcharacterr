#' Function to generate a custom colour palette for particulate matter plots. 
#' 
#' @author Stuart K. Grange
#' 
#' @param format What type of object should be returned?  
#' 
#' @param names When \code{format} is \code{character}, should the character 
#' vector be named? 
#' 
#' @return Named vector, vector, named colour palette, or a tibble. 
#' 
#' @examples 
#' 
#' colours_pmcharacterr()
#' colours_pmcharacterr(format = "palette")
#' colours_pmcharacterr(format = "tibble")
#'
#' @export 
colours_pmcharacterr <- function(format = c("character", "palette", "tibble", 
                                            "data.frame", "df"), names = TRUE) {
  
  # Check input
  stopifnot(format %in% c("character", "palette", "tibble", "data.frame", "df"))
  
  # The colours
  x <- dplyr::tribble(
    ~variable,          ~colour,   ~variable_name,     ~variable_expression, ~text_colour,
    "elemental_carbon", "#404040", "Elemental carbon", "EC",                 "white",     
    "organic_carbon",   "#20b848", "Organic matter",   "OM",                 "white",     
    "nitrate",          "#2471ee", "Nitrate",          "NO[3] * '-'",        "white",     
    "ammonium",         "#eea124", "Ammonium",         "NH[4] * '+'",        "white",     
    "sulfate",          "#e6252c", "Sulfate",          "SO[4] ^{2 * '-'}",   "white",     
    "chloride",         "#b82090", "Chloride",         "Cl ^ {'-'}",         "white",     
    "mineral_dust",     "#b84820", "Mineral dust",     "Mineral~dust",       "white",     
    "trace_elements",   "#2090b8", "Trace elements",   "Trace~elements",     "black",     
    "missing",          "#cfcfcf", "Missing mass",     "Unknown",            "black"
  )
  
  if (format[1] == "character") {
    
    x <- x %>% 
      select(variable_name,
             colour) %>% 
      tibble::deframe()
    
    # Drop names
    if (!names) x <- unname(x)
    
  } else if (format[1] == "palette") {
    
    x <- x %>% 
      select(variable_name,
             colour) %>% 
      tibble::deframe() %>% 
      "class<-"("palette") 
    
  } 
  
  return(x)

}


# https://github.com/karthik/wesanderson/blob/master/R/colors.R
#' print.palette <- function(x, ...) {
#'   n <- length(x)
#'   old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
#'   on.exit(par(old))
#'   
#'   image(1:n, 1, as.matrix(1:n), col = x,
#'         ylab = "", xaxt = "n", yaxt = "n", bty = "n")
#'   
#'   rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
#'   text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
#' }
