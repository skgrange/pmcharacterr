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
    "organic_matter",   "#20b848", "Organic matter",   "OM",                 "white",     
    "nitrate",          "#2471ee", "Nitrate",          "NO[3] * '-'",        "white",     
    "ammonium",         "#f8d568", "Ammonium",         "NH[4] * '+'",        "black",     
    "sulfate",          "#e6252c", "Sulfate",          "SO[4] ^{2 * '-'}",   "white",     
    "chloride",         "#b82090", "Chloride",         "Cl ^ {'-'}",         "white",     
    "mineral_dust",     "#dd7e28", "Mineral dust",     "Mineral~dust",       "black",     
    "trace_elements",   "#7dcce8", "Trace elements",   "Trace~elements",     "black",
    "silicon",          "#b82090", "Silicon",          "Si",                 "black",
    "missing",          "#cfcfcf", "Missing mass",     "Unknown",            "black",
    "unidentified",     "#cfcfcf", "Unidentified",     "Unidentified",       "black"
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
