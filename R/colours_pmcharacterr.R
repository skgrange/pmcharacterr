#' Function to generate a custom colour palette for particulate matter plots. 
#' 
#' @author Stuart K. Grange
#' 
#' @param as_palette Should the return be of class \code{palette}? 
#' 
#' @return Colour palette or tibble. 
#' 
#' @examples 
#' 
#' colours_pmcharacterr(as_palette = TRUE)
#' colours_pmcharacterr(as_palette = FALSE)
#'
#' @export 
colours_pmcharacterr <- function(as_palette = TRUE) {
  
  # The colours
  df <- dplyr::tribble(
    ~variable,          ~colour,  
    "elemental_carbon", "#404040",
    "organic_carbon",   "#20b848",
    "nitrate",          "#2471ee",
    "ammonium",         "#eea124",
    "sulfate",          "#e6252c",
    "chloride",         "#b82090",
    "mineral_dust",     "#b84820",
    "trace_elements",   "#2090b8",
    "missing",          "#cfcfcf"
  )
  
  if (as_palette) {
    
    x <- df %>% 
      pull(colour) %>% 
      "class<-"("palette") %>% 
      setNames("colours_pmcharacterr")
    
    return(x)
    
  } else {
    
    return(df)
    
  }
  
}
