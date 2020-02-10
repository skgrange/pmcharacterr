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
      `attr<-`("name", "pmcharacterr")
    
    return(x)
    
  } else {
    
    return(df)
    
  }
  
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
