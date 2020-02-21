#' Function to generate a custom colour palette for particulate matter plots. 
#' 
#' @author Stuart K. Grange
#' 
#' @param format What type of object should be returned?  
#' 
#' @return Named vector, named colour palette, or a tibble. 
#' 
#' @examples 
#' 
#' colours_pmcharacterr()
#' colours_pmcharacterr(format = "palette")
#' colours_pmcharacterr(format = "tibble")
#'
#' @export 
colours_pmcharacterr <- function(format = c("character", "palette", "tibble", 
                                            "data.frame", "df")) {
  
  # Check input
  stopifnot(format %in% c("character", "palette", "tibble", "data.frame", "df"))
  
  # The colours
  x <- dplyr::tribble(
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
  
  if (format[1] == "character") {
    x <- tibble::deframe(x)
  } else if (format[1] == "palette") {
    x <- x %>% 
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
