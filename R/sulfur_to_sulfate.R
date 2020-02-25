#' Functions to convert elements to ions. 
#' 
#' @param x Numeric vector. 
#' 
#' @param s_mass,o_mass,n_mass Atomic weight of elements. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector. 
#' 
#' @examples 
#' 
#' sulfur_to_sulfate(0.28)
#' 
#' @export
sulfur_to_sulfate <- function(x, s_mass = 32.06, o_mass = 15.999) {
  x * (o_mass * 4 + s_mass) / s_mass
}


#' @rdname sulfur_to_sulfate
#' @export
nitrogen_to_nitrate <- function(x, n_mass = 14.007, o_mass = 15.999) {
  x * (o_mass * 3 + n_mass) / n_mass
}
