#' Absorance spectrum of CRY2 pre-irradiated with blue ligh.
#'
#' This function gives a set of numeric multipliers that can be used
#' as a weight to calculate effective doses and irradiances. The
#' returned values are absorbance units.
#'
#' @usage CRY2_Abs_light_fun(w.length)
#' 
#' @param w.length numeric array of w.length (nm)
#'
#' @return a numeric array of the same length as \code{w.length} with 
#' values for the BSWF as presented in the original source.
#' @references \url{http://uv4growth.dyndns.org/}
#' @keywords misc
#' @export
#' @examples
#' CRY2_Abs_light_fun(309:800)
#' 
CRY2_Abs_light_fun <-
  function(w.length){
    CRY2.Absorbance <- numeric(length(w.length))
    CRY2.Absorbance[w.length <= 309] <- NA
    CRY2.Absorbance[w.length > 309 & w.length <= 690] <- 
      spline(CRY2.light.raw.data$w.length, CRY2.light.raw.data$Absorbance,
             xout=w.length[w.length > 309 & w.length <= 690])$y
    CRY2.Absorbance[w.length > 690] <- 0.0
    return(CRY2.Absorbance)
  }
