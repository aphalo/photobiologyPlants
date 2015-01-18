#' Definition of CRY2 weighted waveband
#' 
#' CRY2 absorbance spectrum of ...
#' 
#' @param norm normalization wavelength (nm), default is no normalization applied
#' @param previous a character string with values "darkness" or "ligh" (default)
#' @return a list defining the wavelength range, weighting and normalization
#' @usage CRY2.Abs(norm=NULL, previous="light")
#' @references
#' To be added
#' 
#' @export
#' @seealso \code{\link{new_waveband}}, \code{\link{photon_irradiance}} and \code{\link{energy_irradiance}}
#' @examples
#' CRY2.Abs()
#' CRY2.Abs(300)
#' CRY2.Abs(300, "light")
#' CRY2.Abs(300, "darkness")
#' CRY2.Abs(previous="light")
#' CRY2.Abs(previous="darkness")

CRY2.Abs <- function(norm=NULL, previous="light") {
  CRY2.Abs.fun <- switch(previous,
                     light = CRY2_Abs_light_fun,
                     darkness = CRY2_Abs_dark_fun)
  new_waveband(w.low=309.6, w.high=1000, 
               weight="SWF", SWF.q.fun=CRY2.Abs.fun, SWF.e.fun=CRY2.Abs.fun, SWF.norm=NULL,
               norm=norm, wb.name=paste("CRY2.Abs", previous, sep="."))
}