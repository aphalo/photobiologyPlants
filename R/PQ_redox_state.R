#' Coefficients for redox state of the plastoquinone pool
#'
#' The variables are as follows:
#'
#' \itemize{
#'   \item w.length (nm)
#'   \item k
#' }
#' where \code{k} contains fitted coefficients of a model for estimating the
#' redox state of the plastoquinone pool (Mattila et al., 2020). Based on
#' measurements in \emph{Arabidopsis} plants. If used in a publication, please,
#' cite both Mattila et al. (2020) and this R package.
#'
#' @docType data
#' @keywords datasets
#' @format \code{generic_spct} object with 351 rows and 2 variables.
#'
#' @seealso \code{\link{PQ_redox_state}()}
#'
"PS1_PS2_k.spct"

#' Redox state of the plastoquinone pool
#'
#' Estimate the redox state of the plastoquinone (PQ) pool from spectral
#' irradiance.
#'
#' @param light.spct A \code{source_spct} object or a \code{source_mspct}
#'   object.
#' @param coefs.spct A \code{generic_spct} object containing the coefficients of
#'   the fitted model. Defaults to those in Mattila et al. (2020).
#' @param w.band A \code{waveband} object setting the waveband used for scaling
#'   each the spectral irradiance(s) in \code{light.spct}. Defaults to PAR as
#'   used in Mattila et al. (2020).
#'
#' @details The computations follow the procedure in Mattila et al. (2020). The
#'   spectrum in \code{light.spct} is re-expressed by interpolation to the
#'   wavelengths from \code{coefs.spct}. As in Mattila et al. (2020) the state
#'   is expressed as percent of the PQ pool that is reduced. If the result of
#'   the computation is used in a publication, please, cite both Mattila et al.
#'   (2020) and this R package.
#'
#' @return A numeric vector, with one value for each spectrum in the input.
#'
#' @export
#'
#' @references
#' Mattila H, Khorobrykh S, Hakala-Yatkin M, Havurinne V, Kuusisto I, Antal T,
#' Tyystjärvi T, Tyystjärvi E. 2020. Action spectrum of the redox state of the
#' plastoquinone pool defines its function in plant acclimation. The Plant
#' Journal 104, 1088–1104. \doi{10.1111/tpj.14983}.
#'
#' @examples
#'
#' PQ_redox_state(sun.spct) # PAR is default for scaling
#' PQ_redox_state(sun.spct, w.band = c(370, 730)) # scaling with wavelength range
#' PQ_redox_state(white_led.source_spct)
#' PQ_redox_state(sun_evening.mspct)
#' PQ_redox_state(sun_evening.spct)
#'
PQ_redox_state <- function(light.spct,
                           coefs.spct = photobiologyPlants::PS1_PS2_k.spct,
                           w.band = photobiologyWavebands::PAR()) {
  if (is.source_spct(light.spct) && getMultipleWl(light.spct) > 1L) {
    light.spct <- subset2mspct(light.spct)
  }
  if (is.source_mspct(light.spct)) {
    photobiology::msaply(mspct = light.spct,
                         .fun = PQ_redox_state,
                         coefs.spct = coefs.spct,
                         w.band = w.band)
  } else {
      interpolated.spct <-
        photobiology::interpolate_spct(
          photobiology::fscale(light.spct,
                               f = photobiology::q_irrad,
                               w.band = w.band,
                               unit.out = "photon"),
          w.length.out = coefs.spct$w.length)
    sum(interpolated.spct$s.q.irrad * coefs.spct$k) + 50
  }
}
