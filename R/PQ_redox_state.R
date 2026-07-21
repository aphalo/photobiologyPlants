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
#' Estimate the redox state of the plastoquinone pool from spectral irradiance.
#'
#' @param light.spct A \code{source_spct} object.
#' @param coefs.spct A \code{generic_spct} object containg the coefficients of
#'   the fitted model. Defaults to those in Mattila et al. (2020).
#'
#' @details The computations follow the procedure in Mattila et al. (2020). The
#'   spectrum in \code{light.spct} is re-expressed by interpolation to the
#'   wavelengths from \code{coefs.spct}. If used in a publication, please, cite
#'   both Mattila et al. (2020) and this R package.
#'
#' @return A numeric value.
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
#' PQ_redox_state(sun.spct)
#' PQ_redox_state(white_led.source_spct)
#'
PQ_redox_state <- function(light.spct,
                           coefs.spct = photobiologyPlants::PS1_PS2_k.spct) {
  interpolated.spct <-
    photobiology::interpolate_spct(
      photobiology::fscale(light.spct,
                           f = photobiology::q_irrad,
                           w.band = photobiologyWavebands::PAR()),
      w.length.out = coefs.spct$w.length)
  sum(interpolated.spct$s.q.irrad * coefs.spct$k) + 50
}
