#' Diffusion coefficients in air
#'
#' Diffusion coefficients of water and carbon dioxide in excess air as a
#' function of temperature.
#'
#' @param temperature numeric Temperature [\eqn{^{\circ}C}], in the range -7
#'    to 47 \eqn{^{\circ}C}.
#'
#' @details
#' Implemented as interpolating splines from tabulated data.
#'
#' @return A numeric vector of the same length as \code{temperature} containing
#'    diffusion coefficients expressed in \eqn{m^2 s^{-1}}. \code{NA} is
#'   returned silently for off-range arguments passed to \code{temperature}.
#'
#' @references
#' Monteith, J. L. and Unsworth M. H. (2008) Principles of Environmental
#'   Physics (3 ed.) Academic Press-Elsevier. ISBN: 978-0-12-505103-3. See
#'   Table A.3.
#'
#' @examples
#' D_water(23)
#' D_water(c(10, 12, 15, 100))
#'
#' D_CO2(11)
#'
#' @export
#'
D_water <- function(temperature) {
  D_water.fun <-
    stats::splinefun(x = seq(from = -5, to = 45, by = 5),
                     y = c(20.5, 21.2, 22.0, 22.7, 23.4, 24.2, 24.9, 25.7,
                           26.4, 27.2, 28.0) * 1e-6,
                     method = "hyman")
  ifelse(temperature < -7 | temperature > 47,
         NA_real_,
         D_water.fun(temperature))
}

#' @rdname D_water
#'
#' @export
#'
D_CO2 <- function(temperature) {
  D_CO2.fun <-
    stats::splinefun(x = seq(from = -5, to = 45, by = 5),
                     y = c(12.4, 12.9, 13.3, 13.8, 14.2, 14.7, 15.1, 15.6,
                           16.0, 16.5, 17.0) * 1e-6,
                     method = "hyman")
  ifelse(temperature < -7 | temperature > 47,
         NA_real_,
         D_CO2.fun(temperature))
}
