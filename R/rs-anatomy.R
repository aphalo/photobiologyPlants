#' Stomatal conductance from sizes and density
#'
#' Computations based on mass transfer through pores.
#'
#' @param length,width numeric "Length" and "width" of the stomatal opening,
#'   i.e., the major and minor diameters of the ellipse describing the stomatal
#'   pore's mean cross section [\eqn{m}].
#' @param depth numeric The depth (or diffusion path length) of the stomatal
#'   pore [\eqn{m}].
#' @param num numeric Number of stomata per unit area [\eqn{mm^{-2}}].
#' @param D numeric Diffusion coefficient of the gas [\eqn{m^2\,s^{-1}}].
#'
#' @details The equation for \eqn{r_s} from Monteith and Unsworth (2008):
#'
#' \deqn{r_s = \frac{4(l + \pi\,d / 8)}{\pi\,n\,d^2\,D}}
#'
#' is used to compute diffusive resistance in function \code{rs_from_size()},
#' which can be used to obtain diffusive resistance from a known size of a
#' pore with circular or elliptical cross section. The value of \code{D}, the
#' diffusion should match that of water vapour or \eqn{CO_2}, and its unit
#' of expression determines the whether the returned value is expressed as the
#' inverse of a volume or molar flux rate.
#'
#' Functions \code{gs_from_size()}, \code{gs_w_from_size()} and
#' \code{gs_c_from_size()} are convenience wrappers.
#'
#' @note This is an approximate computation as plant stomata have a section that
#' varies with depth. The equation incorporates a single end correction and
#' assumes no interference among the flows from neighbouring stomata crossing
#' the bounday layer.
#'
#' @return A \code{numeric} vector of resistances expressed in \eqn{s\,m^{-1}}
#' or \eqn{s\,mol^{-1}}, or of conductances expressed in \eqn{m\,s^{-1}} or in
#' \eqn{mol\,m^{-2}\,s^{-1}} when the density expressed in estomata per \eqn{m^2}
#' is passed as argument to \code{n}. With the default of \code{n = 1} the
#' diffusive conductance per individual pore is returned expressed, e.g., for
#' molar conductance, in \eqn{mol\,s-1}.
#'
#' @references
#' Monteith, J. L. and Unsworth M. H. (2008) Principles of Environmental
#'   Physics (3ed) Academic Press-Elsevier. ISBN: 978-0-12-505103-3. See
#'   Section 11.4 Mass transfer through pores.
#'
#' @seealso Functions \code{\link{D_water}()} and \code{\link{D_CO2}()} can be
#' used to compute the diffusion coefficients as a function of temperature. In
#' addition function \code{\link{molar_vol}()} computes the molar volume of an
#' ideal gas as a function of temperature and pressure. Functions
#' \code{\link{gs_mol2vol}()} and \code{\link{gs_vol2mol}()} interconvert
#' conductances between molar and volume bases of expression. These functions
#' are used internally in the functions described here.
#'
#' @examples
#' # a single round stomatal pore
#' gs_from_size(length = 20e-6, depth = 5e-6, D = D_water(23))
#'
#' # a single elliptical stomatal pore
#' gs_from_size(length = 30e-6, width = 10e-6, depth = 5e-6, D = D_water(23))
#'
#' # 200 circular stomatal pores per mm^2
#' rs_from_size(length = 5e-6,
#'              width = 5e-6,
#'              depth = 10-6,
#'              num = 200e6,
#'              D = D_water(25))
#'
#' # 50 elliptical stomatal pores per mm^2
#' rs_from_size(length = 10-6,
#'              width = 5-6,
#'              depth = 20e-6,
#'              num = 50e6,
#'              D = D_water(23))
#'
#' # a single round stomatal pore with dimensions in micrometers
#' gs_from_size(length = 20, depth = 5, D = D_water(23))
#'
#' @export
#'
rs_from_size <- function(length, width = length, depth, num = 1, D) {
  if (all(c(length, width, depth) < 1e-3)) {
    k = 1
  } else if (all(c(length, width, depth) >= 1e-3)) {
    message("Assuming 'length', 'width' and 'depth' are expressed in micrometres (um)")
    k = 1e-6
  } else {
    stop("Dimensions of stomata are inconsistent, metres expected.")
  }
  diameter <- (length + width) / 2 * k
  depth <- depth * k
  4 * (depth + pi *  diameter / 8) / (pi * num * diameter^2 * D)
}

#' @rdname rs_from_size
#'
#' @export
gs_from_size <- function(length, width = length, depth, num = 1, D) {
  1 / rs_from_size(length = length,
                   width = width,
                   depth = depth,
                   num = num,
                   D = D)
}

#' @rdname rs_from_size
#'
#' @param temperature numeric Leaf temperature. Used to estimate the diffusion
#'  coefficient \code{D}.
#'
#' @export

gs_w_from_size <-
  function(length, width = length, depth, num = 1, temperature) {
  D <- D_water(temperature)
  1 / rs_from_size(length = length,
                   width = width,
                   depth = depth,
                   num = num,
                   D = D)
}

#' @rdname rs_from_size
#'
#' @export

gs_c_from_size <-
  function(length, width = length, depth, num = 1, temperature) {
  D <- D_CO2(temperature)
  1 / rs_from_size(length = length,
                   width = width,
                   depth = depth,
                   num = num,
                   D = D)
  }

#' Convert stomatal conductance
#'
#' Convert between stomatal conductances to water vapour and carbon dioxide.
#'
#' @inheritParams D_water
#' @inheritParams molar_vol
#' @param gs_w,gs_c,gs numeric Stomatal conductance to water vapour and carbon
#'   dioxide [\eqn{m^2\,s^{-1}}].
#'
#' @details These conversions are based on the diffusion coefficients, both of
#'   which are looked up based on the temperature.
#'
#' @return A numeric vector of stomatal conductance values expressed as
#'   \eqn{m^2\,s^{-1}}.
#'
#' @examples
#' # example code
#' gs_c_from_gs_w(1/120)
#' gs_vol2mol(1/120)
#'
#' @export
#'
#'
gs_c_from_gs_w <-
  function(gs_w,
           temperature = 21) {
    gs_w / D_water(temperature) * D_CO2(temperature)
  }

#' @rdname gs_c_from_gs_w
#'
#' @export
#'
gs_w_from_gs_c <-
  function(gs_c,
           temperature = 21) {
    gs_c * D_water(temperature) / D_CO2(temperature)
  }

#' @rdname gs_c_from_gs_w
#'
#' @export
#'
gs_vol2mol <-
  function(gs,
           temperature = 21,
           pressure = 101.3e3) {

    gs / molar_vol(temperature = temperature,
                   pressure = pressure)
  }

#' @rdname gs_c_from_gs_w
#'
#' @export
#'
gs_mol2vol <-
  function(gs_c,
           temperature = 21,
           pressure = 101.3e3) {
    gs_c * molar_vol(temperature = temperature,
                     pressure = pressure)
  }

#' Molar volume of an ideal gas
#'
#' Compute the molar volume of an ideal gas from temåerature and pressure.
#'
#' @param temperature numeric Temperature of the gas [\eqn{^{\circ}C}].
#' @param pressure numeric Pressure of the gas, e.g., atmospheric pressure [Pa].
#'
#' @details The ideal gas equation, \eqn{V = R * T / P}, is used to compute the
#' returned value. In the equation \eqn{R} is the gas constant
#' (\eqn{8.314\,m^3\,Pa\,mol^{-1}\,K^{-1}}), \eqn{T} temperature expressed
#' in degrees kelvin
#' and \eqn{P} the pressure expressed in pascals. The argument passed to
#' \code{temperature}, expressed in \eqn{^{\circ}C}, is re-expressed in
#' \eqn{K} for the computation.
#'
#' @return A numeric vector with the molar volume of a gas expressed in
#'   \eqn{m^3} per mole.
#'
#' @examples
#' molar_vol()
#' molar_vol(0)
#' molar_vol(0, 90e3)
#'
#' @export
#'
molar_vol <- function(temperature = 20, pressure = 101.3e3) {
  R <- 8.314 # m^3 Pa mol^-1 K^-1 gas constant
  T <- temperature + 273.15 # C -> K
  R * T / pressure
}

