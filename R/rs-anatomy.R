#' Stomatal conductance from sizes and density
#'
#' Computations based on mass transfer through pores.
#'
#' @param length,width numeric "Length" and "width" of the stomatal opening,
#'   i.e., the major and minor diameters of the ellipse describing the stomatal
#'   pore's mean cross section [\eqn{\mu m}].
#' @param depth numeric The depth (or diffusion path length) of the stomatal
#'   pore [\eqn{\mu m}].
#' @param num numeric Number of stomata per unit area [\eqn{mm^{-2}}].
#' @param D numeric Diffusion coefficient of the gas[\eqn{m^2\,s^{-1}}].
#'
#' @return A resistance expressed in \eqn{s m^{-1}} or a conductance
#'   expressed in \eqn{m s^{-1}}.
#'
#' @references
#' Monteith, J. L. and Unsworth M. H. (2008) Principles of Environmental
#'   Physics (3ed) Academic Press-Elsevier. ISBN: 978-0-12-505103-3. See
#'   Section 11.4 Mass transfer through pores.
#'
#' @examples
#' # a single round stomatal pore
#' gs_from_size(length = 20, depth = 5, D = D_water(23))
#' # a single elliptical stomatal pore
#' gs_from_size(length = 30, width = 10, depth = 5, D = D_water(23))
#' #
#' # 200 circular stomatal pores per mm^2
#' rs_from_size(length = 5,
#'              width = 5,
#'              depth = 10,
#'              num = 200e6,
#'              D = D_water(25))
#' # 50 elliptical stomatal pores per mm^2
#' rs_from_size(length = 10,
#'              width = 5,
#'              depth = 20,
#'              num = 50e6,
#'              D = D_water(23))
#'
#' @export
#'
rs_from_size <- function(length, width = length, depth, num = 1, D) {
  if (length < 1e-3) {
    message("Assuming 'length', 'width' and 'depth' are expressed in metres")
    k = 1
  } else {
    k = 1e-6
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
#'   dioxide [\eqn{m^2 s^{-1}}].
#'
#' @details These conversions are based on the diffusion coefficients, both of
#'   which are looked up based on the temperature.
#'
#' @return A numeric vector of stomatal conductance values expressed as
#'   \eqn{m^2 s^{-1}}.
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
#' (\eqn{8.314 m^3 Pa mol^{-1} ^{\circ}K^{-1}}), \eqn{T} temperature expressed
#' in degrees kelvin
#' and \eqn{P} the pressure expressed in pascals. The argument passed to
#' \code{temperature}, expressed in \eqn{^{\circ}C}, is reexpressed in
#' \eqn{^{\circ}K} for the computation.
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

