#' Calculate phytochrome photoequilibrium
#'
#' Calculate the phytochrome photoequilibrium for monochromatic light from its
#' wavelength or from a spectrum expressed as spectral irradiance.
#'
#' @param x an R object
#' @param ... not used
#'
#' @details The calculations are based on data describing the photochemical
#'   constants for the plant photoreceptor phytochrome measured \emph{in vitro}
#'   and available for wavelengths in the range 380 nm to 770 nm as published by
#'   Mancinelli (1994). For reliable estimates of \eqn{P_{fr} / P_{tot}} from
#'   spectral irradiance, the spectrum should cover all these wavelengths with
#'   reasonably high wavelength resolution.
#'
#' @examples
#' # monochromatic light
#' Pfr_Ptot(620) # one wavelength in nm
#' Pfr_Ptot(c(570, 600, 630, 660, 690, 735, 760)) # six wavelengths
#' Pfr_Ptot(sun.spct) # spectrum of terrestrial sunligth
#'
#' @export
#'
#' @references
#' Mancinelli, A.L. (1994) The physiology of phytochrome action. In
#' Photomorphogenesis in plants, 2nd edition. R.E. Kendrick and G.H.M.
#' Kronenberg, eds. Kluwer Academic Publishers, Dordrecht, pp. 211-269. ISBN
#' 978-0-7923-2551-2 (print), 978-94-011-1884-2 (on-line).
#' \doi{10.1007/978-94-011-1884-2_10}
#'
#' @note If you use these data in a publication, please cite also the original
#'   source as given under references.
#'
Pfr_Ptot <- function(x, ...) UseMethod("Pfr_Ptot")

#' @describeIn Pfr_Ptot Default for generic function
#'
#' @export
#'
Pfr_Ptot.default <- function(x, ...) {
  warning("'Pfr_Ptot' is not defined for objects of class ", class(x)[1])
  return(NA_real_)
}

#' @describeIn Pfr_Ptot Specialization for numeric
#'
#' @param spct.out logical Flag indicating if the returned object should be of
#'   class \code{response_spct} instead of \code{numeric}.
#'
#' @return If \code{x} is \code{numeric}, giving wavelengths (nm), a vector of
#'   numeric values giving the \eqn{P_{fr} / P_{tot}} at each wavelength or a
#'   \code{generic_spct} object with the wavelength values sorted in ascending
#'   order and the corresponding \eqn{P_{fr} / P_{tot}} values in column
#'   \code{s.q.response}.
#'
#' @export
#'
Pfr_Ptot.numeric <- function(x, spct.out = length(x) > 20, ...) {
  if (spct.out && length(x) > 1) {
    x <- unique(sort(x))
    selector <- x >= 300 & x <= 770
    Pfr_Ptot <- numeric(length(x))
    Pfr_Ptot[!selector] <- NA
    Pfr_Ptot[selector] <- Pfr_P_ratio_mono(w.length = x[selector])
    return(response_spct(w.length = x, s.q.response = Pfr_Ptot))
  } else {
    selector <- (x >= 300 & x <= 770)
    Pfr_Ptot <- numeric(length(x))
    Pfr_Ptot[!selector] <- NA
    Pfr_Ptot[selector] <- Pfr_P_ratio_mono(w.length = x[selector])
    return(Pfr_Ptot)
  }
}

#' @describeIn Pfr_Ptot Specialization for source_spct
#'
#'   Calculate phytochrome photoequilibrium from spectral (photon) irradiance
#'
#' @param na.rm logical. If \code{TRUE} \code{link[stats]{na.omit}} is
#'   first called on \code{x}.
#'
#' @return If \code{x} is a \code{source_spct} object, a single numeric value
#'   giving the \eqn{P_{fr} / P_{tot}}.
#'
#' @export
#'
Pfr_Ptot.source_spct <- function(x, ..., na.rm = FALSE) {
  if (na.rm) {
    x <- stats::na.omit(x)
  }
  spct <- trim_spct(x, range = c(300, 770), verbose = FALSE)
  spct <- e2q(spct, action = "replace")
  if (anyNA(spct)) {
    return(NA_real_)
  }
  if (wl_stepsize(spct)[2] > 10) {
    # phytochrome data have steps 4 to 10 nm
    warning("Spectrum 'x' wavelength resolution: ",
            wl_stepsize(spct), ". Expect bad Pfr:Ptot estimate!", )
  }
  Pfr_P_ratio(
    w.length = spct$w.length,
    s.irrad = spct$s.q.irrad,
    unit.in = "photon",
    check.spectrum = FALSE,
    use.cached.mult = FALSE)
}
