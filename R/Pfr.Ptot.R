#' Calculate phytochrome photoequilibrium
#'
#' A method implemented for objects of different classes.
#'
#' @param x an R object
#' @param ... not used
#'
#' @export Pfr_Ptot
#'
#' @references
#' Mancinelli, A.L. (1994) The physiology of phytochrome action. In
#' Photomorphogenesis in plants, 2nd edition. R.E. Kendrick and G.H.M.
#' Kronenberg, eds. Kluwer Academic Publishers, Dordrecht, pp. 211-269. ISBN
#' 978-0-7923-2551-2 (print), 978-94-011-1884-2 (on-line).
#' \href{http://dx.doi.org/10.1007/978-94-011-1884-2_10}{DOI
#' 10.1007/978-94-011-1884-2_10}
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
#'   numeric values giving the unitless photon ratio at each wavelength or a
#'   \code{generic_spct} object with the wavelength values sorted in ascending
#'   order and the corresponding \code{Pfr_Ptot} values in column
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
#' @return If \code{x} is a \code{source_spct} object, a single numeric value
#'   giving the unitless photon ratio
#'
#' @export
#'
Pfr_Ptot.source_spct <- function(x, ...) {
  spct <- trim_spct(x, range = c(300, 770), verbose = FALSE)
  e2q(spct, byref = TRUE)
  Pfr_P_ratio(w.length = spct$w.length,
             s.irrad = spct$s.q.irrad,
             unit.in = "photon",
             check.spectrum = FALSE,
             use.cached.mult = FALSE)
}
