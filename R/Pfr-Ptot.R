#' Calculate phytochrome photoequilibrium
#'
#' Calculate the phytochrome photoequilibrium for monochromatic light from its
#' wavelength or from a spectrum expressed as spectral irradiance.
#'
#' @param x an R object. A \code{numeric} vector
#' @param spct.out logical Flag indicating if the returned object should be of
#'   class \code{response_spct} instead of \code{numeric}.
#' @param na.rm logical. If \code{TRUE} \code{link[stats]{na.omit}} is first
#'   called on \code{x}.
#' @param return.tb logical If \code{TRUE} force return of a data frame for a
#'   single spectrum, to match the returned class for collections of spectra.
#' @param attr2tb character vector, see
#'   \code{\link[photobiology]{add_attr2tb}()} the syntax for \code{attr2tb}
#'   passed as is to formal parameter \code{col.names}.
#' @param idx character Name of the column with the names of the members of the
#'   collection of spectra.
#' @param .parallel	if TRUE, apply function in parallel, using parallel backend
#'   provided by foreach
#' @param .paropts a list of additional options passed into the foreach function
#'   when parallel computation is enabled. This is important if (for example)
#'   your code relies on external data or packages: use the .export and
#'   .packages arguments to supply them so that all cluster nodes have the
#'   correct environment set up for computing.
#' @param ... currently ignored.
#'
#' @details The calculations are based on data describing the photochemical
#'   constants for the plant photoreceptor phytochrome measured \emph{in vitro}
#'   and available for wavelengths in the range 380 nm to 770 nm as published by
#'   Mancinelli (1994). For reliable estimates of \eqn{P_{fr} / P_{tot}} from
#'   spectral irradiance, the spectrum should cover all these wavelengths with
#'   reasonably high wavelength resolution.
#'
#'   Two approaches are possible, using wavelength for monochromatic radiation
#'   or spectral irradiance data. Spectral irradiance data are accepted either
#'   as individual spectra or collections of spectra.
#'
#' @return When input is spectral data, a numeric vector of values
#'   giving the \eqn{P_{fr} / P_{tot}} or a data frame are returned, possibly
#'   with attributes extracted from the spectral objects as additional
#'   columns. When input is a numeric vector of wavelengths, the returned
#'   object is either a numeric vector or a \code{generic_spct} object with
#'   giving the \eqn{P_{fr} / P_{tot}} for each wavelength in the input.
#'
#' @examples
#' # monochromatic light
#' Pfr_Ptot(620) # one wavelength in nm
#' Pfr_Ptot(c(570, 600, 630, 660, 690, 735, 760)) # six wavelengths
#' # spectral irradiance
#' Pfr_Ptot(sun.spct) # one spectrum
#' Pfr_Ptot(sun.spct, attr2tb = "when.measured")
#' Pfr_Ptot(sun_evening.spct) # five spectra
#' Pfr_Ptot(sun_evening.mspct)
#' Pfr_Ptot(sun_evening.mspct, attr2tb = "when.measured")
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
#' @family phytochrome-related functions and data
#'
Pfr_Ptot <- function(x, ...) UseMethod("Pfr_Ptot")

#' @rdname Pfr_Ptot
#'
#' @export
#'
Pfr_Ptot.default <- function(x, ...) {
  warning("'Pfr_Ptot' is not defined for objects of class ", class(x)[1])
  return(NA_real_)
}

#' @rdname Pfr_Ptot
#'
#' @export
#'
Pfr_Ptot.numeric <- function(x,
                             spct.out = length(x) > 20,
                             ...) {
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

#' @rdname Pfr_Ptot
#'
#' @export
#'
Pfr_Ptot.source_spct <- function(x,
                                 return.tb = !is.null(attr2tb),
                                 attr2tb = NULL,
                                 ...,
                                 na.rm = FALSE) {
  if (return.tb || getMultipleWl(x) > 1L) {
    Pfr_Ptot.source_mspct(x = photobiology::subset2mspct(x),
                          return.tb = TRUE,
                          attr2tb = attr2tb)
  } else {
    if (na.rm) {
      x <- stats::na.omit(x)
    }
    spct <- photobiology::trim_spct(x, range = c(300, 770), verbose = FALSE)
    spct <- photobiology::e2q(spct, action = "replace")
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
}

#' @rdname Pfr_Ptot
#'
#' @export
#'
Pfr_Ptot.source_mspct <- function(x,
                                  return.tb = TRUE,
                                  attr2tb = NULL,
                                  ...,
                                  na.rm = FALSE,
                                  idx = "spct.idx",
                                  .parallel = FALSE,
                                  .paropts = NULL) {

  if (!return.tb & !is.null(attr2tb)) {
    warning("Overriding 'return.tb = FALSE' as '!is.null(attr2tb)'!")
  }

  x <-
    subset2mspct(x = x) # expand long form spectra within collection

  z <-
    photobiology::msdply(
      mspct = x,
      .fun = Pfr_Ptot.source_spct,
       idx = idx,
      col.names = "Pfr_Ptot",
      .parallel = .parallel,
      .paropts = .paropts
    )

  if (return.tb) {
    photobiology::add_attr2tb(tb = z,
                              mspct = x,
                              col.names = attr2tb,
                              idx = idx)
  } else {
    z[["Pfr_Ptot"]]
  }
}

