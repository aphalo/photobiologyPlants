#' Calculation of Pr:Ptot ratio from spectral irradiance
#'
#' Ratio between  Pr and Ptot (photoequilibrium)
#' for Type I Phytochrome.
#'
#' @usage Pr_P_ratio(w.length, s.irrad=rep(1.0,length(w.length)), unit.in="energy",
#' check.spectrum=TRUE, use.cached.mult=FALSE)
#' @param w.length numeric array of wavelength (nm)
#' @param s.irrad numeric array of spectral (energy) irradiances (W m-2 nm-1) or (mol s-1 m-2)
#' @param unit.in character string with allowed values "energy", and "photon", or its alias "quantum"
#' @param check.spectrum logical indicating whether to sanity check input data, default is TRUE
#' @param use.cached.mult logical indicating whether multiplier values should be cached between calls
#'
#' @return a single numeric value giving the unitless photon ratio
#' @export
#' @references
#' Mancinelli, A.L. (1994) The physiology of phytochrome action.
#' In Photomorphogenesis in plants, 2nd edition. R.E. Kendrick and
#' G.H.M. Kronenberg, eds. Kluwer Academic Publishers, Dordrecht, pp. 211-269.
#' ISBN 978-0-7923-2551-2 (print), 978-94-011-1884-2 (on-line).
#' \href{http://dx.doi.org/10.1007/978-94-011-1884-2_10}{DOI 10.1007/978-94-011-1884-2_10}
#'
#' @seealso \code{\link[photobiology]{photon_ratio}} and \code{\link[photobiology]{energy_ratio}}
#' @examples
#' data(sun.data)
#' trimmed.sun.data <- with(sun.data, trim_tails(w.length, s.e.irrad, low.limit=300, high.limit=770))
#' with(trimmed.sun.data, Pr_P_ratio(w.length, s.irrad))
#' with(sun.data, Pr_P_ratio(w.length, s.e.irrad))
#' with(sun.data, Pr_P_ratio(w.length, s.e.irrad, use.cached.mult=TRUE))
#' with(sun.data, Pr_P_ratio(w.length, s.e.irrad, check.spectrum=FALSE))
#'
Pr_P_ratio <- function(w.length, s.irrad=rep(1.0,length(w.length)), unit.in="energy",
                       check.spectrum=TRUE, use.cached.mult=FALSE){
  # sanity check for wavelengths
  if (check.spectrum && !check_spectrum(w.length, s.irrad)) {
    return(NA)
  }
  if (unit.in=="energy"){
    s.q.irrad <- as_quantum_mol(w.length,s.irrad)
  }
  else if (unit.in=="photon" || unit.in=="quantum") {
    s.q.irrad <- s.irrad
  } else {
    warning("'unit.in' value not supported.")
    return(NA)
  }
  #
  Pr.wave.raw <- s.q.irrad * Phy_Sigma_R(w.length, use.cached.mult)
  Pfr.wave.raw <- s.q.irrad * Phy_Sigma_FR(w.length, use.cached.mult)
  if (length(w.length)==1){
    return(Pr.wave.raw / (Pr.wave.raw + Pfr.wave.raw))
  }
  # we remove data "rows" with NAs
  Pr.selector <- !is.na(Pr.wave.raw)
  Pfr.selector <- !is.na(Pfr.wave.raw)
  selector <- Pr.selector & Pfr.selector
  if (any(!selector)) {
    warning("NAs removed. Possibly spectral data extends beyond 300...770 nm.")
  }
  Pr.wave <- Pr.wave.raw[selector]
  Pfr.wave <- Pfr.wave.raw[selector]
  w.length <- w.length[selector]
  # we integrate over wavelengths
  Pr.int <- integrate_irradiance(w.length, Pr.wave)
  Pfr.int <- integrate_irradiance(w.length, Pfr.wave)
  # we calculate the ratio at photoequilibrium
  return(Pr.int / (Pr.int + Pfr.int))
}
