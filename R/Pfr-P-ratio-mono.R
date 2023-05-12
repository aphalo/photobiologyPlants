#' Calculation of Pfr:Ptot ratio for monochromatic radiation.
#'
#' Ratio between Pr and Ptot (photoequilibrium)
#' for Type I Phytochrome.
#'
#' @section Warning!:
#' Exact only for narrow band light sources!
#'
#' @param w.length a single value or a vector of wavelengths (nm)
#'
#' @return a single value or a vector of numeric values giving the unitless
#'   ratio
#'
#' @references Mancinelli, A.L. (1994) The physiology of phytochrome action. In
#' Photomorphogenesis in plants, 2nd edition. R.E. Kendrick and G.H.M.
#' Kronenberg, eds. Kluwer Academic Publishers, Dordrecht, pp. 211-269. ISBN
#' 978-0-7923-2551-2 (print), 978-94-011-1884-2 (on-line).
#' \doi{10.1007/978-94-011-1884-2_10}
#'
#' @keywords internal
#'
Pfr_P_ratio_mono <- function(w.length){
  if (length(w.length)==0) return(numeric(0))
  ratio <- sapply(w.length,
                  FUN=function(x){Pfr_P_ratio(x, unit.in="photon",
                                             check.spectrum=FALSE, use.cached.mult=FALSE)})
  return(ratio)
}
