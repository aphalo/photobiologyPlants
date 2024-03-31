#' Pr:Ptot ratio (photoequilibrium) from R:FR photon ratio.
#'
#' Calculation of Pfr:Ptot ratio for Type I Phytochrome from red:far-red photon
#' ratio. "Exact" only for dichromatic irradiation, only approximate for R:FR
#' ratio calculated from a broadband light source.
#'
#' @param R.FR R:FR a single value or a vector of photon ratio (dimentionless) values
#'
#' @return a single value or a vector of numeric values giving the Pr:Ptot
#'   dimensionless ratio
#' @export
#' @references Mancinelli, A.L. (1994) The physiology of phytochrome action. In
#' Photomorphogenesis in plants, 2nd edition. R.E. Kendrick and G.H.M.
#' Kronenberg, eds. Kluwer Academic Publishers, Dordrecht, pp. 211-269. ISBN
#' 978-0-7923-2551-2 (print), 978-94-011-1884-2 (on-line).
#' \doi{10.1007/978-94-011-1884-2_10}
#'
#' @seealso \code{\link[photobiology]{q_ratio}}
#' @examples
#' Pfr_Ptot_R_FR(1.15)
#' Pfr_Ptot_R_FR(0.10)
#' Pfr_Ptot_R_FR(c(0.1,1.15,5.0,20.0))
#'
Pfr_Ptot_R_FR <- function(R.FR){
  if (length(R.FR) == 0) return(numeric(0))
  ratio <- sapply(R.FR,
                  FUN=function(x){Pfr_P_ratio(c(660, 730), c(x, 1.0),
                                              unit.in="photon",
                                  check.spectrum=FALSE, use.cached.mult=FALSE)})
  return(ratio)
}
