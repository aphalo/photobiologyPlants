#' Pr Sigma as a function of wavelength
#'
#' Pr Sigma as a function of wavelength, calculated by interpolatio from data
#' for Type I Phytochrome as compiled by Mancinelli (1994).
#'
#' @usage Phy_Sigma_R(w.length, use.cached.mult=FALSE)
#' @param w.length numeric array of wavelength (nm)
#' @param use.cached.mult logical ignored
#'
#' @return a numeric array with values for Sigma
#' @export
#' @references Mancinelli, A.L. (1994) The physiology of phytochrome action. In
#' Photomorphogenesis in plants, 2nd edition. R.E. Kendrick and G.H.M.
#' Kronenberg, eds. Kluwer Academic Publishers, Dordrecht, pp. 211-269. ISBN
#' 978-0-7923-2551-2 (print), 978-94-011-1884-2 (on-line).
#' \href{http://dx.doi.org/10.1007/978-94-011-1884-2_10}{DOI
#' 10.1007/978-94-011-1884-2_10}
#'
#' @seealso \code{\link[photobiologyPlants]{Phy_Sigma}},
#'   \code{\link[photobiologyPlants]{Pfr_P_ratio}} and
#'   \code{\link[photobiologyPlants]{Pfr_P_ratio_R_FR}}
#'
#' @examples
#' with(sun.data, Phy_Sigma_R(w.length))
#' with(sun.data, Phy_Sigma_R(w.length, TRUE))
#'
Phy_Sigma_R <-
  function(w.length, use.cached.mult=FALSE){
    Sigma.R.mult <- numeric(length(w.length))
    Sigma.R.mult[w.length >= 300 & w.length <= 770] <-
      stats::spline(photobiologyPlants::phytochrome.spct$w.length,
                    photobiologyPlants::phytochrome.spct$Sigma.R,
             xout = w.length[w.length >= 300 & w.length <= 770])$y
    Sigma.R.mult[w.length < 300 | w.length > 770] <- NA

    return(Sigma.R.mult)
  }
