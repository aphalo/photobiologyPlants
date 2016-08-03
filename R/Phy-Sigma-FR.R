#' Pfr Sigma as a function of wavelength
#'
#' Pfr Sigma as a function of wavelength, calculated by interpolatio from data
#' for Type I Phytochrome as compiled by Mancinelli (xxxx).
#'
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
#'   \code{\link[photobiologyPlants]{Pfr_Ptot}} and
#'   \code{\link[photobiologyPlants]{Pfr_Ptot_R_FR}}
#' @examples
#' with(sun.spct, Phy_Sigma_FR(w.length))
#' with(sun.spct, Phy_Sigma_FR(w.length, TRUE))
#'
Phy_Sigma_FR <-
  function(w.length, use.cached.mult=FALSE){
    Sigma.FR.mult <- numeric(length(w.length))
    Sigma.FR.mult[w.length >= 300 & w.length <= 770] <-
      stats::spline(photobiologyPlants::phytochrome.spct$w.length,
                    photobiologyPlants::phytochrome.spct$Sigma.FR,
             xout = w.length[w.length >= 300 & w.length <= 770])$y
    Sigma.FR.mult[w.length < 300 | w.length > 770] <- NA

    return(Sigma.FR.mult)
  }
