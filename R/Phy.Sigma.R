#' Phytochrome Sigma as a function of wavelength
#'
#' Phytochrome Sigma as a function of wavelength, calculated by interpolatio
#' from data for Type I Phytochrome as compiled by Mancinelli (1994).
#'
 #' @param w.length numeric array of wavelength (nm)
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
#' @seealso \code{\link[photobiologyPlants]{Pfr_P_ratio}} and
#'   \code{\link[photobiologyPlants]{Pfr_P_ratio_R_FR}}
#' @examples
#' with(sun.data, Phy_Sigma(w.length))
#'
Phy_Sigma <-
  # Sigma = Sigma_R + Sigma_FR
  function(w.length){
    Sigma.int <- numeric(length(w.length))
    Sigma.int[w.length >= 300 & w.length <= 770] <-
      stats::spline(photobiologyPlants::phytochrome.spct$w.length,
                    photobiologyPlants::phytochrome.spct$Sigma.R +
                      photobiologyPlants::phytochrome.spct$Sigma.FR,
             xout=w.length[w.length >= 300 & w.length <= 770])$y
    Sigma.int[w.length < 300 | w.length > 770] <- NA
    return(Sigma.int)
  }
