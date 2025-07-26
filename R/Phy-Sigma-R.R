#' Pr Sigma as a function of wavelength
#'
#' Pr Sigma as a function of wavelength, calculated by interpolation from data
#' for Type I Phytochrome as compiled by Mancinelli (1994).
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
#' \doi{10.1007/978-94-011-1884-2_10}
#'
#' @family phytochrome-related functions and data
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
