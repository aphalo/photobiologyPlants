#' Calculate R:FR photon ratio from spectral irradiance.
#'
#' This function returns the red:far-red photon ratio of a light source
#' spectrum.
#'
#' @usage R_FR(spct, std = "Smith10", use.cached.mult =
#'   getOption("photobiology.use.cached.mult", default = FALSE),
#'   use.hinges=getOption("photobiology.use.hinges", default=NULL))
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of red and far-red should be used,
#'   defaults to "Smith"
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the R:FR photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @keywords manip misc
#' @export
#' @examples
#' R_FR(sun.spct)
#'
R_FR <- function(spct, std = "Smith10",
                 use.cached.mult = getOption("photobiology.use.cached.mult", default = FALSE),
                 use.hinges = getOption("photobiology.use.hinges", default=NULL)) {
  q_ratio(spct, Red(std), Far_red(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}


#' Calculate B:G photon ratio from spectral irradiance.
#'
#' This function returns the blue:green photon ratio of a light source spectrum.
#'
#' @usage B_G(spct, std = "Sellaro", use.cached.mult =
#'   getOption("photobiology.use.cached.mult", default = FALSE),
#'   use.hinges=getOption("photobiology.use.hinges", default=NULL))
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of red and far-red should be used,
#'   defaults to "Sellaro"
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the B:G photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @keywords manip misc
#' @export
#' @examples
#' R_FR(sun.spct)
#'
B_G <- function(spct, std = "Sellaro",
                use.cached.mult = getOption("photobiology.use.cached.mult", default = FALSE),
                use.hinges = getOption("photobiology.use.hinges", default=NULL)) {
  q_ratio(spct, Blue(std), Green(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}
