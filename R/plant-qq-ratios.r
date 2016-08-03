#' Calculate R:FR photon ratio from spectral irradiance.
#'
#' This function returns the red:far-red photon ratio of a light source
#' spectrum.
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
                 use.cached.mult = FALSE,
                 use.hinges = TRUE) {
  q_ratio(spct, Red(std), Far_red(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}


#' Calculate B:G photon ratio from spectral irradiance.
#'
#' This function returns the blue:green photon ratio of a light source spectrum.
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of blue and green should be used,
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
#' B_G(sun.spct)
#'
B_G <- function(spct, std = "Sellaro",
                use.cached.mult = FALSE,
                use.hinges = TRUE) {
  q_ratio(spct, Blue(std), Green(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}

#' Calculate UVB:UV photon ratio from spectral irradiance.
#'
#' This function returns the UVB:UV photon ratio of a light source
#' spectrum.
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of UVB and UV should be used,
#'   defaults to "ISO"
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the UVB:UV photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @keywords manip misc
#' @export
#' @examples
#' UVB_UV(sun.spct)
#'
UVB_UV <- function(spct, std = "ISO",
                 use.cached.mult = FALSE,
                 use.hinges = TRUE) {
  q_ratio(spct, UVB(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}

#' Calculate UVA:UV photon ratio from spectral irradiance.
#'
#' This function returns the UVA:UV photon ratio of a light source
#' spectrum.
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of UVB and UV should be used,
#'   defaults to "ISO"
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the UVA:UV photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @keywords manip misc
#' @export
#' @examples
#' UVA_UV(sun.spct)
#'
UVA_UV <- function(spct, std = "ISO",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE) {
  q_ratio(spct, UVA(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}

#' Calculate UV:PAR photon ratio from spectral irradiance.
#'
#' This function returns the UV:PAR photon ratio of a light source
#' spectrum.
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of UV should be used,
#'   defaults to "ISO"
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the UV:PAR photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @keywords manip misc
#' @export
#' @examples
#' UV_PAR(sun.spct)
#'
UV_PAR <- function(spct, std = "ISO",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE) {
  q_ratio(spct, UV(std), PAR(),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}

#' Calculate UVB:PAR photon ratio from spectral irradiance.
#'
#' This function returns the UVB:PAR photon ratio of a light source
#' spectrum.
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of UVB should be used,
#'   defaults to "ISO"
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the UVB:UV photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @keywords manip misc
#' @export
#' @examples
#' UVB_PAR(sun.spct)
#'
UVB_PAR <- function(spct, std = "ISO",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE) {
  q_ratio(spct, UVB(std), PAR(),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}

#' Calculate UVA:PAR photon ratio from spectral irradiance.
#'
#' This function returns the UVA:PAR photon ratio of a light source
#' spectrum.
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of UVA should be used,
#'   defaults to "ISO"
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the UVA:PAR photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @keywords manip misc
#' @export
#' @examples
#' UVA_PAR(sun.spct)
#'
UVA_PAR <- function(spct, std = "ISO",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE) {
  q_ratio(spct, UVA(std), PAR(),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}
