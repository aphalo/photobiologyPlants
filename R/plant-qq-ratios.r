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
#' @seealso \code{link[photobiologyWavebands]{Red}},
#'   \code{link[photobiologyWavebands]{Far_red}}.
#'
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
#' @seealso \code{link[photobiologyWavebands]{Blue}},
#'   \code{link[photobiologyWavebands]{Green}}.
#'
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
#' @seealso \code{link[photobiologyWavebands]{UVB}},
#'   \code{link[photobiologyWavebands]{UV}}.
#'
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
#' @seealso \code{link[photobiologyWavebands]{UVA}},
#'   \code{link[photobiologyWavebands]{UV}}.
#'
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

#' Calculate UVAlw:UV photon ratio from spectral irradiance.
#'
#' This function returns the UVA:UV photon ratio of a light source
#' spectrum.
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of UVAlw should be used,
#'   defaults to "plants". For UV "ISO" is always used.
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the UVA:UV photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @note Whenever possible use UVA1 instead of UVAlw and UVA2 instead of UVAsw
#'   as UVA1 and UVA2 are frequently used defintions, even if not standarddised,
#'   while UVAlw and UVAsw are ad-hoc definitions used for specific optical
#'   filters.
#'
#' @seealso \code{link[photobiologyWavebands]{UVA}},
#'   \code{link[photobiologyWavebands]{UV}}.
#'
#' @export
#' @examples
#' UVA_UV(sun.spct)
#'
UVAlw_UV <- function(spct, std = "plants",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE) {
  q_ratio(spct, UVAlw(std), UV("ISO"),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}

#' Calculate UVAsw:UV photon ratio from spectral irradiance.
#'
#' This function returns the UVAsw:UV photon ratio of a light source
#' spectrum.
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of UVAsw should be used,
#'   defaults to "plants". For UV "ISO" is always used.
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the UVA:UV photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @note Whenever possible use UVA1 instead of UVAlw and UVA2 instead of UVAsw
#'   as UVA1 and UVA2 are frequently used defintions, even if not standarddised,
#'   while UVAlw and UVAsw are ad-hoc definitions used for specific optical
#'   filters.
#'
#' @seealso \code{link[photobiologyWavebands]{UVA}},
#'   \code{link[photobiologyWavebands]{UV}}.
#'
#' @export
#' @examples
#' UVAsw_UV(sun.spct)
#'
UVAsw_UV <- function(spct, std = "plants",
                     use.cached.mult = FALSE,
                     use.hinges = TRUE) {
  q_ratio(spct, UVAsw(std), UV("ISO"),
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
#' @seealso \code{link[photobiologyWavebands]{UV}},
#'   \code{link[photobiologyWavebands]{PAR}}.
#'
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
#' @seealso \code{link[photobiologyWavebands]{UVB}},
#'   \code{link[photobiologyWavebands]{PAR}}.
#'
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
#' @seealso \code{link[photobiologyWavebands]{UVA}},
#'   \code{link[photobiologyWavebands]{PAR}}.
#'
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

#' Calculate UVA1:UV photon ratio from spectral irradiance.
#'
#' This function returns the UVA1:UV photon ratio of a light source
#' spectrum.
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of UVA1 should be used,
#'   defaults to "CIE".  For UV "ISO" is always used.
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the UVA:UV photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @seealso \code{link[photobiologyWavebands]{UV}},
#'   \code{link[photobiologyWavebands]{UVA}}.
#'
#' @export
#' @examples
#' UVA1_UV(sun.spct)
#'
UVA1_UV <- function(spct, std = "CIE",
                     use.cached.mult = FALSE,
                     use.hinges = TRUE) {
  q_ratio(spct, UVA1(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}

#' Calculate UVA2:UV photon ratio from spectral irradiance.
#'
#' This function returns the UVA2:UV photon ratio of a light source
#' spectrum.
#'
#' @param spct an object of class "source.spct"
#' @param std select which definition of UVA1 should be used,
#'   defaults to "CIE". For UV "ISO" is always used.
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors
#'
#' @return a single numeric nondimensional value giving the UVA:UV photon ratio,
#'   with name attribute set to the name of the wavebands, with "(q:q)"
#'   appended.
#'
#' @seealso \code{link[photobiologyWavebands]{UV}},
#'   \code{link[photobiologyWavebands]{UVA}}.
#'
#' @export
#' @examples
#' UVA2_UV(sun.spct)
#'
UVA2_UV <- function(spct, std = "CIE",
                     use.cached.mult = FALSE,
                     use.hinges = TRUE) {
  q_ratio(spct, UVA2(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges)
}
