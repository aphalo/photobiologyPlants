#' Calculate photon ratios from spectral irradiance
#'
#' These functions return photon ratios between two different wavebands computed
#' from one or more light source spectra.
#'
#' @param spct an object of class "source.spct" or an object of class
#'   "source.mspct" containing one of more spectra.
#' @param std character Name of the variants of the two waveband definitions to
#'   use.
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls.
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors.
#' @param ... named arguments to be forwarded to \code{q_ratio} methods.
#'
#' @details These functions are convenience wrappers on calls to method
#'   \code{\link[photobiology]{q_ratio}} with specific waveband definitions from
#'   package \code{\link[photobiologyWavebands]{photobiologyWavebands}}. To
#'   compute other photon ratios call method \code{\link[photobiology]{q_ratio}}
#'   with predefined or ad hoc \code{\link[photobiology]{waveband}} definitions.
#'
#'   The returned value is the ratio between two photon irradiances (or two
#'   photon fluence values) each integrated over the range of wavelengths in a
#'   waveband definition, which can differ in wavelength extent. Some ratios
#'   are defined for non-overlapping ranges of wavelengths (e.g., R:FR photon
#'   ratio) while others are defined for overlapping ranges of wavelengths
#'   (e.g., UVB:UV, which will never exceed 1 in value).
#'
#' @return When \code{spct} contains a single spectrum, a single named numeric
#'   dimensionless value giving a photon ratio, with name constructed from the
#'   name of the wavebands, with "(q:q)" appended is returned. When \code{spct}
#'   contains multiple spectra, either in long form or as a collection of
#'   spectral objects, the returned object is a data frame with a factor
#'   identifying the spectra and a numeric variable with the numeric values of
#'   the ratio.
#'
#' @seealso Ratios are computed with \code{\link[photobiology]{q_ratio}} with
#'   \code{\link[photobiology]{waveband}} objects as input. In the table below
#'   the wavebands and default for \code{std} used to compute the photon ratios
#'   in each of the functions are listed and linked to the respective help
#'   pages.
#'
#'   \tabular{llll}{
#'      \strong{Function}  \tab \strong{Numerator} \tab \strong{Denominator} \tab \strong{Default} \code{std} \cr
#'      \code{R_FR}  \tab \code{\link[photobiologyWavebands]{Red}} \tab \code{\link[photobiologyWavebands]{Far_red}} \tab "Smith20" \cr
#'      \code{B_G}   \tab \code{\link[photobiologyWavebands]{Blue}} \tab \code{\link[photobiologyWavebands]{Green}} \tab "Sellaro" \cr
#'      \code{UVB_UV}  \tab \code{\link[photobiologyWavebands]{UVB}} \tab \code{\link[photobiologyWavebands]{UV}} \tab "ISO" \cr
#'      \code{UVB_UVA}  \tab \code{\link[photobiologyWavebands]{UVB}} \tab \code{\link[photobiologyWavebands]{UVA}} \tab "ISO" \cr
#'      \code{UVA_UV}  \tab \code{\link[photobiologyWavebands]{UVA}} \tab \code{\link[photobiologyWavebands]{UV}} \tab "ISO" \cr
#'      \code{UVAlw_UV}  \tab \code{\link[photobiologyWavebands]{UVAlw}} \tab \code{\link[photobiologyWavebands]{UV}} \tab "plants" \cr
#'      \code{UVAsw_UV}  \tab \code{\link[photobiologyWavebands]{UVAsw}} \tab \code{\link[photobiologyWavebands]{UV}} \tab "plants" \cr
#'      \code{UV_PAR}  \tab \code{\link[photobiologyWavebands]{UV}} \tab \code{\link[photobiologyWavebands]{PAR}} \tab "ISO" \cr
#'      \code{UVB_PAR}  \tab \code{\link[photobiologyWavebands]{UVB}} \tab \code{\link[photobiologyWavebands]{PAR}} \tab "ISO" \cr
#'      \code{UVA_PAR}  \tab \code{\link[photobiologyWavebands]{UVA}} \tab \code{\link[photobiologyWavebands]{PAR}} \tab "ISO" \cr
#'      \code{UVA1_UV}  \tab \code{\link[photobiologyWavebands]{UVA1}} \tab \code{\link[photobiologyWavebands]{UV}} \tab "CIE" \cr
#'      \code{UVA2_UV}  \tab \code{\link[photobiologyWavebands]{UVA2}} \tab \code{\link[photobiologyWavebands]{UV}} \tab "CIE" \cr
#'      \code{UVA2_UVA}  \tab \code{\link[photobiologyWavebands]{UVA2}} \tab \code{\link[photobiologyWavebands]{UVA}} \tab "CIE" \cr
#'   }
#'
#' @export
#'
#' @name photon ratios
#' @rdname photon-ratios
#'
#' @examples
#' # default, one spectrum
#' R_FR(sun.spct)
#'
#' # default, multiple spectra
#' R_FR(sun_evening.spct)
#' R_FR(sun_evening.mspct)
#'
#' # different waveband definitions
#' R_FR(sun.spct, std = "Smith10")
#' R_FR(sun.spct, std = "Smith20")
#' R_FR(sun.spct, std = "Sellaro")
#' R_FR(sun.spct, std = "Apogee")
#'
R_FR <- function(spct, std = "Smith20",
                 use.cached.mult = FALSE,
                 use.hinges = TRUE,
                 ...) {
  q_ratio(spct, Red(std), Far_red(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' B_G(sun.spct)
#'
B_G <- function(spct, std = "Sellaro",
                use.cached.mult = FALSE,
                use.hinges = TRUE,
                ...) {
  q_ratio(spct, Blue(std), Green(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVB_UV(sun.spct)
#'
UVB_UV <- function(spct, std = "ISO",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE,
                   ...) {
  q_ratio(spct, UVB(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVB_UVA(sun.spct)
#'
UVB_UVA <- function(spct, std = "ISO",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE,
                   ...) {
  q_ratio(spct, UVB(std), UVA(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVA_UV(sun.spct)
#'
UVA_UV <- function(spct, std = "ISO",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE,
                   ...) {
  q_ratio(spct, UVA(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVAlw_UV(sun.spct)
#'
UVAlw_UV <- function(spct, std = "plants",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE,
                   ...) {
  q_ratio(spct, UVAlw(std), UV("ISO"),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVAsw_UV(sun.spct)
#'
UVAsw_UV <- function(spct, std = "plants",
                     use.cached.mult = FALSE,
                     use.hinges = TRUE,
                     ...) {
  q_ratio(spct, UVAsw(std), UV("ISO"),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UV_PAR(sun.spct)
#'
UV_PAR <- function(spct, std = "ISO",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE,
                   ...) {
  q_ratio(spct, UV(std), PAR(),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVB_PAR(sun.spct)
#'
UVB_PAR <- function(spct, std = "ISO",
                    use.cached.mult = FALSE,
                    use.hinges = TRUE,
                    ...) {
  q_ratio(spct, UVB(std), PAR(),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVA_PAR(sun.spct)
#'
UVA_PAR <- function(spct, std = "ISO",
                    use.cached.mult = FALSE,
                    use.hinges = TRUE,
                    ...) {
  q_ratio(spct, UVA(std), PAR(),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVA1_UV(sun.spct)
#'
UVA1_UV <- function(spct, std = "CIE",
                    use.cached.mult = FALSE,
                    use.hinges = TRUE,
                    ...) {
  q_ratio(spct, UVA1(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVA2_UV(sun.spct)
#'
UVA2_UV <- function(spct, std = "CIE",
                    use.cached.mult = FALSE,
                    use.hinges = TRUE,
                    ...) {
  q_ratio(spct, UVA2(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVA2_UVA(sun.spct)
#'
UVA2_UVA <- function(spct, std = "CIE",
                    use.cached.mult = FALSE,
                    use.hinges = TRUE,
                    ...) {
  q_ratio(spct, UVA2(std), UVA(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          ...)
}
