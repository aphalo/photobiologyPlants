#' Calculate photon ratios from spectral irradiance
#'
#' Photon ratios used in plant photobiology to summarize difference in spectral
#' composition of light. Both historical, current and recently proposed photon
#' ratios are implemented. The denominator is always a photon (= quantum)
#' irradiance for a single waveband. The denominator is the irradiance either
#' in a single waveband or the sum of irradiances in two wavebands.
#'
#' @param spct an object of class "source.spct" or an object of class
#'   "source.mspct" containing one of more spectra.
#' @param std character Name of the variants of the waveband definitions to
#'   use (see table below).
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls.
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors.
#' @param naming character one of "long", "default", "short" or "none". Used to
#'   select the type of names to assign to the returned value.
#' @param name.tag character Used to tag the name of the returned values.
#' @param ... named arguments to be forwarded to \code{q_ratio} or
#'   \code{q_irrad} methods.
#'
#' @details These functions are convenience wrappers on calls to method
#'   \code{\link[photobiology]{q_ratio}()} with specific waveband definitions from
#'   package \code{\link[photobiologyWavebands]{photobiologyWavebands}}. To
#'   compute other photon ratios call method \code{\link[photobiology]{q_ratio}()}
#'   with predefined or ad hoc \code{\link[photobiology]{waveband}()} definitions.
#'
#'   Many predefined \code{\link[photobiology]{waveband}()} definitions accept a
#'   character string as argument to \code{std} used to select among different
#'   standardised and ad hoc but frequently used variations in the wavelength
#'   ranges. These functions for computing photon ratios forward the arguments
#'   received through parameter \code{std} to the parameter of the same name of
#'   the different waveband constructors as listed in the table below.
#'
#'   The returned value is the ratio between two photon irradiances (identical
#'   two photon fluence values) each integrated over the range of wavelengths in
#'   a waveband definition, which can differ in wavelength extent. Some ratios
#'   are defined for non-overlapping ranges of wavelengths (e.g., R:FR photon
#'   ratio) while others are defined for overlapping ranges of wavelengths
#'   (e.g., UVB:UV, which will never exceed 1 in value) or for the sum of
#'   irradiance in two wavelength ranges (e.g., FR:R+FR).
#'
#'   \tabular{lllll}{
#'      \strong{Function}  \tab \strong{Numerator} \tab \strong{Denominator} \tab \strong{Default} \code{std} \tab \strong{Ref.} \cr
#'      \code{R_FR()}  \tab \code{\link[photobiologyWavebands]{Red}(std)} \tab \code{\link[photobiologyWavebands]{Far_red}(std)} \tab "Smith20" \tab 4,5,6 \cr
#'      \code{R_RpFR()}  \tab \code{\link[photobiologyWavebands]{Red}(std)} \tab \code{\link[photobiologyWavebands]{Red}(std)} + \code{\link[photobiologyWavebands]{Far_red}(std)} \tab "Smith20" \tab 2 \cr
#'      \code{FR_RpFR()}  \tab \code{\link[photobiologyWavebands]{Far_red}(std)} \tab \code{\link[photobiologyWavebands]{Red}(std)} + \code{\link[photobiologyWavebands]{Far_red}(std)} \tab "Smith20" \tab 2\cr
#'      \code{FR_ePAR()}  \tab \code{\link[photobiologyWavebands]{Far_red}(std)} \tab \code{\link[photobiologyWavebands]{PAR}("ePAR")} \tab "Smith20" \tab 3 \cr
#'      \code{FR_PAR()}  \tab \code{\link[photobiologyWavebands]{Far_red}(std)} \tab \code{\link[photobiologyWavebands]{PAR}("McCree")} \tab "Smith20" \tab 3 \cr
#'      \code{B_G()}   \tab \code{\link[photobiologyWavebands]{Blue}(std)} \tab \code{\link[photobiologyWavebands]{Green}(std)} \tab "Sellaro" \tab 4 \cr
#'      \code{UVB_UV()}  \tab \code{\link[photobiologyWavebands]{UVB}(std)} \tab \code{\link[photobiologyWavebands]{UV}(std)} \tab "ISO" \tab 1,7 \cr
#'      \code{UVB_UVA()}  \tab \code{\link[photobiologyWavebands]{UVB}(std)} \tab \code{\link[photobiologyWavebands]{UVA}(std)} \tab "ISO" \tab 1,7 \cr
#'      \code{UVA_UV()}  \tab \code{\link[photobiologyWavebands]{UVA}(std)} \tab \code{\link[photobiologyWavebands]{UV}(std)} \tab "ISO" \tab 1,7 \cr
#'      \code{UVAlw_UV()}  \tab \code{\link[photobiologyWavebands]{UVAlw}(std)} \tab \code{\link[photobiologyWavebands]{UV}(std)} \tab "plants" \tab 7,8 \cr
#'      \code{UVAsw_UV()}  \tab \code{\link[photobiologyWavebands]{UVAsw}(std)} \tab \code{\link[photobiologyWavebands]{UV}(std)} \tab "plants" \tab 7,8\cr
#'      \code{UV_PAR()}  \tab \code{\link[photobiologyWavebands]{UV}(std)} \tab \code{\link[photobiologyWavebands]{PAR}("McCree")} \tab "ISO" \tab 1 \cr
#'      \code{UVB_PAR()}  \tab \code{\link[photobiologyWavebands]{UVB}(std)} \tab \code{\link[photobiologyWavebands]{PAR}("McCree")} \tab "ISO" \tab 1 \cr
#'      \code{UVA_PAR()}  \tab \code{\link[photobiologyWavebands]{UVA}(std)} \tab \code{\link[photobiologyWavebands]{PAR}("McCree")} \tab "ISO" \tab 1 \cr
#'      \code{UVA1_UV()}  \tab \code{\link[photobiologyWavebands]{UVA1}(std)} \tab \code{\link[photobiologyWavebands]{UV}(std)} \tab "CIE" \tab 1,7 \cr
#'      \code{UVA2_UV()}  \tab \code{\link[photobiologyWavebands]{UVA2}(std)} \tab \code{\link[photobiologyWavebands]{UV}(std)} \tab "CIE" \tab 1,7 \cr
#'      \code{UVA1_UVA()}  \tab \code{\link[photobiologyWavebands]{UVA1}(std)} \tab \code{\link[photobiologyWavebands]{UVA}(std)} \tab "CIE" \tab 1,7 \cr
#'      \code{UVA2_UVA()}  \tab \code{\link[photobiologyWavebands]{UVA2}(std)} \tab \code{\link[photobiologyWavebands]{UVA}(std)} \tab "CIE" \tab 1,7 \cr
#'   }
#'
#' @return When \code{spct} contains a single spectrum, a single named numeric
#'   dimensionless value giving a photon ratio, with name constructed from the
#'   name of the wavebands, with "(q:q)" appended is returned. When \code{spct}
#'   contains multiple spectra, either in long form or as a collection of
#'   spectral objects, the returned object is a data frame with a factor
#'   identifying the spectra and a numeric variable with the numeric values of
#'   the ratio.
#'
#' @seealso Ratios are computed with \code{\link[photobiology]{q_ratio}()} with
#'   \code{\link[photobiology]{waveband}()} objects as input. Two "fractions": R
#'   fraction \code{R_RpFR()} and FR fraction \code{FR_RpFR()} are computed using
#'   directly \code{\link[photobiology]{q_irrad}()}, which is also called by
#'   \code{\link[photobiology]{q_ratio}()}. In the table above the wavebands and
#'   default argument for \code{std} used to compute the photon ratios in each
#'   of the functions are listed and linked to the respective help pages.
#'
#' @references
#'
#' [1] Aphalo PJ, Albert A, Björn LO, McLeod AR, Robson TM, Rosenqvist E (Eds.).
#' 2012. Beyond the Visible: A handbook of best practice in plant UV
#' photobiology. Helsinki: University of Helsinki, Department of Biosciences,
#' Division of Plant Biology. \doi{10.31885/9789521083631}.
#'
#' [2] Kusuma P, Bugbee B. 2021. Improving the Predictive Value of Phytochrome
#' Photoequilibrium: Consideration of Spectral Distortion Within a Leaf.
#' Frontiers in Plant Science 12. \doi{10.3389/fpls.2021.596943}.
#'
#' [3] Kusuma P, Bugbee B. 2021. Far-red Fraction: An Improved Metric for
#' Characterizing Phytochrome Effects on Morphology. Journal of the American
#' Society for Horticultural Science 146, 3–13. \doi{10.21273/jashs05002-20}.
#'
#' [4] Sellaro R, Crepy M, Trupkin SA, Karayekov E, Buchovsky AS, Rossi C, Casal
#' JJ. 2010. Cryptochrome as a sensor of the blue / green ratio of natural
#' radiation in Arabidopsis. Plant Physiology 154, 401–409.
#' \doi{10.1104/pp.110.160820}.
#'
#' [5] Smith H. 1981. Plants and the Daylight Spectrum. London: Academic Press.
#'
#' [6] Smith H, Holmes MG. 1984. Techniques in Photomorphogenesis. London:
#' Academic Press.
#'
#' [7] Rai N, Morales LO, Aphalo PJ. 2021. Perception of solar UV radiation by
#' plants: photoreceptors and mechanisms. Plant Physiology 186, 1382–1396.
#' \doi{10.1093/plphys/kiab162}.
#'
#' [8] Rai N, Farkas AOD, Safronov O, et al. 2020. The photoreceptor UVR8
#' mediates the perception of both UV-B and UV-A wavelengths up to 350 nm of
#' sunlight with responsivity moderated by cryptochromes. Plant, Cell &
#' Environment 43, 1513–1527. \doi{10.1111/pce.13752}.
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
#' # naming
#' R_FR(sun.spct, naming = "default")
#' R_FR(sun.spct, naming = "none")
#' R_FR(sun.spct, naming = "short")
#' R_FR(sun.spct, naming = "long")
#' R_FR(sun.spct, name.tag = "")
#'
#' # default, multiple spectra
#' R_FR(sun_evening.spct)
#' R_FR(sun_evening.mspct)
#' R_FR(sun_evening.mspct, naming = "long")
#' R_FR(sun_evening.mspct, name.tag = "")
#'
#' # different waveband definitions
#' R_FR(sun.spct, std = "Smith10")
#' R_FR(sun.spct, std = "Smith20")
#' R_FR(sun.spct, std = "Sellaro")
#' R_FR(sun.spct, std = "Apogee")
#' R_FR(sun.spct, std = "Apogee", naming = "long")
#'
R_FR <- function(spct, std = "Smith20",
                 use.cached.mult = FALSE,
                 use.hinges = TRUE,
                 naming = "short",
                 name.tag = "[q:q]",
                 ...) {
  if (std == "Smith") std <- "Smith20"
  q_ratio(spct, Red(std), Far_red(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          naming = naming,
          name.tag = name.tag,
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#'
#' @examples
#' FR_ePAR(sun.spct)
#' FR_ePAR(sun.spct, "Smith10")
#' FR_ePAR(sun.spct, "Smith20")
#' FR_ePAR(sun.spct, "Sellaro")
#'
FR_ePAR <- function(spct, std = "Smith20",
                    use.cached.mult = FALSE,
                    use.hinges = TRUE,
                    naming = "short",
                    name.tag = "[q:q]",
                    ...) {
  q_ratio(spct, Far_red(std), PAR("ePAR"),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          naming = naming,
          name.tag = name.tag,
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' FR_PAR(sun.spct)
#' FR_PAR(sun.spct, "Smith10")
#' FR_PAR(sun.spct, "Smith20")
#' FR_PAR(sun.spct, "Sellaro")
#'
FR_PAR <- function(spct, std = "Smith20",
                  use.cached.mult = FALSE,
                  use.hinges = TRUE,
                  naming = "short",
                  name.tag = "[q:q]",
                  ...) {
  q_ratio(spct, Far_red(std), PAR("McCree"),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          naming = naming,
          name.tag = name.tag,
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' FR_RpFR(sun.spct)
#' FR_RpFR(sun.spct, "Smith10")
#' FR_RpFR(sun.spct, "Smith20")
#' FR_RpFR(sun.spct, "Sellaro")
#'
FR_RpFR <- function(spct, std = "Smith20",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE,
                   naming = "short",
                   name.tag = "[q:q]",
                   ...) {
  q_FR <- q_irrad(spct = spct,
                  w.band = photobiologyWavebands::Far_red(std = std),
                  naming = naming,
                  ...)
  q_R <- q_irrad(spct = spct,
                  w.band = photobiologyWavebands::Red(std = std),
                  naming = naming,
                 ...)
  ratio <- q_FR / (q_R + q_FR)
  if (naming != "none") {
    names(ratio) <-
      paste(names(q_FR), ":", names(q_R), "+", names(q_FR), name.tag, sep = "")
  }
  attr(ratio, "radiation.unit") <- "q:q ratio"
  ratio
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' R_RpFR(sun.spct)
#' R_RpFR(sun.spct, "Smith10")
#' R_RpFR(sun.spct, "Smith20")
#' R_RpFR(sun.spct, "Sellaro")
#'
R_RpFR <- function(spct, std = "Smith20",
                   use.cached.mult = FALSE,
                   use.hinges = TRUE,
                   naming = "short",
                   name.tag = "[q:q]",
                   ...) {
  q_FR <- q_irrad(spct = spct,
                  w.band = photobiologyWavebands::Far_red(std = std),
                  naming = naming,
                  ...)
  q_R <- q_irrad(spct = spct,
                 w.band = photobiologyWavebands::Red(std = std),
                 naming = naming,
                 ...)
  ratio <- q_R / (q_R + q_FR)
  if (naming != "none") {
    names(ratio) <-
      paste(names(q_R), ":", names(q_R), "+", names(q_FR), name.tag, sep = "")
  }
  attr(ratio, "radiation.unit") <- "q:q ratio"
  ratio
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
                naming = "short",
                name.tag = "[q:q]",
                ...) {
  q_ratio(spct, Blue(std), Green(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                   naming = "short",
                   name.tag = "[q:q]",
                   ...) {
  q_ratio(spct, UVB(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                   naming = "short",
                   name.tag = "[q:q]",
                   ...) {
  q_ratio(spct, UVB(std), UVA(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                   naming = "short",
                   name.tag = "[q:q]",
                   ...) {
  q_ratio(spct, UVA(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                   naming = "short",
                   name.tag = "[q:q]",
                   ...) {
  q_ratio(spct, UVAlw(std), UV("ISO"),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                     naming = "short",
                     name.tag = "[q:q]",
                     ...) {
  q_ratio(spct, UVAsw(std), UV("ISO"),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                   naming = "short",
                   name.tag = "[q:q]",
                   ...) {
  q_ratio(spct, UV(std), PAR(),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                    naming = "short",
                    name.tag = "[q:q]",
                    ...) {
  q_ratio(spct, UVB(std), PAR(),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                    naming = "short",
                    name.tag = "[q:q]",
                    ...) {
  q_ratio(spct, UVA(std), PAR(),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                    naming = "short",
                    name.tag = "[q:q]",
                    ...) {
  q_ratio(spct, UVA1(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                    naming = "short",
                    name.tag = "[q:q]",
                    ...) {
  q_ratio(spct, UVA2(std), UV(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
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
                    naming = "short",
                    name.tag = "[q:q]",
                    ...) {
  q_ratio(spct, UVA2(std), UVA(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
          ...)
}

#' @rdname photon-ratios
#'
#' @export
#' @examples
#' UVA1_UVA(sun.spct)
#'
UVA1_UVA <- function(spct, std = "CIE",
                     use.cached.mult = FALSE,
                     use.hinges = TRUE,
                     naming = "short",
                     name.tag = "[q:q]",
                     ...) {
  q_ratio(spct, UVA1(std), UVA(std),
          use.cached.mult = use.cached.mult,
          use.hinges = use.hinges,
          quantity = "total",
          naming = naming,
          name.tag = name.tag,
          ...)
}
