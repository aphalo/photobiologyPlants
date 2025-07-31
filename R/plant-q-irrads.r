#' Constrained extended PAR from spectral irradiance
#'
#' Compute the constrained extended photosynthetically active radiation (xPAR)
#' photon irradiance and its components ePAR, PAR and FR.700.750.
#'
#' @param spct an object of class "source.spct".
#' @param w.band a waveband object or a list of waveband objects with additional
#'   waveband definitions for which to compute photon irradiance.
#' @param time.unit character or lubridate::duration object.
#' @param scale.factor numeric Multiplier applied to returned value.
#' @param use.cached.mult logical indicating whether multiplier values should be
#'   cached between calls.
#' @param use.hinges logical indicating whether to use hinges to reduce
#'   interpolation errors.
#' @param ... ignored.
#'
#' @details PAR is defined by a very simple biological spectral weighting
#'   function (BSWF) giving equal action per photon in the range 400 nm to 700
#'   nm. Radiation in the range 700 to 750 nm has a synergistic effect on the
#'   photosynthesis rate as long as it is present in addition to PAR. This
#'   synergism is called Emerson's effect. An alternative definition, ePAR, was
#'   proposed by Bugbee and Zhen. It uses the same BSWF as PAR but over the
#'   range 400 to 750 nm. Apogee, sells nowadays a sensor able to directly
#'   measure this photon irradiance, type SQ-610-SS ePAR sensor. The limitation
#'   is, as these authors have demonstrated, that when the contribution of FR is
#'   more than 40% of PAR, the excess FR no longer contributes significantly to
#'   photosynthesis. This can be assessed by quantifying both components
#'   separately, either from spectral data or using a sensor with at least two
#'   channels, such as Apogee's S2-141-SS PAR-FAR sensor. This bounded extended
#'   PAR is labelled here xPAR.
#'
#'   Under natural illumination and commonly used plant grow lights, a
#'   difference between unconstrained (ePAR) and constrained (xPAR) extended PAR
#'   is very unlikely to be observed. As xPAR cannot be computed from spectral
#'   irradiance using a single waveband definition or measured with a
#'   single-channel broadband sensor, this function can be used to check under
#'   which conditions ePAR and xPAR irradiances differ.
#'
#'   Methods \code{xPAR_irrad()} return four photon irradiances: ePAR (400-750 nm,
#'   unconstrained), xPAR(400-750 nm, with FR contribution constrained to a
#'   maximum of 40% of PAR), PAR (400-700 nm) and FR (700-750 nm).
#'
#' @return a data.frame with four numeric variables photon irradiances for xPAR,
#'   ePAR, PAR, and the far-red with wavelength 700 to 750 nm. expressed in
#'   \eqn{mol m^{-2} s^{-1}} if \code{scale.factor = 1}, and possibly additional
#'   ones with metadata copied from the spectra. The data frame has one row for
#'   each spectrum in the object passed as argument to formal parameter
#'   \code{spct}.
#'
#' @seealso \code{\link[photobiology]{q_irrad}()} and
#'   \code{\link[photobiologyWavebands]{PAR}()}.
#'
#' @family photosynthesis-related functions and data
#'
#' @references
#' McCree KJ. 1972. Test of current definitions of photosynthetically active
#' radiation against leaf photosynthesis data. Agricultural Meteorology 10,
#' 443-453. \doi{10.1016/0002-1571(72)90045-3}.
#'
#' McCree KJ. 1976. A Rational Approach to Light Measurements in Plant Ecology.
#' In: Smith H, ed. Commentaries in Plant Science. Oxford: Pergamon Press.
#'
#' Zhen S, van Iersel M, Bugbee B. 2021. Why Far-Red Photons Should Be Included
#' in the Definition of Photosynthetic Photons and the Measurement of
#' Horticultural Fixture Efficacy. Frontiers in Plant Science 12.
#' \doi{10.3389/fpls.2021.693445}.
#'
#' Zhen S, van Iersel MW, Bugbee B. 2022. Photosynthesis in sun and
#' shade: the surprising importance of far‐red photons. New Phytologist 236,
#' 538–546. \doi{10.1111/nph.18375}.
#'
#' @name photon irradiances
#' @rdname photon-irradiances
#' @concept extended PAR
#'
#' @export
#' @examples
#' # default with a single spectrum (spectral irradiance)
#' xPAR_irrad(sun.spct) # mol m-2 s-1
#' xPAR_irrad(sun.spct, scale.factor = 1e6) # umol m-2 s-1
#' xPAR_irrad(sun.spct, time.unit = "hour") # mol m-2 h-1
#'
#' # add irradiances for other wavebands
#' xPAR_irrad(sun.spct, scale.factor = 1e6, w.band = UVA("CIE"))
#'
#' # DLI from a daily spectrum (spectral daily integral)
#' summary(sun_daily.spct)
#' xPAR_irrad(sun_daily.spct) # mol m-2 d-1
#'
#' # multiple spectra
#' xPAR_irrad(sun_evening.spct, scale.factor = 1e6)
#'
#' # multiple spectra as a collection
#' xPAR_irrad(sun_evening.mspct, scale.factor = 1e6)
#'
#' # copy metadata from the spectra, see help(q_irrad)
#' xPAR_irrad(sun_evening.mspct,
#'            scale.factor = 1e6,
#'            attr2tb = c("lon", "lat", "when.measured"))
#'
#' @export
#'
xPAR_irrad <-
  function(spct,
           w.band,
           time.unit,
           scale.factor,
           use.cached.mult,
           use.hinges,
           ...) UseMethod("xPAR_irrad")

#' @rdname photon-irradiances
#'
#' @export
#'
xPAR_irrad.default <-
  function(spct,
           w.band,
           time.unit,
           scale.factor,
           use.cached.mult,
           use.hinges,
           ...) {
  warning("'xPAR_irrad()' is not defined for objects of class \"",
          class(spct)[1], "\"")
  return(NA)
}

#' @rdname photon-irradiances
#'
#' @export
#'
xPAR_irrad.source_spct <-
  function(spct,
           w.band = list(),
           time.unit = NULL,
           scale.factor = 1,
           use.cached.mult = getOption("photobiology.use.cached.mult",
                                       default = FALSE),
           use.hinges = NULL,
           ...) {
    if (is.null(time.unit)) {
      time.unit <- attr(spct, which = "time.unit", exact = TRUE)
    }
    if (is.waveband(w.band)) {
      w.band <- list(w.band)
    }
    w.bands <- list(photobiologyWavebands::PAR("ePAR"),
                    photobiologyWavebands::PAR(),
                    photobiology::waveband(c(700, 750),
                                           hinges = NULL,
                                           wb.name = "FR.700.750"))
    w.bands <- c(w.bands, w.band)
    z <-
      photobiology::q_irrad(spct = spct,
                            w.band = w.bands,
                            quantity = "total",
                            time.unit = time.unit,
                            scale.factor = scale.factor,
                            wb.trim = "TRUE",
                            use.hinges = use.hinges,
                            allow.scaled = FALSE,
                            return.tb = TRUE)
    time.unit.attr <- attr(z, "time.unit")
    radiation.unit.attr <- attr(z, "radiation.unit")
    Q_xPAR <- with(z, ifelse((Q_FR.700.750 / Q_PAR) > 0.4,
                             Q_PAR * 1.4,
                             Q_PAR + Q_FR.700.750))
    z <- cbind(Q_xPAR, z)
    attr(z, "time.unit") <- time.unit.attr
    attr(z, "radiation.unit") <- radiation.unit.attr
    z
  }

#' @rdname photon-irradiances
#'
#' @param attr2tb character vector, see \code{\link[photobiology]{add_attr2tb}}
#'   for the syntax for \code{attr2tb} passed as is to formal parameter
#'   \code{col.names}.
#' @param idx character Name of the column with the names of the members of the
#'   collection of spectra.
#' @param .parallel	if TRUE, apply function in parallel, using parallel backend
#'   provided by foreach
#' @param .paropts a list of additional options passed into the foreach function
#'   when parallel computation is enabled. This is important if (for example)
#'   your code relies on external data or packages: use the .export and
#'   .packages arguments to supply them so that all cluster nodes have the
#'   correct environment set up for computing.
#'
#' @export
#'
xPAR_irrad.source_mspct <-
  function(spct,
           w.band = list(),
           time.unit = NULL,
           scale.factor = 1,
           use.cached.mult = getOption("photobiology.use.cached.mult",
                                       default = FALSE),
           use.hinges = NULL,
           ...,
           attr2tb = NULL,
           idx = "spct.idx",
           .parallel = FALSE,
           .paropts = NULL) {

    spct <-
      photobiology::subset2mspct(spct) # expand long form spectra in collection

    z <-
      photobiology::msdply(
        mspct = spct,
        .fun = xPAR_irrad,
        w.band = w.band,
        time.unit = time.unit,
        scale.factor = scale.factor,
        use.cached.mult = use.cached.mult,
        use.hinges = use.hinges,
        idx = idx,
        .parallel = .parallel,
        .paropts = .paropts
      )

    colnames(z) <- gsub("xPAR_irrad_", "", colnames(z))

    photobiology::add_attr2tb(tb = z,
                              mspct = spct,
                              col.names = attr2tb,
                              idx = idx)
  }
