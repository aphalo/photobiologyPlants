#' Coefficients for redox state of the plastoquinone pool
#'
#' The variables are as follows:
#'
#' \itemize{
#'   \item w.length (nm)
#'   \item k
#' }
#' where \code{k} contains fitted coefficients of a model for estimating the
#' redox state of the plastoquinone pool (Mattila et al., 2020). Based on
#' measurements in \emph{Arabidopsis} plants. If used in a publication, please,
#' cite both Mattila et al. (2020) and this R package.
#'
#' @docType data
#' @keywords datasets
#' @format \code{generic_spct} object with 351 rows and 2 variables.
#'
#' @seealso \code{\link{PQ_redox_state}()}
#'
"PS1_PS2_k.spct"

#' Redox state of the plastoquinone pool
#'
#' Estimate the redox state of the plastoquinone (PQ) pool from spectral
#' irradiance.
#'
#' @param light.spct A \code{source_spct} object or a \code{source_mspct}
#'   object.
#' @param coefs.spct A \code{generic_spct} object containing the coefficients of
#'   the fitted model. Defaults to those in Mattila et al. (2020).
#' @param w.band A \code{waveband} object setting the waveband used for scaling
#'   each the spectral irradiance(s) in \code{light.spct}. Defaults to PAR as
#'   used in Mattila et al. (2020).
#' @param force.to.range logical If \code{TRUE}, convert negative values into
#'   zero.
#' @param return.tb logical If \code{TRUE} force return of a data frame for a
#'   single spectrum, to match the returned class for collections of spectra.
#' @param attr2tb character vector, see \code{\link[photobiology]{add_attr2tb}}
#'  the syntax for \code{attr2tb} passed as is to formal parameter \code{col.names}.
#' @param idx character Name of the column with the names of the members of the
#'   collection of spectra.
#' @param .parallel	if TRUE, apply function in parallel, using parallel backend
#'   provided by foreach
#' @param .paropts a list of additional options passed into the foreach function
#'   when parallel computation is enabled. This is important if (for example)
#'   your code relies on external data or packages: use the .export and
#'   .packages arguments to supply them so that all cluster nodes have the
#'   correct environment set up for computing.
#' @param ... currently ignored.
#'
#' @details The computations follow the procedure in Mattila et al. (2020). The
#'   spectrum in \code{light.spct} is re-expressed by interpolation to the
#'   wavelengths from \code{coefs.spct}. As in Mattila et al. (2020) the state
#'   is expressed as percent of the PQ pool that is reduced. If the result of
#'   the computation is used in a publication, please, cite both Mattila et al.
#'   (2020) and this R package.
#'
#' @return A numeric vector, with one value for each spectrum in the input.
#'
#' @export
#'
#' @references
#' Mattila H, Khorobrykh S, Hakala-Yatkin M, Havurinne V, Kuusisto I, Antal T,
#' Tyystjärvi T, Tyystjärvi E. 2020. Action spectrum of the redox state of the
#' plastoquinone pool defines its function in plant acclimation. The Plant
#' Journal 104, 1088–1104. \doi{10.1111/tpj.14983}.
#'
#' @examples
#'
#' PQ_redox_state(sun.spct) # PAR is default for scaling
#' PQ_redox_state(sun.spct, w.band = c(370, 730)) # scaling with wavelength range
#' PQ_redox_state(sun.spct, attr2tb = "what.measured")
#' PQ_redox_state(white_led.source_spct)
#' PQ_redox_state(sun_evening.mspct)
#' PQ_redox_state(sun_evening.mspct, attr2tb = "when.measured")
#' PQ_redox_state(sun_evening.spct)
#'
PQ_redox_state <- function(light.spct,
                           coefs.spct = photobiologyPlants::PS1_PS2_k.spct,
                           w.band = photobiologyWavebands::PAR(),
                           force.to.range = TRUE,
                           ...) UseMethod("PQ_redox_state")

#' @rdname PQ_redox_state
#'
#' @export
#'
PQ_redox_state.default <-
  function(light.spct,
           coefs.spct = photobiologyPlants::PS1_PS2_k.spct,
           w.band = photobiologyWavebands::PAR(),
           force.to.range = TRUE,
           ...) {
    warning("'PQ_redox_state' is not defined for objects of class ",
            class(light.spct)[1])
    return(NA)
  }

#' @rdname PQ_redox_state
#'
#' @export
#'
PQ_redox_state.source_spct <-
  function(light.spct,
           coefs.spct = photobiologyPlants::PS1_PS2_k.spct,
           w.band = photobiologyWavebands::PAR(),
           force.to.range = TRUE,
           return.tb = !is.null(attr2tb),
           attr2tb = NULL,
           ...) {

    if (return.tb || getMultipleWl(light.spct) > 1L) {
      z <-
        PQ_redox_state(light.spct = photobiology::subset2mspct(light.spct),
                       coefs.spct = coefs.spct,
                       return.tb = TRUE,
                       attr2tb = attr2tb)
    } else {
      interpolated.spct <-
        photobiology::interpolate_spct(
          photobiology::fscale(light.spct,
                               f = photobiology::q_irrad,
                               w.band = w.band,
                               unit.out = "photon"),
          w.length.out = coefs.spct$w.length)
      z <-
        sum(interpolated.spct[["s.q.irrad"]] * coefs.spct[["k"]]) + 50
      if (force.to.range) {
        z <- ifelse(z < 0, 0, z)
        z <- ifelse(z > 100, 100, z)
      }
    }
    z
  }

#' @rdname PQ_redox_state
#'
#' @export
#'
PQ_redox_state.source_mspct <-
  function(light.spct,
           coefs.spct = photobiologyPlants::PS1_PS2_k.spct,
           w.band = photobiologyWavebands::PAR(),
           force.to.range = TRUE,
           return.tb = TRUE,
           attr2tb = NULL,
           idx = "spct.idx",
           ...,
           .parallel = FALSE,
           .paropts = NULL) {

    if (!return.tb & !is.null(attr2tb)) {
      warning("Overriding 'return.tb = FALSE' as '!is.null(attr2tb)'!")
    }

    light.mspct <-
      subset2mspct(light.spct) # expand long form spectra within collection

    z <-
      photobiology::msdply(
        mspct = light.mspct,
        .fun = PQ_redox_state.source_spct,
        coefs.spct = coefs.spct,
        w.band = w.band,
        force.to.range = force.to.range,
        idx = idx,
        col.names = "PQ_redox.state",
        .parallel = .parallel,
        .paropts = .paropts
      )

    if (return.tb) {
      photobiology::add_attr2tb(tb = z,
                                mspct = light.mspct,
                                col.names = attr2tb,
                                idx = idx)
    } else {
      z[["PQ_redox.state"]]
    }
  }
