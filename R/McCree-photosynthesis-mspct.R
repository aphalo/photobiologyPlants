#' @title
#' McCree's action spectra for whole-leaf photosynthesis.
#'
#' @description The 'classical' action spectra of K. J. McCree (1972) for
#'   \emph{Amaranthus edulis} Speg. var. UCD 1966 and \emph{Avena sativa} L.
#'   var. Coronado are included in this data set. Response is net \eqn{CO_2}
#'   uptake measured on leaf sections umder monochromatic light. The light
#'   source used was a xenon-arc lamp fitted with a monochromator. Irradiance
#'   was in the range 10 to 15 \eqn{W m^{-2}}.
#'
#' @references
#' McCree, K. J. (1972) Significance of Enhancement for Calculations
#'   Based on the Action Spectrum for Photosynthesis. Plant Physiology, 49,
#'   704-706. Fig. 1, AMARANTH.
#'
#' @name McCree_photosynthesis.mspct
#' @note Digitised from bitmap of from the original publication.
#' @docType data
#' @keywords datasets
#'
#' @format A \code{\link[photobiology]{response_mspct}} object with two member
#'   \code{\link[photobiology]{response_spct}}
#'   objects each with 300 rows and 2 numeric variables, \code{w.length} and
#'   \code{s.e.response}.
#'
#' @family photosynthesis-related functions and data
#'
#' @note If you use these data in a publication, please cite also the original
#'   source as given under references.
#'
#' @examples
#' summary(McCree_photosynthesis.mspct)
#'
NULL
