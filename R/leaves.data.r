#' @title Spectral data for leaves
#'
#' @description A dataset containing the wavelengths at a 1 nm interval for the range 350 to 1000 nm.
#' Tabulated values for total reflectance and total tarnsmittance.
#'
#' The variables are as follows:
#'
#' \itemize{
#'   \item w.length (nm)
#'   \item Rfr
#'   \item Tfr
#' }
#'
#' @docType data
#' @keywords datasets
#' @format \code{object.spct} with 651 rows and 3 variables
#' @name leaves
#' @references
#' Noda H. 'Reflectance and transmittance spectra of leaves and shoots of 22 vascular plant species
#' and reflectance spectra of trunks and branches of 12 tree species in Japan'
#' ERDP-2013-02.1.1 (http://db.cger.nies.go.jp/JaLTER/metacat/metacat/ERDP-2013-02.1.1/jalter-en)\cr
#' JaLTER, Japan Long Term Ecological Research Network, \url{http://www.jalter.org/}\cr
#'
#' @note We thank H. Noda for allowing us to include these data in our package. We have included here
#' only data for two leaves from one species (Solidago altissima) and for wavelengths shorter than 1000 nm,
#' from the much larger original data set. The whole data set
#' is publicly available and the data easy to read into R. The data included here where measured with
#' a Li-Cor LI-1800 spectroradiometer equipped with a LI-1800-12 (Li-Cor) integrating sphere, and
#' consequently are for total reflectance and total transmittance. Further details on methods are
#' available through the JaLTER web site.
#' If you use these data in a publication, please
#' cite the original source as given under references and contact the original author.
#'
#' @aliases Solidago_lower_adax.spct Solidago_lower_abax.spct Solidago_upper_adax.spct Solidago_upper_abax.spct
#'
NULL
