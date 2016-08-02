#' @title Spectral data for 'Solidago altissima' leaves
#'
#' @description A dataset containing for wavelengths at a 1 nm interval in the
#'   range 350 to 1000 nm, tabulated values for total reflectance and total
#'   transmittance, for the upper and lower epidermis of one leaf from the upper
#'   part of a shoot and another one from the lower part.
#'
#' The variables in each spectrum are as follows:
#'
#' \itemize{
#'   \item w.length (nm)
#'   \item Rfr
#'   \item Tfr
#' }
#'
#' @docType data
#' @keywords datasets
#' @format \code{object_mspct} collection object with four \code{object_spct}
#' member objects, each with 651 rows and 3 variables
#'
#' @references Noda H. 'Reflectance and transmittance spectra of leaves and
#' shoots of 22 vascular plant species and reflectance spectra of trunks and
#' branches of 12 tree species in Japan' ERDP-2013-02.1.1
#' (http://db.cger.nies.go.jp/JaLTER/metacat/metacat/ERDP-2013-02.1.1/jalter-en)\cr
#' JaLTER, Japan Long Term Ecological Research Network,
#' \url{http://www.jalter.org/}\cr
#'
#' @note We thank H. M. Noda for allowing us to include these data in our
#'   package. We have included here only data for two leaves from one species
#'   (Solidago altissima) and for wavelengths shorter than 1000 nm, from the
#'   much larger original data set. The whole data set is publicly available and
#'   the data easy to read into R. The data included here where measured with a
#'   Li-Cor LI-1800 spectroradiometer equipped with a LI-1800-12 (Li-Cor)
#'   integrating sphere, and consequently are for total reflectance and total
#'   transmittance. Further details on methods are available through the JaLTER
#'   web site. If you use these data in a publication, please cite the original
#'   source as given under references and contact the original author.
#'
"Solidago_altissima.mspct"

#' @title Spectral data for 'Betula ermanii' leaves
#'
#' @description A dataset containing for wavelengths at a 1 nm interval in the
#'   range 350 to 1000 nm, tabulated values for total reflectance and total
#'   transmittance, for the upper and lower epidermis of leaves of different
#'   ages from 'Betula ermanii' trees growing in the forest in Japan.
#'
#' The variables in each spectrum are as follows:
#'
#' \itemize{
#'   \item w.length (nm)
#'   \item Rfr
#'   \item Tfr
#' }
#'
#' @docType data
#' @keywords datasets
#' @format \code{object_mspct} collection object with six \code{object_spct}
#' member objects, each with 651 rows and 3 variables
#'
#' @references Noda H. 'Reflectance and transmittance spectra of leaves and
#' shoots of 22 vascular plant species and reflectance spectra of trunks and
#' branches of 12 tree species in Japan' ERDP-2013-02.1.1
#' (http://db.cger.nies.go.jp/JaLTER/metacat/metacat/ERDP-2013-02.1.1/jalter-en)\cr
#' JaLTER, Japan Long Term Ecological Research Network,
#' \url{http://www.jalter.org/}\cr
#'
#' @note We thank H. M. Noda for allowing us to include these data in our
#'   package. We have included here only data for two leaves from one species
#'   (Solidago altissima) and for wavelengths shorter than 1000 nm, from the
#'   much larger original data set. The whole data set is publicly available and
#'   the data easy to read into R. The data included here where measured with a
#'   Li-Cor LI-1800 spectroradiometer equipped with a LI-1800-12 (Li-Cor)
#'   integrating sphere, and consequently are for total reflectance and total
#'   transmittance. Further details on methods are available through the JaLTER
#'   web site. If you use these data in a publication, please cite the original
#'   source as given under references and contact the original author.
#'
"Betula_ermanii.mspct"
