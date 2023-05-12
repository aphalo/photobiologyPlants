#' Absorbance spectra for carotenoids.
#'
#' A dataset containing the wavelengths at an arbitrary nm interval. Tabulated
#' values for the in vitro absorbance spectrum of beta-carotene, lutein, lycopene,
#' 3-4,di-hydro-lycopene, phytoene, phytofluene, violaxanthin and zeaxanthin.
#' Data were digitized from plots downloaded from LipidBase
#' (\url{https://lipidbank.jp/}), The official database of Japanese Conference
#' on the Biochemistry of Lipids (JCBL). Data contributed to LipinBank by
#' Takaichi Sinichi.
#'
#' The variables of the member spectra are as follows:
#'
#' \itemize{ \item w.length (nm) \item A (spectral absorbance) }
#'
#' @docType data
#' @keywords datasets
#' @format A \code{filter_mspct} with eight member \code{filter_spct} objects each
#'   with 300 rows and 2 numeric variables, \code{w.length} and \code{A}
#'
#' @name carotenoids.mspct
#'
#' @references
#' Watanabe K., Yasugi E. and Oshima M. "How to search the glycolipid data in
#'   LIPIDBANK for Web: the newly developed lipid database" Japan Trend
#'   Glycosci. and Glycotechnol. 12, 175-184, 2000.
#'
#' @note If you use these data in a publication, please cite also the original
#'   source as given under references.
#'
#' @examples
#' names(carotenoids.mspct)
#' getWhatMeasured(carotenoids.mspct[[1]])
#'
#'
NULL
