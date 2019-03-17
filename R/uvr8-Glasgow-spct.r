#' UVR8 absorbance spectrum
#'
#' A dataset containing the wavelengths at an arbitrary nm interval. Tabulated
#' values for the in vitro absorbance spectrum of UVR8.
#'
#' The variables are as follows:
#'
#' \itemize{ \item w.length (nm) \item A (spectral absorbance) }
#'
#' @docType data
#' @keywords datasets
#'
#' @format A \code{filter_mspct} object with one member \code{filter_spct} objects
#'   with 300 rows and 2 numeric variables, \code{w.length} and \code{A}
#'
#' @name UVR8s.mspct
#'
#' @references Christie, J. M., A. S. Arvai, K. J. Baxter, M. Heilmann, A. J.
#' Pratt, A. O'Hara, S. M. Kelly, M. Hothorn, B. O. Smith, K. Hitomi, et al.
#' (2012). Plant UVR8 photoreceptor senses UV-B by tryptophan-mediated
#' disruption of cross-dimer salt bridges. In: Science (New York, N.Y.)
#' 335.6075, pp. 1492-1496. DOI: 10.1126/science.1218091. (Figure S3)
#'
#' @note If you use these data in a publication, please cite also the original
#'   source as given under references.
#'
#' @examples
#' names(UVR8s.mspct)
#' getWhatMeasured(UVR8s.mspct[[1]])
#'
NULL
