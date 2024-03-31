#' Fluorescence emission spectra for chlorophylls.
#'
#' Optical absorption spectra of chlorophyll \eqn{a} in methanol and chlorophylls \eqn{a}
#' and \eqn{b} in diethyl ether containing the wavelengths at 1 nm interval.
#'
#' The variables of the member spectra are as follows:
#'
#' \itemize{ \item w.length (nm) \item A (spectral absorbance) }
#'
#' @details Data from PhotochemCAD 2.1a has been munged on 2 June 2017 by Scott
#'   Prahl (\url{https://omlc.org/}) to make the information available to
#'   non-Windows users. Although he has tried to be as careful as possible, he
#'   may have introduced some error; the cautious user is advised to compare
#'   these results with the original sources at
#'   \url{https://www.photochemcad.com/} (Du et al., 1998; Dixon et al., 2005).
#'
#'   Fluorescence emission was measured using a Spex FluoroMax. The excitation
#'   and emission monochromators were set at 1 mm, giving a spectral bandwidth
#'   of 4.25 nm.  The data interval was 0.5 nm and the integration time was 2.0
#'   sec. Samples were prepared in 1cm path length quartz cells with absorbance
#'   less than 0.1 at the excitation and all emission wavelengths to uniformly
#'   illuminate across the sample, and to avoid the inner-filter effect. The
#'   dark counts were subtracted and the spectra were corrected for
#'   wavelength-dependent instrument sensitivity.
#'
#' @docType data
#' @keywords datasets
#' @format A \code{filter_mspct} with three member \code{filter_spct} objects
#'   each with variable number of rows and 2 numeric variables, \code{w.length}
#'   and \code{A}
#'
#' @name chlorophylls_fluorescence.mspct
#'
#' @references J. M. Dixon, M. Taniguchi and J. S. Lindsey "PhotochemCAD 2. A
#'   refined program with accompanying spectral databases for photochemical
#'   calculations", Photochem. Photobiol., 81, 212-213, 2005.
#'
#'   H. Du, R. A. Fuh, J. Li, A. Corkan, J. S. Lindsey, "PhotochemCAD: A
#'   computer-aided design and research tool in photochemistry," Photochem.
#'   Photobiol., 68, 141-142, 1998.
#'
#' @note If you use these data in a publication, please cite also the original
#'   sources as given under references. For more information please visit
#'   \url{https://omlc.org/}.
#'
#' @examples
#' names(chlorophylls_fluorescence.mspct)
#' getWhatMeasured(chlorophylls_fluorescence.mspct[[1]])
#'
NULL
