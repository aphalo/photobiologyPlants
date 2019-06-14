#' Absorbance spectra for chlorophylls.
#'
#' Optical absorption spectra of chlorophyll a in methanol and chlorophylls
#' a and by in diethyl ether containing the wavelengths at 1 nm interval.
#'
#' The variables of the member spectra are as follows:
#'
#' \itemize{ \item w.length (nm) \item A (spectral absorbance) }
#'
#' @details Data from PhotochemCAD 2.1a has been munged on 2 June 2017 by Scott
#'   Prahl (\url{https://omlc.org/}) to make the information available to
#'   non-Windows users. Although he has tried to be as careful as possible, he
#'   may have introduced some error; the cautious user is advised to compare
#'   these results with the original sources (Du et al., 1998; Dixon et al., 2005).
#'
#' The spectral absorption measurements of chlorophyll a in methanol,
#' chlorophyll a and chlorophyll b in diethyl ether were made by J. Li on
#' 12-11-1997 using a Cary 3. The absorption values were collected using a
#' spectral bandwidth of 1.0 nm, a signal averaging time of 0.133 sec, a data
#' interval of 0.25 nm, and a scan rate of 112.5 nm/min.
#'
#' Chlorophyll a measurements were scaled to make the molar extinction
#' coefficient match the value of 111700 cm-1/M at 417.8 nm.  These values were
#' then interpolated to report extinction coefficients at regular 1nm intervals.
#' The reported molar extinction coefficient is from Strain et al. (1963).
#'
#' Chlorophyll b measurements were scaled to make the molar extinction
#' coefficient match the value of 159100 cm-1/M at 453.0 nm.  These values were
#' then interpolated to report extinction coefficients at regular 1nm intervals.
#' The reported molar extinction coefficient is from Vernon and Seely (1966).
#'
#' @docType data
#' @keywords datasets
#' @format A \code{filter_mspct} with three member \code{filter_spct} objects
#'   each with variable number of rows and 2 numeric variables, \code{w.length}
#'   and \code{A}
#'
#' @name chlorophylls.mspct
#'
#' @references J. M. Dixon, M. Taniguchi and J. S. Lindsey "PhotochemCAD 2. A
#' refined program with accompanying spectral databases for photochemical
#' calculations", Photochem. Photobiol., 81, 212-213, 2005.
#'
#' H. Du, R. A. Fuh, J. Li, A. Corkan, J. S. Lindsey, "PhotochemCAD: A
#' computer-aided design and research tool in photochemistry," Photochem.
#' Photobiol., 68, 141-142, 1998.
#'
#' Strain, H. H., M. R. Thomas and J. J. Katz (1963) Spectral absorption
#' properties of ordinary and fully deuteriated chlorophylls a and b.  Biochim.
#' Biophys. Acta 75, 306-311.
#'
#' Vernon, L. P. and G. R. Seely (1966) The chlorophylls.  Academic Press, NY.
#'
#' @note If you use these data in a publication, please cite also the original
#'   sources as given under references. For more information please visit
#'   \url{https://omlc.org/}.
#'
#' @examples
#' names(chlorophylls.mspct)
#' getWhatMeasured(chlorophylls.mspct[[1]])
#'
NULL
