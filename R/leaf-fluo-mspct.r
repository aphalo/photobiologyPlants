#' Fluorescence emission spectra of leaves.
#'
#' Fluroescence spectra of whole leaves. Wheat excited with low irradiance of
#' UVA1 radiation at 355 nm. Fluorescence state of chlorophylls equivalent to
#' \eqn{F_0}.
#'
#' The variables of the member spectra are as follows:
#'
#' \itemize{ \item w.length (nm) \item s.e.irrad (QSEU) }
#'
#' @details Data for spectrum \code{wheat_Fo_ex355nm} from Meyer et al. (2003, Fig. 2A).
#'    The fluorescence emission is expressed in quinine sulphate equivalent units (QSEU).
#'    Data were obtained by digitizing the figure in the publication and extracting
#'    the data with DigitizeIt under Windows 11.
#'
#' @docType data
#' @keywords datasets
#' @format A \code{source_mspct} with one member \code{source_spct} object.
#'   each with variable number of rows and 2 numeric variables, \code{w.length}
#'   and \code{s.e.irrad}
#'
#' @name leaf_fluorescence.mspct
#'
#' @references
#' Meyer et al. (2003) UV-induced blue-green and far-red fluorescence along
#' wheat leaves: a potential signature of leaf ageing. Journal of Experimental
#' Botany, 54: 757-769. \doi{10.1093/jxb/erg063}.
#'
#' @note If you use these data in a publication, please cite also the original
#'   sources as given under references.
#'
#' @examples
#' names(leaf_fluorescence.mspct)
#' what_measured(leaf_fluorescence.mspct)
#'
NULL
