% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PfrPtot-RFR.R
\name{Pfr_Ptot_R_FR}
\alias{Pfr_Ptot_R_FR}
\title{Pr:Ptot ratio (photoequilibrium) from R:FR photon ratio.}
\usage{
Pfr_Ptot_R_FR(R.FR)
}
\arguments{
\item{R.FR}{R:FR a single value or a vector of photon ratio (dimentionless) values}
}
\value{
a single value or a vector of numeric values giving the Pr:Ptot
  dimensionless ratio
}
\description{
Calculation of Pfr:Ptot ratio for Type I Phytochrome from red:far-red photon
ratio. "Exact" only for dichromatic irradiation, only approximate for R:FR
ratio calculated from a broadband light source.
}
\examples{
Pfr_Ptot_R_FR(1.15)
Pfr_Ptot_R_FR(0.10)
Pfr_Ptot_R_FR(c(0.1,1.15,5.0,20.0))

}
\references{
Mancinelli, A.L. (1994) The physiology of phytochrome action. In
Photomorphogenesis in plants, 2nd edition. R.E. Kendrick and G.H.M.
Kronenberg, eds. Kluwer Academic Publishers, Dordrecht, pp. 211-269. ISBN
978-0-7923-2551-2 (print), 978-94-011-1884-2 (on-line).
\doi{10.1007/978-94-011-1884-2_10}
}
\seealso{
\code{\link[photobiology]{q_ratio}}

Other phytochrome-related functions and data: 
\code{\link{PHYs.mspct}},
\code{\link{Pfr_Ptot}()},
\code{\link{Phy_Sigma}()},
\code{\link{Phy_Sigma_FR}()},
\code{\link{Phy_Sigma_R}()},
\code{\link{Phy_reaction_rates}()}
}
\concept{phytochrome-related functions and data}
