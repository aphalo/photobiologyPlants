
# photobiologyPlants <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-last-release/photobiologyPlants)](https://cran.r-project.org/package=photobiologyPlants)
[![cran
checks](https://badges.cranchecks.info/worst/photobiologyPlants.svg)](https://cran.r-project.org/web/checks/check_results_photobiologyPlants.html)
[![R Universe
vwersion](https://aphalo.r-universe.dev/badges/photobiologyPlants)](https://aphalo.r-universe.dev/photobiologyPlants)
[![R build
status](https://github.com/aphalo/photobiologyPlants/workflows/R-CMD-check/badge.svg)](https://github.com/aphalo/photobiologyPlants/actions)
<!-- badges: end -->

Package ‘**photobiologyPlants**’ provides pre-defined functions for
quantifying visible (‘VIS’), near infra-red (‘NIR’) and ultraviolet
(‘UV’) radiation in relation to their effects on plants together with
action spectra for photosynthesis and absorbance spectra for the plant
photoreceptors in families ‘phytochromes’, ‘cryptochromes’, ‘zeitlupe
proteins’, ‘phototropins’ and ‘UVR8s’ which are present in plants. It
also includes data sets on the optical properties of plant organs,
photosynthesis and plant pigments, chlorophylls and carotenoids. All
data are derived from the scientific literature. Please, see the help
pages for the different data sets for details about the sources used.

The data in this package are stored in objects of classes defined in
package ‘**photobiology**’ which are mostly backwards compatible with
data frames.

This package is part of a suite of R packages for photobiological
calculations described at the
[r4photobiology](https://www.r4photobiology.info) web site.

## Examples

``` r
library(ggspectra)
library(photobiologyPlants)
```

We plot the action spectrum of photosynthesis in Oats.

``` r
comment(McCree_photosynthesis.mspct$oats)
#> [1] "One of the 'classical' action spectra of K. J. McCree (1972) Avena sativa L. var. Coronado."
```

``` r
class(McCree_photosynthesis.mspct$oats)
#> [1] "response_spct" "generic_spct"  "tbl_df"        "tbl"          
#> [5] "data.frame"
```

``` r
autoplot(McCree_photosynthesis.mspct$oats, 
         annotations = c("+", "title:none:what"))
```

![](man/figures/README-example1a-1.png)<!-- -->

We can calculate the R:FR photon ratio of a spectrum, in this case an
example solar spectrum at ground level from package ‘photobiology’.

``` r
R_FR(sun.spct)
#> R:FR[q:q] 
#>  1.266704 
#> attr(,"radiation.unit")
#> [1] "q:q ratio"
```

We can also estimate the photo-equilibrium of phytochrome exposed *in
vitro* to the same spectrum.

``` r
Pfr_Ptot(sun.spct)
#> [1] 0.68341
```

## Installation

Installation of the most recent released version from CRAN (source and
binaries available):

``` r
install.packages("photobiologyLamps")
```

Installation of the current unstable version from R-Universe CRAN-like
repository (source and binaries available):

``` r
install.packages('photobiologySun', 
                 repos = c('https://aphalo.r-universe.dev', 
                           'https://cloud.r-project.org'))
```

Installation of the current unstable version from GitHub (only source
available):

``` r
# install.packages("remotes")
remotes::install_github("aphalo/photobiologylamps")
```

## Documentation

HTML documentation is available at
(<https://docs.r4photobiology.info/photobiologyPlants/>), including a
*User Guide*.

News on updates to the different packages of the ‘r4photobiology’ suite
are regularly posted at (<https://www.r4photobiology.info/>).

Two articles introduce the basic ideas behind the design of the suite
and describe its use: Aphalo P. J. (2015)
(<https://doi.org/10.19232/uv4pb.2015.1.14>) and Aphalo P. J. (2016)
(<https://doi.org/10.19232/uv4pb.2016.1.15>).

A book is under preparation, and the draft is currently available at
(<https://leanpub.com/r4photobiology/>).

A handbook written before the suite was developed contains useful
information on the quantification and manipulation of ultraviolet and
visible radiation: Aphalo, P. J., Albert, A., Björn, L. O., McLeod, A.
R., Robson, T. M., & Rosenqvist, E. (Eds.) (2012) Beyond the Visible: A
handbook of best practice in plant UV photobiology (1st ed., p. xxx +
174). Helsinki: University of Helsinki, Department of Biosciences,
Division of Plant Biology. ISBN 978-952-10-8363-1 (PDF),
978-952-10-8362-4 (paperback). PDF file available from
(<https://hdl.handle.net/10138/37558>).

## Contributing

Pull requests, bug reports, and feature requests are welcome at
(<https://github.com/aphalo/photobiologyPlants>).

## Citation

If you use this package to produce scientific or commercial
publications, please cite according to:

``` r
citation("photobiologyPlants")
#> To cite package ‘photobiologyPlants’ in publications use:
#> 
#>   Aphalo, Pedro J. (2015) The r4photobiology suite. UV4Plants Bulletin,
#>   2015:1, 21-29. DOI:10.19232/uv4pb.2015.1.14
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     author = {Pedro J. Aphalo},
#>     title = {The r4photobiology suite},
#>     journal = {UV4Plants Bulletin},
#>     volume = {2015},
#>     number = {1},
#>     pages = {21-29},
#>     year = {2015},
#>     doi = {10.19232/uv4pb.2015.1.14},
#>   }
```

## License

© 2015-2024 Pedro J. Aphalo (<pedro.aphalo@helsinki.fi>). Released under
the GPL, version 2 or greater. This software carries no warranty of any
kind.
