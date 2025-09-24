
photobiologyPlants 0.6.1
========================

In recent years a few new photon ratios and irradiances have been proposed to
describe light spectra in relation to plants responses. This update adds 
functions to easily compute them.

* Track enhancements in packages 'photobiology' (>= 0.14.0) and 
'photobiologyWavebands' (>= 0.5.3).
* Add parameter `na.rm` and checks for NAs and low wavelength resolution to the 
`Pfr_Ptot()` method for `source_spct` objects.
* Add methods `xPAR_irrad()` to compute extended photosynthetically active 
radiation photon irradiance in constrained (xPAR) and unconstrained (ePAR), and 
their PAR and far-red (FR) components.
* Add function `FR_RpFR()`, `R_RpFR()`, `FR_PAR()`, `FR_ePAR()`, `UVA1_UVA()`, 
`UVA2_UVA()` and update all functions for computing photon
ratios to forward additional named arguments to `q_ratio()`.
* _Breaking:_ Change default in `R_FR()` from `"Smith10"` into `"Smith20`".
* Collect the documentation of all functions for computing different photon
ratios into a single help page. 

photobiologyPlants 0.6.0
========================

* Add wheat leaf fluorescence emission spectrum data.
* Rebuild data objects with photobiology 0.11.4.
* Functions migrated from package 'photobiology' to this package: see help for
`water_vp_sat()`, `ET_ref()` and `net_irradiance()`.

photobiologyPlants 0.5.0
========================

* Add `UVB_UVA()`.
* Rebuild data objects with photobiology 0.11.2.

photobiologyPlants 0.4.3
========================

* Add a second measured absorbance spectrum for UVR8, extending into longer
  wavelengths.
* Add `UVA1_UV()`, `UVA2_UV()`, `UVAlw_UV()` and `UVAsw_UV()`.
* Rebuild data objects with photobiology 0.10.16.
* The repository with the source of the package was relocated to GitHub.

photobiologyPlants 0.4.2
========================

* CODE BREAKING: Some data objects have been renamed and consequently old
  scripts may need minor edits.
* DATA CHANGES: Spectral data has increased wavelength resolution.
* Add spectral absorbance data for cryptochromes 1 and 3, phototropins 1
  and 2, ZTL protein, eight carotenoids and chlorophylls a and b.
* Add spectral fluorescence emission data for chlorophylls a and b.
* Translate vignette to R markdown and update it.
* Fix bug in Jalter leaf optical properties data object.
* Rebuild data objects with photobiology 0.9.28.

photobiologyPlants 0.4.1
========================

Rebuild data objects with photobiology 0.9.14. Updating data build script to
fix wrong attribute of exported `object_spct` data objects.

photobiologyPlants 0.4.0
========================

* Rebuild data objects with photobiology 0.9.9, after updating scripts to create
collections of spectra instead of separate objects for related data sets.
* Add functions for frequently used photon ratios.
* Remove some redundant phytochrome-related functions.

photobiologyPlants 0.3.3
========================

Compatibility update for photobiology 0.9.1 and ggplot2 2.0.0. Use ggspectra
instead of photobiologygg.

photobiologyPlants 0.3.2
========================

* Set spectral absorbance data for photoreceptors as of type "internal".
* Fix minor error in UVR8 data.
* Rebuild all data and the package under photobiology 0.8.5.

photobiologyPlants 0.3.1
========================

* Rebuild all data and the package under photobiology 0.8.0.
* Remove dependency on 'data.table'.

photobiologyPlants 0.3.0
========================

* Change naming of phytochrome related functions, which were all misnamed!
* Rebuild all data and the package under photobiology 0.6.0.

photobiologyPlants 0.1.2
========================

* Update versions of dependencies.
* New generic function `Pr_Ptot()` for phytochrome photoequiibrium with methods
for numeric values of wavelengths and source.spct objects.
* Updated phytochrome chapter of User Guide.

photobiologyPlants 0.1.1
========================

* Add JaLTER additional data for spectral properties of leaves.
* Rebuild JaLTER data as it was missing the `Rfr.type` attribute.

photobiologyPlants 0.1.0
========================

* Remove all waveband objects as it was confusing to have waveband objects 
and functions for spectral absorbance data.
* Rebuild all data as `.spct objects`.
* Edit the vignettes.

photobiologyPlants 0.0.3
========================

* Add JaLTER data for spectral properties of leaves.

photobiologyPlants 0.0.2
========================

* Add McCree's action spectra for photosynthesis in oat and amaranth as 
response.spct objects.
* Now the absorbance spectra of UVR8 and CRY2 are included as `filter.spct` 
objects.

photobiologyPlants 0.0.1
========================

New package created by merging the CRY and PHY related packages, and adding 
UVR8 functions and data.
Vignettes were edited, but only the User Guide was merged.

----------

photobiologyCry 0.1.3
=====================

Minute update to vignette.

photobiologyCry 0.1.2
=====================

Added missing import directive.

photobiologyCry 0.1.1
=====================

Added vignettes.

photobiologyCry 0.1.0
=====================

First release. Only CRY2.

----------

photobiologyPhy 0.2.5
=====================

Now cache is created in `emptyenv()`.

photobiologyPhy 0.2.4
=====================

Updated to adjust code to changes introduced in photobiology 0.2.16.

photobiologyPhy 0.2.3
=====================

New function Phy_reaction_rates added.

photobiologyPhy 0.2.2
=====================

Bumped required version of photobiology package.
