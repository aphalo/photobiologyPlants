## ---- include=FALSE, echo=FALSE-----------------------------------------------
knitr::opts_chunk$set(fig.width=7.2, fig.height=4.3)

## ---- printing-spectra, eval=TRUE, include=FALSE------------------------------
# library(tibble)
options(tibble.print_max = 6, tibble.print_min = 4)

## ----message=FALSE------------------------------------------------------------
library(ggspectra)
library(photobiology)
library(photobiologyPlants)
library(photobiologyWavebands)

theme_set(theme_bw())

## -----------------------------------------------------------------------------
R_FR(sun.spct)

## -----------------------------------------------------------------------------
Pfr_Ptot(sun.spct)

## -----------------------------------------------------------------------------
R_FR(sun.spct)

## -----------------------------------------------------------------------------
q_ratio(sun.spct, Red("Smith10"), Far_red("Smith10"))

## -----------------------------------------------------------------------------
Pfr_Ptot_R_FR(R_FR(sun.spct))

## -----------------------------------------------------------------------------
Pfr_Ptot(660)
Pfr_Ptot(735)
Pfr_Ptot(c(660, 735))
Pfr_Ptot(435)

## -----------------------------------------------------------------------------
autoplot(Pfr_Ptot(300:770), norm = NULL, unit.out = "photon",
         w.band = Plant_bands(),
         annotations = c("colour.guide", "labels", "boxes")) +
  labs(y = "Phytochrome photoequilibrium, Pfr:Ptot ratio")

## -----------------------------------------------------------------------------
ggplot(data = Pfr_Ptot(300:770), aes(w.length, s.q.response)) +
  geom_line() +
  labs(x = "Wavelength (nm)",
       y = "Phytochrome photoequilibrium, Pfr:Ptot ratio")

## -----------------------------------------------------------------------------
Pfr_Ptot_R_FR(1.15)
Pfr_Ptot_R_FR(0.01)
Pfr_Ptot_R_FR(c(1.15,0.01))

## -----------------------------------------------------------------------------
ex6.data <- data.frame(r.fr=seq(0.01, 5.0, length.out=100), Pfr.p=numeric(100))
ex6.data$Pfr.p <- Pfr_Ptot_R_FR(ex6.data$r.fr)
ggplot(data=ex6.data, aes(r.fr, Pfr.p)) +
  geom_line() +
    labs(x ="R:FR photon ratio",
         y = "Phytochrome photoequilibrium, Pfr:Ptot ratio")


## -----------------------------------------------------------------------------
with(clip_wl(sun.spct, c(300,770)), Phy_reaction_rates(w.length, s.e.irrad))

## -----------------------------------------------------------------------------
ex7.data <- data.frame(w.length=seq(300, 770, length.out=100))
ex7.data$sigma.r <- Phy_Sigma_R(ex7.data$w.length)
ex7.data$sigma.fr <- Phy_Sigma_FR(ex7.data$w.length)
ex7.data$sigma <- Phy_Sigma(ex7.data$w.length)
ggplot(ex7.data, aes(x = w.length)) +
  geom_line(aes(y = sigma.r/ max(sigma.r)), colour = "red") +
  geom_line(aes(y = sigma.fr/ max(sigma.r))) +
  labs(x = "Wavelength (nm)", y = expression(sigma[R]~"and"~sigma[FR]))
rm(ex7.data)

## -----------------------------------------------------------------------------
names(CRYs.mspct)

## -----------------------------------------------------------------------------
A_as_default()
autoplot(interpolate_wl(CRYs.mspct$CRY2_dark, 300:500))

## -----------------------------------------------------------------------------
autoplot(CRYs.mspct[c("CRY2_dark", "CRY2_light")], range = c(300,700))

## -----------------------------------------------------------------------------
autoplot(CRYs.mspct[c("CRY1_dark", "CRY1_light")])

## -----------------------------------------------------------------------------
autoplot(CRYs.mspct["CRY3_dark"], range = c(300,700))

## -----------------------------------------------------------------------------
ggplot(CRYs.mspct[c("CRY1_dark", "CRY2_dark", "CRY3_dark")]) +
  geom_line(aes(linetype = spct.idx)) +
  expand_limits(x = 300)

## -----------------------------------------------------------------------------
names(PHOTs.mspct)

## -----------------------------------------------------------------------------
autoplot(PHOTs.mspct[c("PHOT1_fluo", "PHOT2_fluo")]) +
  expand_limits(x = 300)

## -----------------------------------------------------------------------------
autoplot(PHOTs.mspct[c("PHOT1_dark", "PHOT1_light")])

## -----------------------------------------------------------------------------
autoplot(UVR8s.mspct)

## -----------------------------------------------------------------------------
names(ZTLs.mspct)

## -----------------------------------------------------------------------------
autoplot(ZTLs.mspct) +
  expand_limits(x = 300)

## -----------------------------------------------------------------------------
photon_as_default()

## -----------------------------------------------------------------------------
names(McCree_photosynthesis.mspct)

## -----------------------------------------------------------------------------
autoplot(McCree_photosynthesis.mspct)

## -----------------------------------------------------------------------------
A_as_default()

## -----------------------------------------------------------------------------
names(carotenoids.mspct)

## -----------------------------------------------------------------------------
autoplot(carotenoids.mspct[1:4], 
         annotations = c("-", "labels", "boxes")) 

## -----------------------------------------------------------------------------
autoplot(carotenoids.mspct[5:length(carotenoids.mspct)], 
         annotations = c("-", "labels", "boxes")) 

## -----------------------------------------------------------------------------
A_as_default()

## -----------------------------------------------------------------------------
names(chlorophylls.mspct)

## -----------------------------------------------------------------------------
autoplot(chlorophylls.mspct[c("Chl_a_DME", "Chl_b_DME")]) 

## -----------------------------------------------------------------------------
autoplot(chlorophylls.mspct[c("Chl_a_DME", "Chl_a_MethOH")]) 

## -----------------------------------------------------------------------------
names(chlorophylls_fluorescence.mspct)

## -----------------------------------------------------------------------------
autoplot(chlorophylls_fluorescence.mspct[c("Chl_a_DME", "Chl_b_DME")]) 

## -----------------------------------------------------------------------------
autoplot(chlorophylls_fluorescence.mspct[c("Chl_a_DME", "Chl_a_MethOH")]) 

## -----------------------------------------------------------------------------
Tfr_as_default()

## -----------------------------------------------------------------------------
names(Solidago_altissima.mspct)

## -----------------------------------------------------------------------------
autoplot(Solidago_altissima.mspct$lower_adax) 

## -----------------------------------------------------------------------------
autoplot(Solidago_altissima.mspct$lower_abax) 

## -----------------------------------------------------------------------------
autoplot(as.filter_mspct(Betula_ermanii.mspct)) 

## -----------------------------------------------------------------------------
autoplot(as.reflector_mspct(Betula_ermanii.mspct)) 

