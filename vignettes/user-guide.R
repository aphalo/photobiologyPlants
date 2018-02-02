## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=8, fig.height=4)

## ----message=FALSE-------------------------------------------------------
library(ggplot2)
library(ggspectra)
library(photobiology)
library(photobiologyPlants)
library(photobiologyWavebands)

## ------------------------------------------------------------------------
Pfr_Ptot(sun.spct)

## ------------------------------------------------------------------------
R_FR(sun.spct)

## ------------------------------------------------------------------------
q_ratio(sun.spct, Red("Smith10"), Far_red("Smith10"))

## ------------------------------------------------------------------------
Pfr_Ptot_R_FR(R_FR(sun.spct))

## ------------------------------------------------------------------------
Pfr_Ptot(660)
Pfr_Ptot(735)
Pfr_Ptot(c(660, 735))
Pfr_Ptot(435)

## ------------------------------------------------------------------------
plot(Pfr_Ptot(300:770), norm = NULL, unit.out = "photon",
     w.band = Plant_bands(),
     annotations = c("colour.guide", "labels", "boxes")) +
  labs(y = "Phytochrome photoequilibrium, Pfr:Ptot ratio")

## ------------------------------------------------------------------------
ggplot(data = Pfr_Ptot(300:770), aes(w.length, s.q.response)) +
  geom_line() +
  labs(x = "Wavelength (nm)",
     y = "Phytochrome photoequilibrium, Pfr:Ptot ratio")

## ------------------------------------------------------------------------
Pfr_Ptot_R_FR(1.15)
Pfr_Ptot_R_FR(0.01)
Pfr_Ptot_R_FR(c(1.15,0.01))

## ------------------------------------------------------------------------
ex6.data <- data.frame(r.fr=seq(0.01, 5.0, length.out=100), Pfr.p=numeric(100))
ex6.data$Pfr.p <- Pfr_Ptot_R_FR(ex6.data$r.fr)
ggplot(data=ex6.data, aes(r.fr, Pfr.p)) +
  geom_line() +
    labs(x ="R:FR photon ratio",
         y = "Phytochrome photoequilibrium, Pfr:Ptot ratio")


## ------------------------------------------------------------------------
with(clip_wl(sun.spct, c(300,770)), Phy_reaction_rates(w.length, s.e.irrad))

## ------------------------------------------------------------------------
ex7.data <- data.frame(w.length=seq(300, 770, length.out=100))
ex7.data$sigma.r <- Phy_Sigma_R(ex7.data$w.length)
ex7.data$sigma.fr <- Phy_Sigma_FR(ex7.data$w.length)
ex7.data$sigma <- Phy_Sigma(ex7.data$w.length)
plot(I(sigma.r/ max(sigma.r)) ~ w.length, data=ex7.data, type="l", col="red",
     xlab="Wavelength (nm)", ylab=expression(sigma[R]~and~sigma[FR]))
lines(I(sigma.fr/max(sigma.r)) ~ w.length, data=ex7.data)
rm(ex7.data)

## ------------------------------------------------------------------------
A_as_default()
plot(interpolate_wl(CRY2.mspct$dark_adapted, 300:500))

## ------------------------------------------------------------------------
plot(CRY2.mspct$dark_adapted, range = c(300,700))

## ------------------------------------------------------------------------
plot(normalize(UVR8_Glasgow.spct))

## ------------------------------------------------------------------------
names(McCree_photosynthesis.mspct)

## ------------------------------------------------------------------------
names(Solidago_altissima.mspct)

