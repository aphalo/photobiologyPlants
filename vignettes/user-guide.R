## ----include=FALSE, echo=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width=7.2, fig.height=4.3)

## ----printing-spectra, eval=TRUE, include=FALSE-------------------------------
# library(tibble)
old.options <- options(tibble.print_max = 6, tibble.print_min = 4)

## ----message=FALSE------------------------------------------------------------
library(photobiology)
library(photobiologyPlants)
library(photobiologyWavebands)
eval_plots <- requireNamespace("ggspectra", quietly = TRUE)
if (eval_plots) library(ggspectra)

theme_set(theme_bw())

## -----------------------------------------------------------------------------
R_FR(sun.spct)

## -----------------------------------------------------------------------------
R_FR(sun_evening.spct, 
     std = "Smith20", 
     attr2tb = c("when.measured" = "time.UTC", "lat" = "latitude"))

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

## ----eval = eval_plots--------------------------------------------------------
autoplot(Pfr_Ptot(300:770), unit.out = "photon",
         w.band = Plant_bands(),
         annotations = c("colour.guide", "labels", "boxes")) +
  labs(y = "Phytochrome photoequilibrium, Pfr:Ptot ratio")

## ----eval = eval_plots--------------------------------------------------------
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

## ----eval = eval_plots--------------------------------------------------------
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

## ----eval = eval_plots--------------------------------------------------------
A_as_default()
autoplot(CRYs.mspct$CRY2_dark)

## ----eval = eval_plots, warning=FALSE-----------------------------------------
autoplot(CRYs.mspct[c("CRY2_dark", "CRY2_light")])

## ----eval = eval_plots, warning=FALSE-----------------------------------------
autoplot(CRYs.mspct[c("CRY1_dark", "CRY1_light")])

## ----eval = eval_plots--------------------------------------------------------
autoplot(CRYs.mspct["CRY3_dark"], range = c(300,700))

## ----eval = eval_plots--------------------------------------------------------
ggplot(CRYs.mspct[c("CRY1_dark", "CRY2_dark", "CRY3_dark")]) +
  geom_line(aes(linetype = spct.idx)) +
  expand_limits(x = 300)

## -----------------------------------------------------------------------------
names(PHOTs.mspct)

## ----eval = eval_plots--------------------------------------------------------
autoplot(PHOTs.mspct[c("PHOT1_fluo", "PHOT2_fluo")]) +
  expand_limits(x = 300)

## ----eval = eval_plots--------------------------------------------------------
autoplot(PHOTs.mspct[c("PHOT1_dark", "PHOT1_light")])

## -----------------------------------------------------------------------------
autoplot(UVR8s.mspct)

## -----------------------------------------------------------------------------
names(ZTLs.mspct)

## ----eval = eval_plots--------------------------------------------------------
autoplot(ZTLs.mspct) +
  expand_limits(x = 300)

## -----------------------------------------------------------------------------
A_as_default()

## -----------------------------------------------------------------------------
names(carotenoids.mspct)

## ----eval = eval_plots--------------------------------------------------------
autoplot(carotenoids.mspct[1:4], 
         annotations = c("-", "labels", "boxes")) 

## ----eval = eval_plots--------------------------------------------------------
autoplot(carotenoids.mspct[5:length(carotenoids.mspct)], 
         annotations = c("-", "labels", "boxes")) 

## -----------------------------------------------------------------------------
A_as_default()

## -----------------------------------------------------------------------------
names(chlorophylls.mspct)

## ----eval = eval_plots--------------------------------------------------------
autoplot(chlorophylls.mspct[c("Chl_a_DME", "Chl_b_DME")]) 

## ----eval = eval_plots--------------------------------------------------------
autoplot(chlorophylls.mspct[c("Chl_a_DME", "Chl_a_MethOH")]) 

## -----------------------------------------------------------------------------
names(chlorophylls_fluorescence.mspct)

## ----eval = eval_plots--------------------------------------------------------
autoplot(chlorophylls_fluorescence.mspct[c("Chl_a_DME", "Chl_b_DME")]) 

## ----eval = eval_plots--------------------------------------------------------
autoplot(chlorophylls_fluorescence.mspct[c("Chl_a_DME", "Chl_a_MethOH")]) 

## -----------------------------------------------------------------------------
Tfr_as_default()

## -----------------------------------------------------------------------------
names(Solidago_altissima.mspct)

## ----eval = eval_plots--------------------------------------------------------
autoplot(Solidago_altissima.mspct$lower_adax) 

## ----eval = eval_plots--------------------------------------------------------
autoplot(Solidago_altissima.mspct$lower_abax) 

## ----eval = eval_plots--------------------------------------------------------
autoplot(as.filter_mspct(Betula_ermanii.mspct)) 

## ----eval = eval_plots--------------------------------------------------------
autoplot(as.reflector_mspct(Betula_ermanii.mspct)) 

## -----------------------------------------------------------------------------
names(leaf_fluorescence.mspct)

## ----eval = eval_plots--------------------------------------------------------
autoplot(leaf_fluorescence.mspct$wheat_Fo_ex355nm)

## ----eval = eval_plots--------------------------------------------------------
photon_as_default()

## -----------------------------------------------------------------------------
names(McCree_photosynthesis.mspct)

## ----eval = eval_plots--------------------------------------------------------
autoplot(McCree_photosynthesis.mspct)

## -----------------------------------------------------------------------------
1 / gs_mol2vol(0.150) # mol m-2 s-1 -> s m-2

## -----------------------------------------------------------------------------
# mol m-2 s-1 -> s m-2
1 / gs_mol2vol(0.150, temperature = 5, pressure = 98e3) 

## -----------------------------------------------------------------------------
gs_c_from_gs_w(0.150) # mol m-2 s-1

## -----------------------------------------------------------------------------
gs_w <-
  gs_w_from_size(length = 10e-6, # m
                 width = 5e-6, # m
                 depth = 20e-6, # m
                 n = 50e6, # m^-2 (stomatal density)
                 temperature = 30) |>  # C
  gs_vol2mol(temperature = 30, pressure = 101e3) # C, Pa
gs_w # for water vapour mol m-2 s-1
gs_w * 1e3 # for water vapour mmol m-2 s-1
gs_c_from_gs_w(gs_w) * 1e3 # for CO2 mmol m-2 s-1

## -----------------------------------------------------------------------------
D_water(c(0, 10, 20, 30, 40))
D_CO2(c(0, 10, 20, 30, 40))

## -----------------------------------------------------------------------------
molar_vol(c(0, 10, 20, 30, 40))
molar_vol(temperature = 25, pressure = c(98e3, 100e3, 102e3))

## -----------------------------------------------------------------------------
ET_ref(temperature = 20, # C
       water.vp = water_RH2vp(relative.humidity = 70, # RH%
                              temperature = 20), # C -> Pa
       wind.speed = 0, # m s-1
       net.irradiance = 100) # W m-2

## -----------------------------------------------------------------------------
ET_ref_day(temperature = 20, # C daily mean
           water.vp = 1636.616, # Pa daily mean
           wind.speed = 5, # m s-1 daily mean
           net.radiation = 15e6) # 15 MJ / d / m2 daily total !

## -----------------------------------------------------------------------------
ET_ref(temperature = 20, # C
       water.vp = water_RH2vp(relative.humidity = (1:9) * 10, # RH%
                              temperature = 20), # C -> Pa
       wind.speed = 5, # m s-1
       net.irradiance = 10) # W m-2

## -----------------------------------------------------------------------------
ET_ref_irrad.df <-
  data.frame(irrad = (1:40) * 10,
             ET.ref = ET_ref(temperature = 20, # C
                             water.vp = water_RH2vp(relative.humidity = 70, # RH%
                                                    temperature = 20), # C -> Pa
                             wind.speed = 5, # m s-1
                             net.irradiance = (1:40) * 10) # W m-2
  )
ggplot(ET_ref_irrad.df, aes(irrad, ET.ref)) +
  geom_line() +
  labs(x = expression("Global radiation "*(W~m^{-2})),
       y = expression("Reference evapotranspiration "*(mm~h^{-1})))

## -----------------------------------------------------------------------------
water_vp_sat(20) # temperature in C, partial pressure in Pa
water_vp_sat(20) * 1e-3 # temperature in C, partial pressure in kPa

## -----------------------------------------------------------------------------
vp_sat.df <- data.frame(temperature = -20:100,
                        vp.sat = c(water_vp_sat(-20:-1, over.ice = TRUE),
                                   water_vp_sat(0:100)) * 1e-3)

ggplot(vp_sat.df, aes(temperature, vp.sat)) +
  geom_line() +
  labs(x = "Temperature (C)", y = "Water valour pressure at saturation (kPa)")

## -----------------------------------------------------------------------------
water_vp2RH(1000, 25) # Pa and C -> RH%

## -----------------------------------------------------------------------------
water_vp2mvc(1000, 25) # Pa and C ->  mass per volume g m-3

## -----------------------------------------------------------------------------
water_dp(1000) # Pa -> C 

## -----------------------------------------------------------------------------
water_fp(500) # Pa -> C 

