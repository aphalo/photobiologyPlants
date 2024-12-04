library(dplyr)
library("photobiology")
library(ggspectra)

photon_as_default()

McCree_Amaranth.spct <- read.table(file = "./data-raw/A_McCree_1972_Amaranth.txt", header = TRUE, comment.char = "#")
McCree_Amaranth.spct <- McCree_Amaranth.spct[order(McCree_Amaranth.spct$w.length), ]
McCree_Amaranth.spct <- McCree_Amaranth.spct[-which(diff(McCree_Amaranth.spct$w.length) < 0.1), ]
setResponseSpct(McCree_Amaranth.spct) %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  normalize() %>%
  thin_wl() %>%
  setWhatMeasured("Action spectrum of net CO2 uptake in Amaranthus edulis Speg. var. UCD 1966 (McCree 1972).") %>%
  setHowMeasured("Net CO2 uptake measured on leaf sections after about 2 minutes equilibration time at each wavelength.") ->
  McCree_Amaranth.spct

comment(McCree_Amaranth.spct) <- "One of the 'classical' action spectra of photosynthesis by K. J. McCree (1972): Amaranthus edulis Speg. var. UCD 1966 leaf sections."

is_normalized(McCree_Amaranth.spct)
autoplot(McCree_Amaranth.spct)

McCree_Oat.spct <- read.table(file = "./data-raw/A_McCree_1972_Oats.txt", header = TRUE, comment.char = "#")
McCree_Oat.spct <- McCree_Oat.spct[order(McCree_Oat.spct$w.length), ]
McCree_Oat.spct <- McCree_Oat.spct[-which(diff(McCree_Oat.spct$w.length) < 0.1), ]
setResponseSpct(McCree_Oat.spct) %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  normalize() %>%
  thin_wl() %>%
  setWhatMeasured("Action spectrum of net CO2 uptake in Avena sativa L. var. Coronado (McCree 1972).") %>%
  setHowMeasured("Net CO2 uptake measured on detached leaf sections after about 2 to 10 minutes equilibration time at each wavelength") ->
  McCree_Oat.spct

comment(McCree_Oat.spct) <- "One of the 'classical' action spectra of photosynthesis from K. J. McCree (1972): Avena sativa L. var. Coronado leaf sections."

is_normalized(McCree_Oat.spct)
autoplot(McCree_Oat.spct)

McCree_photosynthesis.mspct <- response_mspct(list(amaranth = McCree_Amaranth.spct,
                                                   oats = McCree_Oat.spct))

autoplot(McCree_photosynthesis.mspct)

save(McCree_photosynthesis.mspct, file = "./data/McCree.rda")

unset_radiation_unit_default()
