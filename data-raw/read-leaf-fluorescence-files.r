library(dplyr)
library("photobiology")
library(ggspectra)

rm(list = ls(pattern = "*"))

A_as_default()

## Chlorophylls (fluorescence spectra)

wheat_leaf_fluo.spct <- read.csv(file = "./data-raw/leaf-fluorescence-ex355/fluorescence.csv",
                                    header = FALSE, skip = 1, comment.char = "#",
                                    col.names = c("w.length", "s.e.irrad"))
wheat_leaf_fluo.spct %>%
  group_by(w.length) %>%
  summarise(s.e.irrad = median(s.e.irrad)) %>%
  setSourceSpct() %>%
  smooth_spct(method = "supsmu", strength = 0.3) %>%
  clean() %>%
  normalize() %>%
  na.omit() %>%
  setHowMeasured("Digitized from Meyer et. al (2003, Fig. 2A)") %>%
  setWhatMeasured("Fo Fluorescence emission spectrum of a wheat leaf excited with UV-A at 355 nm with very low irradiance.") ->
  wheat_leaf_fluo_ex355nm.spct

autoplot(wheat_leaf_fluo_ex355nm.spct)

wheat_leaf_fluo_ex355nm.spct <-
  thin_wl(wheat_leaf_fluo_ex355nm.spct)

autoplot(wheat_leaf_fluo_ex355nm.spct)

comment(wheat_leaf_fluo_ex355nm.spct) <- "Digitized from Fig. 2A of Meyer et al. (2003) Journal of Experimental Botany, 54: 757-769. DOI:10.1093/jxb/erg063"
comment(wheat_leaf_fluo_ex355nm.spct)

getNormalized(wheat_leaf_fluo_ex355nm.spct)

leaf_fluorescence.mspct <- source_mspct(list(wheat_Fo_ex355nm = wheat_leaf_fluo_ex355nm.spct))

summary(leaf_fluorescence.mspct)

save(leaf_fluorescence.mspct, file = "./data/leaf-fluorescence-mspct.rda")

unset_filter_qty_default()
