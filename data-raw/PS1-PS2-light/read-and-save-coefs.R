library(photobiology)
library(ggspectra)
library(readr)
library(ggplot2)

photon_as_default()

PS1_PS2_k.spct <- read_tsv("data-raw/PS1-PS2-light/PS1-PS2-coeffs.tsv",
                           skip = 1, col_names = c("w.length", "k"))
setGenericSpct(PS1_PS2_k.spct)
what_measured(PS1_PS2_k.spct) <-
  paste0("Coefficients for PSI vs. PSII excitation as a function of ",
         "wavelength from Mattila et al. 2020, doi: 10.1111/tpj.14983, ",
         "supplementary file.")
how_measured(PS1_PS2_k.spct) <-
  "Data copied from supplementary file to published paper."

summary(PS1_PS2_k.spct)

autoplot(PS1_PS2_k.spct, y.name = "k") +
  geom_hline(yintercept = 0, linetype = "dashed")

save(PS1_PS2_k.spct, file = "./data/Mattila.rda")
