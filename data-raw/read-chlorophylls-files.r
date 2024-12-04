library(dplyr)
library("photobiology")
library(ggspectra)

rm(list = ls(pattern = "*"))

A_as_default()

## Chlorophylls (absorbance spectra)

chl_a_MethOH.spct <- read.table(file = "./data-raw/chlorophylls/chla-122-abs.txt",
                                header = FALSE, comment.char = "#",
                                col.names = c("w.length", "A"))
chl_a_MethOH.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  setFilterProperties(Rfr.constant = NA_real_, thickness = NA_real_,
                      attenuation.mode = "absorption") %>%
  clean() %>%
  smooth_spct(method = "supsmu", strength = 0.3) %>%
  normalize() %>%
  thin_wl(max.wl.step = 2) %>%
  setWhatMeasured("Chlorophyll a in Methanol (122-abs.txt)") -> chl_a_MethOH.spct

file_header <- readLines("./data-raw/chlorophylls/chla-122-abs.txt", n = 22L)
comment(chl_a_MethOH.spct) <- paste("File header:",
                                    paste(file_header, collapse = "\n"),
                                    sep = "\n")
cat(comment(chl_a_MethOH.spct))

getNormalized(chl_a_MethOH.spct)
autoplot(chl_a_MethOH.spct)

chl_a_DME.spct <- read.table(file = "./data-raw/chlorophylls/chla-123-abs.txt",
                                header = FALSE, comment.char = "#",
                                col.names = c("w.length", "A"))
chl_a_DME.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  setFilterProperties(Rfr.constant = NA_real_, thickness = NA_real_,
                      attenuation.mode = "absorption") %>%
  smooth_spct(method = "supsmu", strength = 0.3) %>%
  clean() %>%
  normalize() %>%
  thin_wl(max.wl.step = 2) %>%
  setWhatMeasured("Chlorophyll a in di-methyl-ether (123-abs.txt)") -> chl_a_DME.spct

file_header <- readLines("./data-raw/chlorophylls/chla-123-abs.txt", n = 22L)
comment(chl_a_DME.spct) <- paste("File header:",
                                    paste(file_header, collapse = "\n"),
                                    sep = "\n")
cat(comment(chl_a_DME.spct))

getNormalized(chl_a_DME.spct)
autoplot(chl_a_DME.spct)

chl_b_DME.spct <- read.table(file = "./data-raw/chlorophylls/chlb-125-abs.txt",
                                header = FALSE, comment.char = "#",
                                col.names = c("w.length", "A"))
chl_b_DME.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  setFilterProperties(Rfr.constant = NA_real_, thickness = NA_real_,
                      attenuation.mode = "absorption") %>%
  clip_wl(c(305, NA)) %>%
  smooth_spct(method = "supsmu", strength = 0.3) %>%
  clean() %>%
  normalize() %>%
  thin_wl(max.wl.step = 2) %>%
  setWhatMeasured("Chlorophyll a in Methanol (122-abs.txt)") -> chl_b_DME.spct

file_header <- readLines("./data-raw/chlorophylls/chlb-125-abs.txt", n = 22L)
comment(chl_b_DME.spct) <- paste("File header:",
                                 paste(file_header, collapse = "\n"),
                                 sep = "\n")
cat(comment(chl_b_DME.spct))

getNormalized(chl_b_DME.spct)
autoplot(chl_b_DME.spct)

chlorophylls.mspct <- filter_mspct(list(Chl_a_MethOH = chl_a_MethOH.spct,
                                        Chl_a_DME = chl_a_DME.spct,
                                        Chl_b_DME = chl_b_DME.spct))

save(chlorophylls.mspct, file = "./data/chlorophylls-mspct.rda")

## Chlorophylls (fluorescence spectra)

chl_a_ems_MethOH.spct <- read.table(file = "./data-raw/chlorophylls/chla-122-ems.txt",
                                header = FALSE, comment.char = "#",
                                col.names = c("w.length", "s.e.irrad"))
chl_a_ems_MethOH.spct %>%
  group_by(w.length) %>%
  summarise(s.e.irrad = median(s.e.irrad)) %>%
  setSourceSpct() %>%
  smooth_spct(method = "supsmu", strength = 0.3) %>%
  clean() %>%
  normalize() %>%
  na.omit() %>%
  thin_wl(max.wl.step = 2) %>%
  setWhatMeasured("Chlorophyll a fluorescence emission spectrum in methanol (122-ems.txt)") -> chl_a_ems_MethOH.spct

file_header <- readLines("./data-raw/chlorophylls/chla-122-ems.txt", n = 15L)
comment(chl_a_ems_MethOH.spct) <- paste("File header:",
                                    paste(file_header, collapse = "\n"),
                                    sep = "\n")
cat(comment(chl_a_ems_MethOH.spct))

getNormalized(chl_a_ems_MethOH.spct)
autoplot(chl_a_ems_MethOH.spct)

chl_a_ems_DME.spct <- read.table(file = "./data-raw/chlorophylls/chla-123-ems.txt",
                             header = FALSE, comment.char = "#",
                             col.names = c("w.length", "s.e.irrad"))
chl_a_ems_DME.spct %>%
  group_by(w.length) %>%
  summarise(s.e.irrad = median(s.e.irrad)) %>%
  setSourceSpct() %>%
  smooth_spct(method = "supsmu", strength = 0.3) %>%
  clean() %>%
  normalize() %>%
  na.omit() %>%
  thin_wl(max.wl.step = 2) %>%
  setWhatMeasured("Chlorophyll a fluorescence emission spectrum in di-methyl-ether (123-ems.txt)") -> chl_a_ems_DME.spct

file_header <- readLines("./data-raw/chlorophylls/chla-123-ems.txt", n = 15L)
comment(chl_a_ems_DME.spct) <- paste("File header:",
                                 paste(file_header, collapse = "\n"),
                                 sep = "\n")
cat(comment(chl_a_ems_DME.spct))

getNormalized(chl_a_ems_DME.spct)
autoplot(chl_a_ems_DME.spct)

chl_b_ems_DME.spct <- read.table(file = "./data-raw/chlorophylls/chlb-125-ems.txt",
                                 header = FALSE, comment.char = "#",
                                 col.names = c("w.length", "s.e.irrad"))
chl_b_ems_DME.spct %>%
  group_by(w.length) %>%
  summarise(s.e.irrad = median(s.e.irrad)) %>%
  setSourceSpct() %>%
  smooth_spct(method = "supsmu", strength = 0.3) %>%
  clean() %>%
  normalize() %>%
  na.omit() %>%
  thin_wl(max.wl.step = 2) %>%
  setWhatMeasured("Chlorophyll b fluorescence emission spectrum in di-methyl-ether (125-ems.txt)") -> chl_b_ems_DME.spct

file_header <- readLines("./data-raw/chlorophylls/chlb-125-ems.txt", n = 15L)
comment(chl_b_ems_DME.spct) <- paste("File header:",
                                 paste(file_header, collapse = "\n"),
                                 sep = "\n")
cat(comment(chl_b_ems_DME.spct))

getNormalized(chl_b_ems_DME.spct)
autoplot(chl_b_ems_DME.spct)


chlorophylls_fluorescence.mspct <- source_mspct(list(Chl_a_MethOH = chl_a_ems_MethOH.spct,
                                        Chl_a_DME = chl_a_ems_DME.spct,
                                        Chl_b_DME = chl_b_ems_DME.spct))

autoplot(chlorophylls_fluorescence.mspct)

save(chlorophylls_fluorescence.mspct, file = "./data/chlorophylls-fluorescence-mspct.rda")

unset_filter_qty_default()
