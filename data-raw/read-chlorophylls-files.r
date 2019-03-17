library(dplyr)
library("photobiology")
library(ggspectra)

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
  clean() %>%
  smooth_spct(method = "supsmu") %>%
  #  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Chlorophyll a in Methanol (122-abs.txt)") -> chl_a_MethOH.spct

file_header <- readLines("./data-raw/chlorophylls/chla-122-abs.txt", n = 22L)
comment(chl_a_MethOH.spct) <- paste("File header:",
                                    paste(file_header, collapse = "\n"),
                                    sep = "\n")
cat(comment(chl_a_MethOH.spct))

is_normalized(chl_a_MethOH.spct)
autoplot(chl_a_MethOH.spct)

chl_a_DME.spct <- read.table(file = "./data-raw/chlorophylls/chla-123-abs.txt",
                                header = FALSE, comment.char = "#",
                                col.names = c("w.length", "A"))
chl_a_DME.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  smooth_spct(method = "supsmu") %>%
  #  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Chlorophyll a in di-methyl-ether (123-abs.txt)") -> chl_a_DME.spct

file_header <- readLines("./data-raw/chlorophylls/chla-123-abs.txt", n = 22L)
comment(chl_a_DME.spct) <- paste("File header:",
                                    paste(file_header, collapse = "\n"),
                                    sep = "\n")
cat(comment(chl_a_DME.spct))

is_normalized(chl_a_DME.spct)
autoplot(chl_a_DME.spct)

chl_b_DME.spct <- read.table(file = "./data-raw/chlorophylls/chlb-125-abs.txt",
                                header = FALSE, comment.char = "#",
                                col.names = c("w.length", "A"))
chl_b_DME.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
#  interpolate_spct(length.out = 300) %>%
  smooth_spct(method = "supsmu") %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Chlorophyll a in Methanol (122-abs.txt)") -> chl_b_DME.spct

file_header <- readLines("./data-raw/chlorophylls/chlb-125-abs.txt", n = 22L)
comment(chl_b_DME.spct) <- paste("File header:",
                                 paste(file_header, collapse = "\n"),
                                 sep = "\n")
cat(comment(chl_b_DME.spct))

is_normalized(chl_b_DME.spct)
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
  clean() %>%
  smooth_spct(method = "supsmu") %>%
  #  interpolate_spct(length.out = 300) %>%
  normalize() %>%
  na.omit() %>%
  setWhatMeasured("Chlorophyll a fluorescence emission spectrum in methanol (122-ems.txt)") -> chl_a_ems_MethOH.spct

file_header <- readLines("./data-raw/chlorophylls/chla-122-ems.txt", n = 15L)
comment(chl_a_ems_MethOH.spct) <- paste("File header:",
                                    paste(file_header, collapse = "\n"),
                                    sep = "\n")
cat(comment(chl_a_ems_MethOH.spct))

is_normalized(chl_a_ems_MethOH.spct)
autoplot(chl_a_ems_MethOH.spct)

chl_a_ems_DME.spct <- read.table(file = "./data-raw/chlorophylls/chla-123-ems.txt",
                             header = FALSE, comment.char = "#",
                             col.names = c("w.length", "s.e.irrad"))
chl_a_ems_DME.spct %>%
  group_by(w.length) %>%
  summarise(s.e.irrad = median(s.e.irrad)) %>%
  setSourceSpct() %>%
  clean() %>%
  smooth_spct(method = "supsmu") %>%
  #  interpolate_spct(length.out = 300) %>%
  normalize() %>%
  na.omit() %>%
  setWhatMeasured("Chlorophyll a fluorescence emission spectrum in di-methyl-ether (123-ems.txt)") -> chl_a_ems_DME.spct

file_header <- readLines("./data-raw/chlorophylls/chla-123-ems.txt", n = 15L)
comment(chl_a_ems_DME.spct) <- paste("File header:",
                                 paste(file_header, collapse = "\n"),
                                 sep = "\n")
cat(comment(chl_a_ems_DME.spct))

is_normalized(chl_a_ems_DME.spct)
autoplot(chl_a_ems_DME.spct)

chl_b_ems_DME.spct <- read.table(file = "./data-raw/chlorophylls/chlb-125-ems.txt",
                                 header = FALSE, comment.char = "#",
                                 col.names = c("w.length", "s.e.irrad"))
chl_b_ems_DME.spct %>%
  group_by(w.length) %>%
  summarise(s.e.irrad = median(s.e.irrad)) %>%
  setSourceSpct() %>%
  clean() %>%
  smooth_spct(method = "supsmu") %>%
  #  interpolate_spct(length.out = 300) %>%
  normalize() %>%
  na.omit() %>%
  setWhatMeasured("Chlorophyll b fluorescence emission spectrum in di-methyl-ether (125-ems.txt)") -> chl_b_ems_DME.spct

file_header <- readLines("./data-raw/chlorophylls/chlb-125-ems.txt", n = 15L)
comment(chl_b_ems_DME.spct) <- paste("File header:",
                                 paste(file_header, collapse = "\n"),
                                 sep = "\n")
cat(comment(chl_b_ems_DME.spct))

is_normalized(chl_b_ems_DME.spct)
autoplot(chl_b_ems_DME.spct)


chlorophylls_fluorescence.mspct <- source_mspct(list(Chl_a_MethOH = chl_a_ems_MethOH.spct,
                                        Chl_a_DME = chl_a_ems_DME.spct,
                                        Chl_b_DME = chl_b_ems_DME.spct))

autoplot(chlorophylls_fluorescence.mspct)

save(chlorophylls_fluorescence.mspct, file = "./data/chlorophylls-fluorescence-mspct.rda")

unset_filter_qty_default()
