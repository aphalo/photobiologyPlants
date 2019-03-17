library(dplyr)
library("photobiology")
library(ggspectra)

A_as_default()

## Carotenoids

beta_carotene.spct <- read.csv(file = "./data-raw/carotenoids/beta-carotene.csv",
                           header = TRUE, comment.char = "#")
names(beta_carotene.spct) <- c("w.length", "A")
beta_carotene.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Beta-carotene in Methanol (VCA0001SP1101)") -> beta_carotene.spct

is_normalized(beta_carotene.spct)
autoplot(beta_carotene.spct)

dihydro_lycopene.spct <- read.csv(file = "./data-raw/carotenoids/Dihydrolycopene.csv",
                               header = TRUE, comment.char = "#")
names(dihydro_lycopene.spct) <- c("w.length", "A")
dihydro_lycopene.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("3-4 dihydro-lycopene (VCA0010SP1051)") -> dihydro_lycopene.spct

is_normalized(dihydro_lycopene.spct)
autoplot(dihydro_lycopene.spct)

lycopene.spct <- read.csv(file = "./data-raw/carotenoids/Lycopene.csv",
                                  header = TRUE, comment.char = "#")
names(lycopene.spct) <- c("w.length", "A")
lycopene.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Lycopene in Methanol (VCA0010SP1107)") -> lycopene.spct

is_normalized(lycopene.spct)
autoplot(lycopene.spct)

lutein.spct <- read.csv(file = "./data-raw/carotenoids/Lutein.csv",
                          header = TRUE, comment.char = "#")
names(lutein.spct) <- c("w.length", "A")
lutein.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Lutein in Methanol (VCA0016SP1108)") -> lutein.spct

is_normalized(lutein.spct)
autoplot(lutein.spct)

lutein.spct <- read.csv(file = "./data-raw/carotenoids/Lutein.csv",
                        header = TRUE, comment.char = "#")
names(lutein.spct) <- c("w.length", "A")
lutein.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Lutein in Methanol (VCA0016SP1108)") -> lutein.spct

is_normalized(lutein.spct)
autoplot(lutein.spct)

phytoene.spct <- read.csv(file = "./data-raw/carotenoids/Phytoene.csv",
                        header = TRUE, comment.char = "#")
names(phytoene.spct) <- c("w.length", "A")
phytoene.spct %>%
  filter(w.length < 322) %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Phytoene (VCA1010SP1051)") -> phytoene.spct

is_normalized(phytoene.spct)
autoplot(phytoene.spct)

phytofluene.spct <- read.csv(file = "./data-raw/carotenoids/Phytofluene.csv",
                          header = TRUE, comment.char = "#")
names(phytofluene.spct) <- c("w.length", "A")
phytofluene.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Phytoene (VCA1010SP1051)") -> phytofluene.spct

is_normalized(phytofluene.spct)
autoplot(phytofluene.spct)

violaxanthin.spct <- read.csv(file = "./data-raw/carotenoids/Violaxanthin.csv",
                             header = TRUE, comment.char = "#")
names(violaxanthin.spct) <- c("w.length", "A")
violaxanthin.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Violaxanthin in methanol (VCA0060SP1112)") -> violaxanthin.spct

is_normalized(violaxanthin.spct)
autoplot(violaxanthin.spct)

zeaxanthin.spct <- read.csv(file = "./data-raw/carotenoids/Zeaxanthin.csv",
                             header = TRUE, comment.char = "#")
names(zeaxanthin.spct) <- c("w.length", "A")
zeaxanthin.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("Zeaxanthin (VCA0007SP1105)") -> zeaxanthin.spct

is_normalized(zeaxanthin.spct)
autoplot(zeaxanthin.spct)

carotenoids.mspct <- filter_mspct(list(beta_carotene = beta_carotene.spct,
                                       dihydro_lycopene = dihydro_lycopene.spct,
                                       lycopene = lycopene.spct,
                                       lutein = lutein.spct,
                                       phytoene = phytoene.spct,
                                       phytofluene = phytofluene.spct,
                                       violaxanthin = violaxanthin.spct,
                                       zeaxanthin = zeaxanthin.spct))

save(carotenoids.mspct, file = "./data/carotenoids-mspct.rda")

unset_filter_qty_default()
