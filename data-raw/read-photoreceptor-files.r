library(dplyr)
library("photobiology")
library(ggspectra)

A_as_default()

## Cryptochromes

CRY1_dark.spct <- read.csv(file = "./data-raw/Cryptochromes/Cry1_dark.csv",
                           header = TRUE, comment.char = "#")
names(CRY1_dark.spct) <- c("w.length", "A")
CRY1_dark.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 100) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("In vitro absorbance of a dark exposed solution of CRY1") -> CRY1_dark.spct

is_normalized(CRY1_dark.spct)
autoplot(CRY1_dark.spct)

CRY1_light.spct <- read.csv(file = "./data-raw/Cryptochromes/Cry1_light_30min.csv",
                           header = TRUE, comment.char = "#")
names(CRY1_light.spct) <- c("w.length", "A")
CRY1_light.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 100) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("In vitro absorbance of a light exposed (30 min) solution of CRY1") -> CRY1_light.spct

is_normalized(CRY1_light.spct)
autoplot(CRY1_light.spct)

CRY3_dark.spct <- read.csv(file = "./data-raw/Cryptochromes/Cry3.csv",
                           header = TRUE, comment.char = "#")
names(CRY3_dark.spct) <- c("w.length", "A")
CRY3_dark.spct %>%
  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  interpolate_spct(length.out = 100) %>%
  setNormalized(norm = TRUE) %>%
  setWhatMeasured("In vitro absorbance of a dark exposed solution of CRY3") -> CRY3_dark.spct

is_normalized(CRY3_dark.spct)
autoplot(CRY3_dark.spct)

CRY2_dark.spct <- read.csv(file = "./data-raw/Cryptochromes/cry2_dark.csv",
                           header = TRUE, comment.char = "#")
CRY2_dark.spct <- CRY2_dark.spct[-(1:2), -3]
setFilterSpct(CRY2_dark.spct, Tfr.type = "internal")
CRY2_dark.spct <- interpolate_spct(CRY2_dark.spct, length.out = 100) %>% normalize()
setWhatMeasured(CRY2_dark.spct, "In vitro absorbance of a dark exposed solution of CRY2")

is_normalized(CRY2_dark.spct)
autoplot(CRY2_dark.spct)

CRY2_light.spct <- read.csv(file = "./data-raw/Cryptochromes/cry2_light.csv",
                            header = TRUE, comment.char = "#")
CRY2_light.spct <- CRY2_light.spct[order(CRY2_light.spct$w.length), -3]
setFilterSpct(CRY2_light.spct, Tfr.type = "internal")
CRY2_light.spct <- interpolate_spct(CRY2_light.spct, length.out = 100) %>% normalize()
setWhatMeasured(CRY2_light.spct, "In vitro absorbance of a light exposed solution of CRY2")

is_normalized(CRY2_light.spct)
autoplot(CRY2_light.spct)

CRYs.mspct <- filter_mspct(list(CRY1_dark = CRY1_dark.spct,
                                CRY1_light = CRY1_light.spct,
                                CRY2_dark = CRY2_dark.spct,
                                CRY2_light = CRY2_light.spct,
                                CRY3_dark = CRY3_dark.spct))
save(CRYs.mspct, file = "./data/CRYs.mspct.rda")

## Phototropins

read.csv(file = "./data-raw/phototropins/phot1.csv",
                           header = TRUE, comment.char = "#") %>%
  rename(w.length = "Line.20.x", A = "Line.20.y") %>%
  filter(A > 1e-2) %>%
  as.filter_spct(Tfr.type = "internal") %>%
  interpolate_spct(length.out = 100) %>%
  normalize() %>%
  setWhatMeasured("phototropin 1 (Arabidopsis)") -> phot1.spct

comment(phot1.spct) <-
  "phototropin 1 (Arabidopsis)\nfrom Cristie et. al, 2002, figure 1a.\nBased on fluorescence yield."

is_normalized(phot1.spct)
autoplot(phot1.spct)

phot2.spct <- read.csv(file = "./data-raw/phototropins/phot2.csv",
                       header = TRUE, comment.char = "#") %>%
  rename(w.length = "Line.26.x", A = "Line.26.y") %>%
#  filter(A > 1e-2) %>%
  as.filter_spct(Tfr.type = "internal") %>%
  interpolate_spct(length.out = 100) %>%
  normalize()

setWhatMeasured(phot2.spct, "phototropin 2 (Arabidopsis)")
comment(phot2.spct) <-
  "phototropin 2 (Arabidopsis)\nfrom Cristie et. al, 2002, figure 7a.\nBased on fluorescence yield."

is_normalized(phot2.spct)
autoplot(phot2.spct)


LOV2.spct <- read.csv(file = "./data-raw/phototropins/LOV2.csv",
                       header = TRUE, comment.char = "#") %>%
  rename(w.length = "Line.27.x", A = "Line.27.y") %>%
  #  filter(A > 1e-2) %>%
  as.filter_spct(Tfr.type = "internal") %>%
  interpolate_spct(length.out = 100) %>%
  normalize()

setWhatMeasured(LOV2.spct, "phototropin 1 LOV2 (Arabidopsis)")
comment(LOV2.spct) <-
  "phototropin 1 LOV2 (Arabidopsis)\nfrom XXXX, figure 7a.\nIn vitro absorbance."

is_normalized(LOV2.spct)
autoplot(LOV2.spct)

PHOTs.mspct <- filter_mspct(list(phot1.fluo = phot1.spct,
                                phot2.fluo = phot2.spct,
                                phot1.LOV2.abs = LOV2.spct))
save(PHOTs.mspct, file = "./data/PHOTs.mspct.rda")

## UVR8

load("./data-raw/UVR8/UVR8.raw.data.rda")
UVR8_Glasgow.spct <- UVR8.raw.data[-64, ]
names(UVR8_Glasgow.spct) <- c("w.length", "A")
setFilterSpct(UVR8_Glasgow.spct, Tfr.type = "internal")
UVR8_Glasgow.spct <- normalize(UVR8_Glasgow.spct)
setWhatMeasured(UVR8_Glasgow.spct, "UVR8 (Arabidopsis)")
comment(phot1.spct) <-
  "UVR8 (Arabidopsis)\nfrom Christie et al. 2012, figure S3\nIn vitro absorbance."

UVR8_Glasgow.spct <- interpolate_spct(UVR8_Glasgow.spct, length.out = 100)

is_normalized(UVR8_Glasgow.spct)
autoplot(UVR8_Glasgow.spct)

UVR8.mspct <- filter_mspct(list(UVR8.abs.Glasgow = UVR8_Glasgow.spct))
save(UVR8.mspct, file = "./data/UVR8.mspct.rda")

## Phytochromes

load("./data-raw/Phytochromes/phytochrome.data.rda")
phytochrome.spct <- setGenericSpct(phytochrome.data)
save(phytochrome.spct, file = "./data/phytochrome.spct.rda")
