library(dplyr)
library(photobiology)
library(ggspectra)

A_as_default()

## Cryptochromes

CRY1_dark.spct <- read.csv(file = "./data-raw/Cryptochromes/Cry1_dark.csv",
                           header = TRUE, comment.char = "#")
names(CRY1_dark.spct) <- c("w.length", "A")
CRY1_dark.spct %>%
#  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  setWhatMeasured("In vitro absorbance of a dark exposed solution of CRY1") -> CRY1_dark.spct

getNormalized(CRY1_dark.spct)
getNormalization(CRY1_dark.spct)
autoplot(CRY1_dark.spct)

CRY1_light.spct <- read.csv(file = "./data-raw/Cryptochromes/Cry1_light_30min.csv",
                           header = TRUE, comment.char = "#")
names(CRY1_light.spct) <- c("w.length", "A")
CRY1_light.spct %>%
#  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  setWhatMeasured("In vitro absorbance of a light exposed (30 min) solution of CRY1") -> CRY1_light.spct

getNormalized(CRY1_light.spct)
getNormalization(CRY1_light.spct)
autoplot(CRY1_light.spct)

CRY3_dark.spct <- read.csv(file = "./data-raw/Cryptochromes/Cry3.csv",
                           header = TRUE, comment.char = "#")
names(CRY3_dark.spct) <- c("w.length", "A")
CRY3_dark.spct %>%
#  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  setWhatMeasured("In vitro absorbance of a dark exposed solution of CRY3") -> CRY3_dark.spct

getNormalized(CRY3_dark.spct)
getNormalization(CRY3_dark.spct)
autoplot(CRY3_dark.spct)

CRY2_dark.spct <- read.csv(file = "./data-raw/Cryptochromes/cry2_dark.csv",
                           header = TRUE, comment.char = "#")
CRY2_dark.spct[-(1:2), -3] %>%
  setFilterSpct(Tfr.type = "internal") %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() -> CRY2_dark.spct
setWhatMeasured(CRY2_dark.spct, "In vitro absorbance of a dark exposed solution of CRY2")

getNormalized(CRY2_dark.spct)
getNormalization(CRY2_dark.spct)
autoplot(CRY2_dark.spct)

CRY2_light.spct <- read.csv(file = "./data-raw/Cryptochromes/cry2_light.csv",
                            header = TRUE, comment.char = "#")
CRY2_light.spct[order(CRY2_light.spct$w.length), -3] %>%
  setFilterSpct(Tfr.type = "internal") %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  setWhatMeasured("In vitro absorbance of a light exposed solution of CRY2") ->
  CRY2_light.spct

getNormalized(CRY2_light.spct)
getNormalization(CRY2_light.spct)
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
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  setWhatMeasured("phototropin 1 (Arabidopsis)") -> phot1.spct

comment(phot1.spct) <-
  "phototropin 1 (Arabidopsis)\nfrom Cristie et. al, 2002, figure 1a.\nBased on fluorescence yield."

getNormalized(phot1.spct)
getNormalization(phot1.spct)
autoplot(phot1.spct)

read.csv(file = "./data-raw/phototropins/LOV2-dark.csv",
         header = TRUE, comment.char = "#") %>%
  rename(w.length = "Line.6.x", A = "Line.6.y") %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  as.filter_spct(Tfr.type = "internal") %>%
  clean() %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  setWhatMeasured("phototropin 1 (Arabidopsis)") -> phot1_LOV2_dark.spct

comment(phot1_LOV2_dark.spct) <-
  "phototropin 1 (Arabidopsis) LOV2, dark adapted\nfrom Cristie et. al, 2015, figure 3B."

getNormalized(phot1_LOV2_dark.spct)
getNormalization(phot1_LOV2_dark.spct)
autoplot(phot1_LOV2_dark.spct)

read.csv(file = "./data-raw/phototropins/LOV2-light.csv",
         header = TRUE, comment.char = "#") %>%
  rename(w.length = "Line.7.x", A = "Line.7.y") %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  as.filter_spct(Tfr.type = "internal") %>%
  clean() %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  setWhatMeasured("phototropin 1 (Arabidopsis)") -> phot1_LOV2_light.spct

comment(phot1_LOV2_light.spct) <-
  "phototropin 1 (Arabidopsis) LOV2, blue-ligth adapted\nfrom Cristie et. al, 2015, figure 3B."

getNormalized(phot1_LOV2_light.spct)
getNormalization(phot1_LOV2_light.spct)
autoplot(phot1_LOV2_light.spct)

read.csv(file = "./data-raw/phototropins/phot2.csv",
                       header = TRUE, comment.char = "#") %>%
  rename(w.length = "Line.26.x", A = "Line.26.y") %>%
  as.filter_spct(Tfr.type = "internal") %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  setWhatMeasured("phototropin 2 (Arabidopsis)") -> phot2.spct

comment(phot2.spct) <-
  "phototropin 2 (Arabidopsis)\nfrom Cristie et. al, 2002, figure 7a.\nBased on fluorescence yield."

getNormalized(phot2.spct)
getNormalization(phot2.spct)
autoplot(phot2.spct)


read.csv(file = "./data-raw/phototropins/LOV2.csv",
         header = TRUE, comment.char = "#") %>%
  rename(w.length = "Line.27.x", A = "Line.27.y") %>%
  #  filter(A > 1e-2) %>%
  as.filter_spct(Tfr.type = "internal") %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  setWhatMeasured("phototropin 1 LOV2 (Arabidopsis)") -> LOV2.spct

comment(LOV2.spct) <-
  "phototropin 1 LOV2 (Arabidopsis)\nfrom XXXX, figure 7a.\nIn vitro absorbance."

getNormalized(LOV2.spct)
getNormalization(LOV2.spct)
autoplot(LOV2.spct)

PHOTs.mspct <- filter_mspct(list(PHOT1_fluo = phot1.spct,
                                 PHOT2_fluo = phot2.spct,
                                 PHOT1_dark = phot1_LOV2_dark.spct,
                                 PHOT1_light = phot1_LOV2_light.spct))
save(PHOTs.mspct, file = "./data/PHOTs.mspct.rda")

## ZTL zeitloupe

ZTL_dark.spct <- read.csv(file = "./data-raw/ZTL/ZTL-dark.csv",
                           header = TRUE, comment.char = "#")
names(ZTL_dark.spct) <- c("w.length", "A")
ZTL_dark.spct %>%
#  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  #  setNormalized(norm = TRUE) %>%
  setWhatMeasured("In vitro absorbance of a dark adapted solution of ZTL") -> ZTL_dark.spct

getNormalized(ZTL_dark.spct)
getNormalization(ZTL_dark.spct)
autoplot(ZTL_dark.spct)

ZTL_light.spct <- read.csv(file = "./data-raw/ZTL/ZTL-light.csv",
                          header = TRUE, comment.char = "#")
names(ZTL_light.spct) <- c("w.length", "A")
ZTL_light.spct %>%
#  mutate(A = A / max(A)) %>%
  group_by(w.length) %>%
  summarise(A = median(A)) %>%
  setFilterSpct(Tfr.type = "internal") %>%
  clean() %>%
  smooth_spct(method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() %>%
  setWhatMeasured("In vitro absorbance of a light adapted solution of ZTL") -> ZTL_light.spct

getNormalized(ZTL_light.spct)
getNormalization(ZTL_light.spct)
autoplot(ZTL_light.spct)

ZTLs.mspct <- filter_mspct(list(ZTL_dark = ZTL_dark.spct,
                                ZTL_light = ZTL_light.spct))
autoplot(ZTLs.mspct)
save(ZTLs.mspct, file = "./data/ZTLs.mspct.rda")

## UVR8

load("./data-raw/UVR8/Glasgow-UVR8.raw.data.rda")
UVR8_Glasgow.spct <- UVR8.raw.data[-64, ]
names(UVR8_Glasgow.spct) <- c("w.length", "A")
setFilterSpct(UVR8_Glasgow.spct, Tfr.type = "internal")
UVR8_Glasgow.spct <- normalize(UVR8_Glasgow.spct)
setWhatMeasured(UVR8_Glasgow.spct, "UVR8 (protein in vitro)")
comment(UVR8_Glasgow.spct) <-
  "UVR8 protein in vitro)\nfrom Christie et al. 2012, figure S3\nIn vitro absorbance."

smooth_spct(UVR8_Glasgow.spct, method = "supsmu") %>%
  interpolate_spct(length.out = 300) %>%
  setNormalized() %>%
  normalize() -> UVR8_Glasgow.spct

getNormalized(UVR8_Glasgow.spct)
getNormalization(UVR8_Glasgow.spct)
autoplot(UVR8_Glasgow.spct)

load("./data-raw/UVR8/UVR8Ecoli-merged-spct.Rda")
rownames(UVR8Ecoli_merged.spct) <- NULL
UVR8_Orebro.spct <- UVR8Ecoli_merged.spct
setNormalised(UVR8_Orebro.spct) # remove old normalization
UVR8_Orebro.spct <- normalize(UVR8_Orebro.spct)
setWhatMeasured(UVR8_Orebro.spct, "UVR8 (protein in vitro)")
comment(UVR8_Orebro.spct) <-
  "UVR8 (protein in vitro)\nfrom Rai et al. 2020, figure S6\nIn vitro absorbance."


getNormalized(UVR8_Orebro.spct)
getNormalization(UVR8_Orebro.spct)
autoplot(UVR8_Orebro.spct)

UVR8s.mspct <- filter_mspct(list(UVR8.abs.Glasgow = UVR8_Glasgow.spct,
                                 UVR8.abs.Orebro = UVR8_Orebro.spct))

autoplot(UVR8s.mspct)

save(UVR8s.mspct, file = "./data/UVR8s.mspct.rda")

## Phytochromes

load("./data-raw/Phytochromes/phytochrome.data.rda")
phytochrome.spct <- setGenericSpct(phytochrome.data)
PHYs.mspct <- generic_mspct(list(PHY = phytochrome.spct))
save(phytochrome.spct, PHYs.mspct, file = "./data/phytochrome.spct.rda")

unset_filter_qty_default()

