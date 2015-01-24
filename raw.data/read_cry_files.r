library("photobiology")
library("photobiologygg")
old.wd <- setwd("raw.data")
CRY2_dark.spct <- read.csv(file = "cry2_dark.csv", header = TRUE, comment.char = "#")
setFilterSpct(CRY2_dark.spct)
CRY2_dark.spct <- interpolate_spct(CRY2_dark.spct, length.out = 100)
# plot(CRY2_dark.spct)
# plot(CRY2_dark.spct, plot.qty = "absorbance")
CRY2_light.spct <- read.csv(file = "cry2_light.csv", header = TRUE, comment.char = "#")
setFilterSpct(CRY2_light.spct)
CRY2_light.spct <- interpolate_spct(CRY2_light.spct, length.out = 100)
# plot(CRY2_light.spct)
# plot(CRY2_light.spct, plot.qty = "absorbance")
setwd(old.wd)
save(CRY2_dark.spct, CRY2_light.spct, file = "./data/cry.rda")


## UVR8

UVR8_Glasgow.spct <- copy(UVR8.raw.data)
names(UVR8_Glasgow.spct) <- c("w.length", "A")
setFilterSpct(UVR8_Glasgow.spct)
UVR8_Glasgow.spct <- interpolate_spct(UVR8_Glasgow.spct, length.out = 100)
plot(UVR8_Glasgow.spct, plot.qty = "absorbance")
