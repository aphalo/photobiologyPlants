library("photobiology")
CRY2_dark.spct <- read.csv(file = "./raw.data/Cryptochromes/cry2_dark.csv",
                           header = TRUE, comment.char = "#")
CRY2_dark.spct <- CRY2_dark.spct[-(1:2), -3]
setFilterSpct(CRY2_dark.spct, Tfr.type = "internal")
CRY2_dark.spct <- interpolate_spct(CRY2_dark.spct, length.out = 100)

CRY2_light.spct <- read.csv(file = "./raw.data/Cryptochromes/cry2_light.csv",
                            header = TRUE, comment.char = "#")
CRY2_light.spct <- CRY2_light.spct[order(CRY2_light.spct$w.length), -3]
setFilterSpct(CRY2_light.spct, Tfr.type = "internal")
CRY2_light.spct <- interpolate_spct(CRY2_light.spct, length.out = 100)

save(CRY2_dark.spct, CRY2_light.spct, file = "./data/cry.spct.rda")


## UVR8

load("./raw.data/UVR8/UVR8.raw.data.rda")
UVR8_Glasgow.spct <- UVR8.raw.data[-64, ]
names(UVR8_Glasgow.spct) <- c("w.length", "A")
setFilterSpct(UVR8_Glasgow.spct, Tfr.type = "internal")
UVR8_Glasgow.spct <- interpolate_spct(UVR8_Glasgow.spct, length.out = 100)

save(UVR8_Glasgow.spct, file = "./data/UVR8.spct.rda")

## Phytochromes

load("./raw.data/Phytochromes/phytochrome.data.rda")
phytochrome.spct <- setGenericSpct(phytochrome.data)
save(phytochrome.spct, file = "./data/phytochrome.spct.rda")
