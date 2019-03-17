# read.edrp.files.r
library(photobiology)
library(lubridate)
library(ggspectra)

Tfr_as_default()

file_path <- "./data-raw/JaLTER/ERDP-2013-02/"
temp.df <- read.csv(paste(file_path,
                          "Solidago_leaf_lower_2010_0728_1_LICOR.csv",
                          sep = ""),
                    header = FALSE,  skip = 3)
temp.df <- subset(temp.df, V1 <= 1000)
Solidago_lower_adax.spct <- temp.df[ , c(1,2,3)]
Solidago_lower_abax.spct <- temp.df[ , c(1,4,5)]
names(Solidago_lower_adax.spct) <- c("w.length", "Rfr", "Tfr")
names(Solidago_lower_abax.spct) <- c("w.length", "Rfr", "Tfr")

setObjectSpct(Solidago_lower_adax.spct, Tfr.type = "total", Rfr.type = "total")
setObjectSpct(Solidago_lower_abax.spct, Tfr.type = "total", Rfr.type = "total")

when <- ymd_h("2010-07-28 12", tz = "Asia/Tokyo")
where <- data.frame(lat = 36.11667, lon =  140.13333, address = "The experimental grassland at the Terrestrial Environmental Research Center of the University of the Tsukuba")
setWhereMeasured(Solidago_lower_adax.spct, where)
setWhenMeasured(Solidago_lower_adax.spct, when)
setWhatMeasured(Solidago_lower_adax.spct, "Solidago altissima, lower canopy leaf, adaxial epidermis")
setWhereMeasured(Solidago_lower_abax.spct, where)
setWhenMeasured(Solidago_lower_abax.spct, when)
setWhatMeasured(Solidago_lower_abax.spct, "Solidago altissima, lower canopy leaf, abaxial epidermis")

temp.df <- read.csv(paste(file_path,
                          "Solidago_leaf_upper_2010_0728_1_LICOR.csv",
                          sep = ""),
                    header = FALSE,  skip = 3)
temp.df <- subset(temp.df, V1 <= 1000)
Solidago_upper_adax.spct <- temp.df[ , c(1,2,3)]
Solidago_upper_abax.spct <- temp.df[ , c(1,4,5)]
names(Solidago_upper_adax.spct) <- c("w.length", "Rfr", "Tfr")
names(Solidago_upper_abax.spct) <- c("w.length", "Rfr", "Tfr")

setObjectSpct(Solidago_upper_adax.spct, Tfr.type = "total", Rfr.type = "total")
setObjectSpct(Solidago_upper_abax.spct, Tfr.type = "total", Rfr.type = "total")

setWhereMeasured(Solidago_upper_adax.spct, where)
setWhenMeasured(Solidago_upper_adax.spct, when)
setWhatMeasured(Solidago_upper_adax.spct, "Solidago altissima, lower canopy leaf, adaxial epidermis")
setWhereMeasured(Solidago_upper_abax.spct, where)
setWhenMeasured(Solidago_upper_abax.spct, when)
setWhatMeasured(Solidago_upper_abax.spct, "Solidago altissima, lower canopy leaf, abaxial epidermis")

Solidago_altissima.mspct <- object_mspct(list(lower_adax = Solidago_lower_adax.spct,
                                              lower_abax = Solidago_lower_abax.spct,
                                              upper_adax = Solidago_upper_adax.spct,
                                              upper_abax = Solidago_upper_abax.spct))

autoplot(as.filter_mspct(Solidago_altissima.mspct))
autoplot(as.reflector_mspct(Solidago_altissima.mspct))

temp.df <- read.csv(paste(file_path,
                          "Betula_leaf_first_2010_0802_1_LICOR.csv",
                          sep = ""),
                    header = FALSE,  skip = 3)
temp.df <- subset(temp.df, V1 <= 1000)
first_flush_adax.spct <- temp.df[ , c(1,2,3)]
first_flush_abax.spct <- temp.df[ , c(1,4,5)]
names(first_flush_adax.spct) <- c("w.length", "Rfr", "Tfr")
names(first_flush_abax.spct) <- c("w.length", "Rfr", "Tfr")

setObjectSpct(first_flush_adax.spct, Tfr.type = "total", Rfr.type = "total")
setObjectSpct(first_flush_abax.spct, Tfr.type = "total", Rfr.type = "total")

when <- ymd_h("2010-08-02 12", tz = "Asia/Tokyo")
where <- data.frame(lat = 36.13333, lon =  137.41667, address = "Takayama deciduous broadleaf forest")
setWhereMeasured(first_flush_adax.spct, where)
setWhenMeasured(first_flush_adax.spct, when)
setWhatMeasured(first_flush_adax.spct, "Betula ermanii, first flush leaf from late May, adaxial")
setWhereMeasured(first_flush_abax.spct, where)
setWhenMeasured(first_flush_abax.spct, when)
setWhatMeasured(first_flush_abax.spct, "Betula ermanii, first flush leaf from late May, abaxial")

temp.df <- read.csv(paste(file_path,
                          "Betula_leaf_second_2010_0802_1_LICOR.csv",
                          sep = ""),
                    header = FALSE,  skip = 3)
temp.df <- subset(temp.df, V1 <= 1000)
summer_flush_adax.spct <- temp.df[ , c(1,2,3)]
summer_flush_abax.spct <- temp.df[ , c(1,4,5)]
names(summer_flush_adax.spct) <- c("w.length", "Rfr", "Tfr")
names(summer_flush_abax.spct) <- c("w.length", "Rfr", "Tfr")

setObjectSpct(summer_flush_adax.spct, Tfr.type = "total", Rfr.type = "total")
setObjectSpct(summer_flush_abax.spct, Tfr.type = "total", Rfr.type = "total")

setWhereMeasured(summer_flush_adax.spct, where)
setWhenMeasured(summer_flush_adax.spct, when)
setWhatMeasured(summer_flush_adax.spct, "Betula ermanii, leaf from early summer flush, adaxial")
setWhereMeasured(summer_flush_abax.spct, where)
setWhenMeasured(summer_flush_abax.spct, when)
setWhatMeasured(summer_flush_abax.spct, "Betula ermanii, leaf from early summer flush, abaxial")

temp.df <- read.csv(paste(file_path,
                          "Betula_leaf_yellow_2010_1006_1_LICOR.csv",
                          sep = ""),
                    header = FALSE,  skip = 3)
temp.df <- subset(temp.df, V1 <= 1000)
senesced_adax.spct <- temp.df[ , c(1,2,3)]
senesced_abax.spct <- temp.df[ , c(1,4,5)]
names(senesced_adax.spct) <- c("w.length", "Rfr", "Tfr")
names(senesced_abax.spct) <- c("w.length", "Rfr", "Tfr")

setObjectSpct(senesced_adax.spct, Tfr.type = "total", Rfr.type = "total")
setObjectSpct(senesced_abax.spct, Tfr.type = "total", Rfr.type = "total")

when <- ymd_h("2010-10-06 12", tz = "Asia/Tokyo")
setWhereMeasured(senesced_adax.spct, where)
setWhenMeasured(senesced_adax.spct, when)
setWhatMeasured(senesced_adax.spct, "Betula ermanii, yellow senesced leaf, adaxial")
setWhereMeasured(senesced_abax.spct, where)
setWhenMeasured(senesced_abax.spct, when)
setWhatMeasured(senesced_abax.spct, "Betula ermanii, yellow senesced leaf, abaxial")

Betula_ermanii.mspct <- object_mspct(list(first_flush_adax = first_flush_adax.spct,
                                          first_flush_abax = first_flush_abax.spct,
                                          summer_flush_adax = summer_flush_adax.spct,
                                          summer_flush_abax = summer_flush_abax.spct,
                                          senesced_adax = senesced_adax.spct,
                                          senesced_abax = senesced_abax.spct))

autoplot(as.filter_mspct(Betula_ermanii.mspct))
autoplot(as.reflector_mspct(Betula_ermanii.mspct))

save(Solidago_altissima.mspct,
     Betula_ermanii.mspct,
     file = "./data/leaves.spct.rda")

unset_filter_qty_default()

