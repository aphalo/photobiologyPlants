# read.edrp.files.r

library("photobiology")
temp.df <- read.csv("raw.data/JaLTER/ERDP-2013-02/Solidago_leaf_lower_2010_0728_1_LICOR.csv", header = FALSE,  skip = 3)
temp.df <- subset(temp.df, V1 <= 1000)
Solidago_lower_adax.spct <- temp.df[ , c(1,2,3)]
Solidago_lower_abax.spct <- temp.df[ , c(1,4,5)]
names(Solidago_lower_adax.spct) <- c("w.length", "Rfr", "Tfr")
names(Solidago_lower_abax.spct) <- c("w.length", "Rfr", "Tfr")

setObjectSpct(Solidago_lower_adax.spct)
setObjectSpct(Solidago_lower_abax.spct)

temp.df <- read.csv("raw.data/JaLTER/ERDP-2013-02/Solidago_leaf_upper_2010_0728_1_LICOR.csv", header = FALSE,  skip = 3)
temp.df <- subset(temp.df, V1 <= 1000)
Solidago_upper_adax.spct <- temp.df[ , c(1,2,3)]
Solidago_upper_abax.spct <- temp.df[ , c(1,4,5)]
names(Solidago_upper_adax.spct) <- c("w.length", "Rfr", "Tfr")
names(Solidago_upper_abax.spct) <- c("w.length", "Rfr", "Tfr")

setObjectSpct(Solidago_upper_adax.spct)
setObjectSpct(Solidago_upper_abax.spct)

save(Solidago_lower_adax.spct, Solidago_lower_abax.spct,
     Solidago_upper_adax.spct, Solidago_upper_abax.spct,
     file = "data/leaves.spct.rda")
