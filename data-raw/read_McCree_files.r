library("photobiology")

McCree_Amaranth.spct <- read.table(file = "./data-raw/A_McCree_1972_Amaranth.txt", header = TRUE, comment.char = "#")
McCree_Amaranth.spct <- McCree_Amaranth.spct[order(McCree_Amaranth.spct$w.length), ]
McCree_Amaranth.spct <- McCree_Amaranth.spct[-which(diff(McCree_Amaranth.spct$w.length) < 0.1), ]
setResponseSpct(McCree_Amaranth.spct)
McCree_Amaranth.spct <- normalize(McCree_Amaranth.spct)
McCree_Amaranth.spct <- interpolate_spct(McCree_Amaranth.spct, length.out = 200)
setWhatMeasured(McCree_Amaranth.spct, "Net CO2 uptake measured on leaf sections after about 2 minutes equilibration time at each wavelength.")
comment(McCree_Amaranth.spct) <- "One of the 'classical' action spectra of K. J. McCree (1972) Amaranthus edulis Speg. var. UCD 1966 was used for this data."

McCree_Oat.spct <- read.table(file = "./data-raw/A_McCree_1972_Oats.txt", header = TRUE, comment.char = "#")
McCree_Oat.spct <- McCree_Oat.spct[order(McCree_Oat.spct$w.length), ]
McCree_Oat.spct <- McCree_Oat.spct[-which(diff(McCree_Oat.spct$w.length) < 0.1), ]
setResponseSpct(McCree_Oat.spct)
McCree_Oat.spct <- normalize(McCree_Oat.spct)
McCree_Oat.spct <- interpolate_spct(McCree_Oat.spct, length.out = 200)
setWhatMeasured(McCree_Oat.spct, "Net CO2 uptake measured after about 2 to 10 minutes measurement time at each wavelength")
comment(McCree_Oat.spct) <- "One of the 'classical' action spectra of K. J. McCree (1972) Avena sativa L. var. Coronado."

McCree_photosynthesis.mspct <- response_mspct(list(amaranth = McCree_Amaranth.spct,
                                                   oats = McCree_Oat.spct))

save(McCree_photosynthesis.mspct, file = "./data/McCree.rda")
