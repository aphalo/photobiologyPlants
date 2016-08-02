library("photobiology")

McCree_Amaranth.spct <- read.table(file = "./data-raw/A_McCree_1972_Amaranth.txt", header = TRUE, comment.char = "#")
McCree_Amaranth.spct <- McCree_Amaranth.spct[order(McCree_Amaranth.spct$w.length), ]
McCree_Amaranth.spct <- McCree_Amaranth.spct[-which(diff(McCree_Amaranth.spct$w.length) < 0.1), ]
setResponseSpct(McCree_Amaranth.spct)
McCree_Amaranth.spct <- interpolate_spct(McCree_Amaranth.spct, length.out = 200)

McCree_Oat.spct <- read.table(file = "./data-raw/A_McCree_1972_Oats.txt", header = TRUE, comment.char = "#")
McCree_Oat.spct <- McCree_Oat.spct[order(McCree_Oat.spct$w.length), ]
McCree_Oat.spct <- McCree_Oat.spct[-which(diff(McCree_Oat.spct$w.length) < 0.1), ]
setResponseSpct(McCree_Oat.spct)
McCree_Oat.spct <- interpolate_spct(McCree_Oat.spct, length.out = 200)

save(McCree_Amaranth.spct, McCree_Oat.spct, file = "./data/McCree.rda")
