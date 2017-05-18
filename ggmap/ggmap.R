## load RTR dataset from https://www.netztest.at/de/Opendata
# Data is licensed as CC BY 3.0 Österreich

library(ggmap)
library(ggalt)
library(viridis)

setwd("~/git/mobile-context-qoe-management-paper/cartography/")
df <- read.csv(unz("netztest-opendata.zip", "netztest-opendata.csv"), stringsAsFactors = FALSE)


## get base map for the region
map.googol <- qmap("Wien", zoom=12, source = "google", color = "bw")  

## simple bandwidth point map
map.googol + geom_point(data = df, aes(x = long, y = lat, color = download_kbit/1024), alpha = 0.7,size = 1) +
  scale_colour_viridis(trans = "log")# + 
ggsave("carto-points.pdf")

## summary2d example
library(hexbin)
map.googol + coord_cartesian() +
  stat_summary_hex(data = df, aes(x = long, y = lat, z=download_kbit/1024), fun = mean, alpha = 0.5, bins = 100) + 
  scale_fill_viridis(trans = "log")
ggsave("carto-summary2d-hex-mean.pdf")
