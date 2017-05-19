## load RTR dataset from https://www.netztest.at/de/Opendata
# Data is licensed as CC BY 3.0 Österreich

## > devtools::install_github("dkahle/ggmap") #the latest version
library(ggmap)
library(ggalt)
library(viridis)

setwd("~/git/qoe-cartography/")
df <- read.csv(unz("ggmap/netztest-opendata.zip", "netztest-opendata.csv"), stringsAsFactors = FALSE)


## get base map for the region
map.googol <- qmap("Wien", zoom=12, source = "stamen", color = "bw")  

## simple bandwidth point map
map.googol + geom_point(data = df, aes(x = long, y = lat, color = download_kbit/1024), alpha = 0.7,size = 1) +
  scale_colour_viridis(trans = "log")# + 
ggsave("carto-points.pdf")

yt_colors <- function(x) {
  mn <- mean(x)
  if(mn >= 45 * 1.25)
    return("2160p")
  else if(mn >= 16 * 1.25)
    return("1440p")
  else if(mn >= 8 * 1.25)
    return("1080p")
  else if(mn >= 5 * 1.25)
    return("720p")
  else if(mn >= 2.5 * 1.25)
    return("480p")
  else if(mn >= 1 * 1.25)
    return("360p")
  else
    return("n/a")
}

yt_colors_q5 <- function(x) {
  mn <- quantile(x, probs = c(0.05))
  if(mn >= 45 * 1.25)
    return("2160p")
  else if(mn >= 16 * 1.25)
    return("1440p")
  else if(mn >= 8 * 1.25)
    return("1080p")
  else if(mn >= 5 * 1.25)
    return("720p")
  else if(mn >= 2.5 * 1.25)
    return("480p")
  else if(mn >= 1 * 1.25)
    return("360p")
  else
    return("n/a")
}

## summary2d example
library(hexbin)
map.googol + coord_cartesian() +
  stat_summary_hex(data = df, aes(x = long, y = lat, z=download_kbit/1024), fun = yt_colors, alpha = 0.7, bins = 60) + 
  scale_fill_manual(values = c("2160p" = "#66FF00", "1440p" = "#99FFCC", 
                               "1080p" = "#0066FF", "720p" = "#CCFFFF", 
                               "480p" = "#FFFFCC", "360p" = "#FF9933", 
                               "n/a" = "#FF6666"))
ggsave("carto-summary2d-hex-ytcolors.pdf")


### Plot network access technology facets
table(df$network_type) # filter out categories with insufficient samples
df.sub <- subset(df,network_type %in% c("LTE", "HSPA+", "LAN", "EDGE", "UMTS", "WLAN"))

map.googol + coord_cartesian() +
  stat_summary_hex(data = df.sub, aes(x = long, y = lat, z=download_kbit/1024), fun = yt_colors, alpha = 0.9, bins = 100) + 
  scale_fill_manual(values = c("2160p" = "#66FF00", "1440p" = "#99FFCC", 
                               "1080p" = "#0066FF", "720p" = "#CCFFFF", 
                               "480p" = "#FFFFCC", "360p" = "#FF9933", 
                               "n/a" = "#FF6666")) + 
  facet_wrap(facets = ~network_type)
ggsave("carto-summary2d-ytcolors-network_facets.pdf")


### facets with 5% quantile as base instead of mean BW
map.googol + coord_cartesian() +
  stat_summary_hex(data = df.sub, aes(x = long, y = lat, z=download_kbit/1024), fun = yt_colors_q5, alpha = 0.9, bins = 100) + 
  scale_fill_manual(values = c("2160p" = "#66FF00", "1440p" = "#99FFCC", 
                               "1080p" = "#0066FF", "720p" = "#CCFFFF", 
                               "480p" = "#FFFFCC", "360p" = "#FF9933", 
                               "n/a" = "#FF6666")) + 
  facet_wrap(facets = ~network_type)
ggsave("carto-summary2d-ytcolors-network_facets_q5.pdf")
