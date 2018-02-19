#https://www.ncdc.noaa.gov/ibtracs/index.php?name=numbering
#https://www.ncdc.noaa.gov/ibtracs/index.php?name=wmo-var
#Time zone: UTC
library(data.table)

#Retrieve data ----
cycl = fread("/Users/eprau/EPR/Toulouse/UPS/Stage_M2/cyclone/Allstorms.ibtracs_wmo.v03r10.csv", skip = 1)
cycl_all = fread("/Users/eprau/EPR/Toulouse/UPS/Stage_M2/cyclone/Allstorms.ibtracs_all.v03r10.csv", skip = 1)
cycl_jtwc = cycl_all[, c(1:19, 70:74)]

#separate the row that describes unit
unit = cycl[1, ]
cycl = cycl[-1, ]
unit_jtwc = cycl_jtwc[1, ]
cycl_jtwc = cycl_jtwc[-1, ]

#convert columns to correct format
cycl$ISO_time = as.POSIXct(cycl$ISO_time, tz = "GMT")
cycl[, c(9:12, 14:15)] = lapply(cycl[, c(9:12, 14:15)], as.numeric)
cycl$Season_Name = paste0(cycl$Season, cycl$Name)
cycl_jtwc$ISO_time = as.POSIXct(cycl_jtwc$ISO_time, tz = "GMT")
cycl_jtwc[, c(9:12, 14:15, 17:18, 20:24)] = lapply(cycl_jtwc[, c(9:12, 14:15, 17:18, 20:24)], as.numeric)
cycl_jtwc$Season_Name = paste0(cycl_jtwc$Season, cycl_jtwc$Name)

#Filter cyclones ----
year = 2001
lon = 121.5578
lat = 24.7611
thres = 200 #set threshold
use = "tokyo" #which database to use

if(use == "tokyo"){
  cycl_filt = cycl[Season %in% year, ]
} else if (use == "jtwc"){
  cycl_filt = cycl_jtwc[Season %in% year, ]
} else {
  print("Not the correct type of database")
}

#https://en.wikipedia.org/wiki/Decimal_degrees
#https://gis.stackexchange.com/questions/142326/calculating-longitude-length-in-miles
#https://gis.stackexchange.com/questions/8650/measuring-accuracy-of-latitude-and-longitude/8674#8674
#111.32: distance of one degree in kilometers at equateur
cycl_filt$Distance = sqrt(((cycl_filt$Longitude - lon) * 111.32 * cos(lat)) ^ 2 +
                            ((cycl_filt$Latitude - lat) * 111.32) ^ 2) #calculate distance from specified lcoation
cycl_sel = cycl_filt[Distance <= thres, ]
todraw = unique(cycl_sel$Season_Name)

#Draw maps ----
# library(maps)
# library(mapdata)
# dev.new(width = 5,height = 5)
# map(database = "world", xlim = c(110, 135), ylim = c(10, 40))
# lines(cycl_filt[Name %in% "LEKIMA", ]$Longitude, cycl_filt[Name %in% "LEKIMA", ]$Latitude)

library(ggplot2)
library(ggmap)
tw = get_map(c(lon = lon, lat = lat), scale = 2, zoom = 5, maptype = "satellite", source = "google")

if(use == "tokyo"){
  ggmap(tw) + 
    geom_point(data = as.data.frame(c(lon, lat)), shape = 8) +
    geom_path(data = cycl_filt[Season_Name %in% todraw, ], aes(x = Longitude, y = Latitude, group = Season_Name, color = Season_Name))
} else if (use == "jtwc"){
  ggmap(tw) + 
    geom_point(data = as.data.frame(c(lon, lat)), shape = 8) +
    geom_path(data = cycl_filt[Season_Name %in% todraw, ], aes(x = Longitude_for_mapping, y = Latitude_for_mapping, group = Season_Name, color = Season_Name))
} else {
  print("Incorrect database name")
}

fwrite(cycl_sel, "/Users/eprau/EPR/Toulouse/UPS/Stage_M2/cyclone_sel.csv")