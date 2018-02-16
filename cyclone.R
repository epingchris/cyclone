#https://www.ncdc.noaa.gov/ibtracs/index.php?name=numbering
#https://www.ncdc.noaa.gov/ibtracs/index.php?name=wmo-var
#Time zone: UTC
library(data.table)

cycl = fread("/Users/eprau/EPR/Toulouse/UPS/Stage_M2/cyclone/Allstorms.ibtracs_wmo.v03r10.csv", skip = 1)
unit = cycl[1, ]
cycl = cycl[-1, ]
cycl$ISO_time = as.POSIXct(cycl$ISO_time, tz = "GMT")
cycl$Longitude = as.numeric(cycl$Longitude)
cycl$Latitude = as.numeric(cycl$Latitude)

cycl2001 = cycl[Season == 2001 & Basin == "WP", ]

#https://en.wikipedia.org/wiki/Decimal_degrees
#https://gis.stackexchange.com/questions/142326/calculating-longitude-length-in-miles
#https://gis.stackexchange.com/questions/8650/measuring-accuracy-of-latitude-and-longitude/8674#8674
lon = 121.5578
lat = 24.7611
trig = cos(lat)
degkm = 111.32
cycl2001$Distance = sqrt(((cycl2001$Longitude - lon) * degkm * trig) ^ 2 + ((cycl2001$Latitude - lat) * degkm) ^ 2)

thres = 250
cycl_sel = cycl2001[Distance <= thres, ]
cycl_sel