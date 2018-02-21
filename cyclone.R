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
year = 2001:2010
use = "tokyo" #which database to use

if(use == "tokyo"){
  cycl_filt = cycl[Season %in% year, ]
} else if (use == "jtwc"){
  cycl_filt = cycl_jtwc[Season %in% year, ]
} else {
  print("Not the correct type of database")
}

#Retrieve on raster ----
library(raster)
library(geosphere)
library(maps)
library(mapdata)
library(mapproj)

thres = 150 #maximum distance between position and cyclones
start.time = Sys.time()
coord = as.matrix(data.frame(lon = rep(seq(0.5, 179.5, by = 1), 90), lat = rep(seq(89.5, 0.5, by = -1), each = 180)))
mat = distm(cycl_filt[, list(Longitude, Latitude)], coord) / 1000
lst = split(t(mat), seq(nrow(t(mat))))
cycl_sel = lapply(lst, function(x) cycl_filt[which(x < thres), ])
frequ = lapply(cycl_sel, function(x) length(unique(x$Season_Name)) / length(year))
dur = lapply(cycl_sel, function(x) nrow(x) / length(year))
wind_int = lapply(cycl_sel, function(x) mean(tapply(x$`Wind(WMO)`, x$Season_Name, max)))
press_int = lapply(cycl_sel, function(x) mean(tapply(x$`Pres(WMO)`, x$Season_Name, min)))
end.time = Sys.time()
time.diff = end.time - start.time
time.diff

PlotCycl = function(val, filename){
  ras = raster(xmn = 0, xmx = 180, ymn = 0, ymx = 90, resolution = 1)
  values(ras) = unlist(get(val))
  pdf(paste0("/Users/eprau/EPR/Toulouse/UPS/Stage_M2/", filename, ".pdf"))
  if(val == "press_int"){
    breakpoints = 800:1100
    arg = list(at = seq(800, 1100, by = 50), labels = seq(800, 1100, by = 50))
    plot(ras, col = heat.colors(300), breaks = breakpoints, axis.args = arg, zlim = c(800, 1100))
  } else if(val == "wind_int"){
    plot(ras, col = rev(heat.colors(300)))
  } else {
    plot(ras)
  }
  map(database = "world", xlim = c(0, 180), ylim = c(0, 90), add = T)
  abline(h = 24, v = 121, lty = 3)
  dev.off()
}

PlotCycl("frequ", "cyclone_freq")
PlotCycl("dur", "cyclone_dur")
PlotCycl("wind_int", "cyclone_wind")
PlotCycl("press_int", "cyclone_press")

ras = raster(xmn = 0, xmx = 180, ymn = 0, ymx = 90, resolution = 1)
values(ras) = unlist(press_int)


#Retrieve information ----

#https://en.wikipedia.org/wiki/Decimal_degrees
#https://gis.stackexchange.com/questions/142326/calculating-longitude-length-in-miles
#https://gis.stackexchange.com/questions/8650/measuring-accuracy-of-latitude-and-longitude/8674#8674
#111.32: distance of one degree in kilometers at equateur
#https://stackoverflow.com/questions/31668163/geographic-geospatial-distance-between-2-lists-of-lat-lon-points-coordinates
RetrCycl = function(base, lon, lat, thres){
  #calculate distance from specified lcoation, and select only those under a threshold
  cycl_sel = base[sqrt(((Longitude - lon) * 111.32 * cos(lat)) ^ 2 +
                         ((Latitude - lat) * 111.32) ^ 2) <= thres, ]
  todraw = unique(cycl_sel$Season_Name)
  freq = length(todraw) / length(year)
  dur = nrow(cycl_sel) / length(year)
  wind_int = mean(tapply(cycl_sel$`Wind(WMO)`, cycl_sel$Season_Name, max))
  press_int = mean(tapply(cycl_sel$`Pres(WMO)`, cycl_sel$Season_Name, min))
  return(list(Names = todraw, Frequency = freq, Duration = dur, 
              Wind_Intensity = wind_int, Pressure_Intensity = press_int))
}

dat = RetrCycl(cycl_filt, lon = 121.5578, lat = 24.7611, thres = 150)

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
  year_name = ifelse(length(year) == 1, year, paste(year[1], rev(year)[1], sep = "_"))
  ggsave(paste0("season", year_name, "jma.png"))
} else if (use == "jtwc"){
  ggmap(tw) + 
    geom_point(data = as.data.frame(c(lon, lat)), shape = 8) +
    geom_path(data = cycl_filt[Season_Name %in% todraw, ], aes(x = Longitude_for_mapping, y = Latitude_for_mapping, group = Season_Name, color = Season_Name))
  year_name = ifelse(length(year) == 1, year, paste(year[1], rev(year)[1], sep = "_"))
  ggsave(paste0("season", year_name, "jtwc.png"))
         
} else {
  print("Incorrect database name")
}

fwrite(cycl_sel, "cyclone_sel.csv")