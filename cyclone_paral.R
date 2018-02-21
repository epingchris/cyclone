#https://www.ncdc.noaa.gov/ibtracs/index.php?name=numbering
#https://www.ncdc.noaa.gov/ibtracs/index.php?name=wmo-var
#Time zone: UTC
library(data.table)

#Retrieve data ----
cycl = fread("Allstorms.ibtracs_wmo.v03r10.csv", skip = 1)
  
#separate the row that describes unit
unit = cycl[1, ]
cycl = cycl[-1, ]
  
#convert columns to correct format
cycl[, c(9:12, 14:15)] = lapply(cycl[, c(9:12, 14:15)], as.numeric)
cycl$ISO_time = as.POSIXct(cycl$ISO_time, tz = "GMT")
cycl$Season_Name = paste0(cycl$Season, cycl$Name)

#Filter cyclones ----
year = 2001:2010
cycl_filt = cycl[Season %in% year, ]

#Retrieve and store in raster ----
library(geosphere)

thres = 150 #maximum distance between position and cyclones
coord = as.matrix(data.frame(lon = rep(seq(-179.75, 179.75, by = 0.5), 360), lat = rep(seq(89.75, -89.75, by = -0.5), each = 720)))
mat = distm(cycl_filt[, list(Longitude, Latitude)], coord) / 1000
lst = split(t(mat), seq(nrow(t(mat))))
cycl_sel = lapply(lst, function(x) cycl_filt[which(x < thres), ])
frequ = lapply(cycl_sel, function(x) length(unique(x$Season_Name)) / length(year))
dur = lapply(cycl_sel, function(x) nrow(x) / length(year))
wind_int = lapply(cycl_sel, function(x) mean(tapply(x$`Wind(WMO)`, x$Season_Name, max)))
press_int = lapply(cycl_sel, function(x) mean(tapply(x$`Pres(WMO)`, x$Season_Name, min)))

library(raster)
library(maps)

PlotCycl = function(val, filename){
  ras = raster(xmn = -180, xmx = 180, ymn = 90, ymx = 90, resolution = 0.5)
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
  map(database = "world", xlim = c(-180, 180), ylim = c(-90, 90), add = T)
  abline(h = 24, v = 121, lty = 3)
  dev.off()
}

PlotCycl("frequ", "cyclone_freq")
PlotCycl("dur", "cyclone_dur")
PlotCycl("wind_int", "cyclone_wind")
PlotCycl("press_int", "cyclone_press")