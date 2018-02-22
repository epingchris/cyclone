#https://www.ncdc.noaa.gov/ibtracs/index.php?name=numbering
#https://www.ncdc.noaa.gov/ibtracs/index.php?name=wmo-var
#Time zone: UTC
rm(list = ls())

#Retrieve data ----
library(data.table)
use = "tokyo" #set the database to use; "tokyo" or "jtwc"
if(use == "tokyo"){
  cycl = fread("/Users/eprau/EPR/Toulouse/UPS/Stage_M2/cyclone/Allstorms.ibtracs_wmo.v03r10.csv", skip = 1)
  
  #separate the row that describes unit
  unit = cycl[1, ]
  cycl = cycl[-1, ]
  
  #convert columns to correct format
  cycl[, c(9:12, 14:15)] = lapply(cycl[, c(9:12, 14:15)], as.numeric)
  
} else if (use == "jtwc"){
  cycl = fread("/Users/eprau/EPR/Toulouse/UPS/Stage_M2/cyclone/Allstorms.ibtracs_all.v03r10.csv", skip = 1)
  cycl = cycl[, c(1:19, 70:74)]
  
  #separate the row that describes unit
  unit = cycl[1, ]
  cycl = cycl[-1, ]
  
  #convert columns to correct format
  cycl[, c(9:12, 14:15, 17:18, 20:24)] = lapply(cycl[, c(9:12, 14:15, 17:18, 20:24)], as.numeric)
  
} else {
  print("Not the correct type of database")
}

#convert columns to correct format
cycl$ISO_time = as.POSIXct(cycl$ISO_time, tz = "GMT")
cycl$Season_Name = paste0(cycl$Season, cycl$Name)

#Retrieve information for one point----
dirct = "/Users/eprau/EPR/Toulouse/UPS/Stage_M2/cyclone_res/"
coord = list(fushan = c(121.5678, 24.7611),
             luquillo = c(-65.816, 18.3262),
             stdenis = c(55.44806, -20.87889))
thres = 150
year = 2001:2010
mapname = "stdenis"
lon = coord[[mapname]][1]
lat = coord[[mapname]][2]

cycl_filt = cycl[Season %in% year, ] #filter by year
coord_cycl = SpatialPointsDataFrame(as.matrix(cycl_filt[, list(Longitude, Latitude)]), data.frame(pt = seq(1:nrow(cycl_filt))),
                                    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
mat = distm(coord_cycl, matrix(c(lon, lat), ncol = 2)) / 1000 #calculate distance
cycl_sel = cycl_filt[which(mat < thres), ] #filter by distance
year_name = ifelse(length(year) == 1, year, paste(year[1], rev(year)[1], sep = "_"))
fwrite(cycl_sel, paste0(dirct, mapname, "_", year_name, "_sel.csv")) #save selected table
todraw = unique(cycl_sel$Season_Name) #name of typhoons to be drawn

#draw background map
library(ggplot2)
library(ggmap)
bgmap = get_map(location = c(lon, lat), scale = 2, zoom = 5, maptype = "satellite", source = "google")

#draw cyclone paths near one point
ggmap(bgmap) + 
  geom_point(data = as.data.frame(c(lon, lat)), shape = 8, col = "white") +
  geom_path(data = cycl_filt[Season_Name %in% todraw, ], aes(x = Longitude, y = Latitude, group = Season_Name, color = Season_Name))
ggsave(paste0(dirct, mapname, "_", year_name, "_path.png"))

#Retrieve raster ----
library(geosphere)
library(sp)
library(raster)

RetrCycl = function(year, x0, x1, y0, y1, resol, thres = 150, mapout = T, mapname = "map"){
  cycl_filt = cycl[Season %in% year, ] #filter by year
  
  #set up global coordinates
  lon = rep(seq(x0 + resol / 2, x1 - resol / 2, by = resol), (y1 - y0) / resol)
  lat = rep(seq(y1 - resol / 2, y0 + resol / 2, by = -resol), each = (x1 - x0) / resol)
  coord = SpatialPointsDataFrame(matrix(c(lon, lat), ncol = 2), data.frame(ID = seq(1:length(lon))),
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  #set up coordinates of typhoon events
  coord_cycl = SpatialPointsDataFrame(as.matrix(cycl_filt[, list(Longitude, Latitude)]), data.frame(pt = seq(1:nrow(cycl_filt))),
                                      proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  mat = distm(coord_cycl, coord) / 1000 #calculate distance
  lst = split(t(mat), seq(nrow(t(mat))))
  cycl_sel = lapply(lst, function(x) cycl_filt[which(x < thres), ]) #filter by distance
  frequ = lapply(cycl_sel, function(x) length(unique(x$Season_Name)) / length(year)) #frequency (number of typhoons)
  dur = lapply(cycl_sel, function(x) nrow(x) / length(year)) #duration (number of records)
  wind = lapply(cycl_sel, function(x) mean(tapply(x$`Wind(WMO)`, x$Season_Name, max))) #maximum wind speed
  press = lapply(cycl_sel, function(x) mean(tapply(x$`Pres(WMO)`, x$Season_Name, min))) #minimum atmospheric pressure
  
  #store in rasters
  ras.freq = raster(xmn = x0, xmx = x1, ymn = y0, ymx = y1, resolution = resol)
  ras.dur = raster(xmn = x0, xmx = x1, ymn = y0, ymx = y1, resolution = resol)
  ras.wind = raster(xmn = x0, xmx = x1, ymn = y0, ymx = y1, resolution = resol)
  ras.press = raster(xmn = x0, xmx = x1, ymn = y0, ymx = y1, resolution = resol)
  values(ras.freq) = unlist(frequ)
  values(ras.dur) = unlist(dur)
  values(ras.wind) = unlist(wind)
  values(ras.press) = unlist(press)
  
  dirct = "/Users/eprau/EPR/Toulouse/UPS/Stage_M2/cyclone_res/"
  writeRaster(ras.freq, paste0(dirct, mapname, "_freq.grd"), format = "raster")
  writeRaster(ras.dur, paste0(dirct, mapname, "_dur.grd"), format = "raster")
  writeRaster(ras.wind, paste0(dirct, mapname, "_wind.grd"), format = "raster")
  writeRaster(ras.press, paste0(dirct, mapname, "_press.grd"), format = "raster")
  
  if(mapout){
    library(maps)
    
    #plot frequency
    pdf(paste0(dirct, mapname, "_freq.pdf"))
    plot(ras.freq, col = rev(heat.colors(300)))
    map(database = "world", xlim = c(x0, x1), ylim = c(y0, y1), add = T)
    abline(h = 24, v = 121, lty = 3)
    dev.off()
    
    #plot duration
    pdf(paste0(dirct, mapname, "_dur.pdf"))
    plot(ras.dur, col = rev(heat.colors(300)))
    map(database = "world", xlim = c(x0, x1), ylim = c(y0, y1), add = T)
    abline(h = 24, v = 121, lty = 3)
    dev.off()
    
    #plot wind speed
    pdf(paste0(dirct, mapname, "_wind.pdf"))
    plot(ras.wind, col = rev(heat.colors(300)))
    map(database = "world", xlim = c(x0, x1), ylim = c(y0, y1), add = T)
    abline(h = 24, v = 121, lty = 3)
    dev.off()
    
    #plot atmospheric pressure
    pdf(paste0(dirct, mapname, "_press.pdf"))
    arg = list(at = seq(800, 1100, by = 50), labels = seq(800, 1100, by = 50))
    plot(ras.press, col = heat.colors(300), breaks = 800:1100, axis.args = arg, zlim = c(800, 1100))
    map(database = "world", xlim = c(x0, x1), ylim = c(y0, y1), add = T)
    abline(h = 24, v = 121, lty = 3)
    dev.off()
  }
}

RetrCycl(year = 2001:2003, x0 = 0, x1 = 180, y0 = 0, y1 = 90, resol = 1, mapname = "NE")
RetrCycl(year = 2001:2003, x0 = 0, x1 = 180, y0 = 0, y1 = 90, resol = 0.5, mapname = "fine_NE")
RetrCycl(year = 2001:2003, x0 = -180, x1 = 180, y0 = -90, y1 = 90, resol = 1, mapname = "world")
RetrCycl(year = 2001:2003, x0 = -180, x1 = 180, y0 = -90, y1 = 90, resol = 0.5, mapname = "fine_world")