## 
## Hurricane Milton data visualization
## ECMWF output
## 
## Code was compiled by Paul Julian
## contact info: pauljulianphd@gmail.com

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(openxlsx)

library(sf)
library(USAboundaries)
library(EVERSpatDat)
library(raster)

library(rnaturalearth)
# library(rnaturalearthdata)

library(ncdf4)  # package to handle NetCDF

#Paths
wd="C:/Julian_LaCie/_GitHub/Hurricanes"
## double check set the wd correctly
# getwd()==wd
paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/","/_documents/"))
plot.path=paths[1]
export.path = paths[2]
data.path = paths[3]


utm17=st_crs("EPSG:26917")
wgs84=st_crs("EPSG:4326")
# GIS Data ----------------------------------------------------------------
SW.US=c("Florida","Georgia","Alabama","South Carolina",
        "Mississippi","Louisiana",
        "Texas")

FL.shp=us_states(resolution ="low",states=SW.US)|>
  st_transform(wgs84)

## Florida Counties 
link = "https://services1.arcgis.com/O1JpcwDW8sjYuddV/arcgis/rest/services/urbanboundaries2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
FLUrban = link|>
  st_read()|>
  st_transform(wgs84)

FLUrban|>st_is_valid()
FLUrban = FLUrban|>st_make_valid()

# Hurricane Tracks --------------------------------------------------------
# Hurricane best track 2024
# head = c("BASIN", "CY", "YYYYMMDDHH", "TECHNUM", "TECH", "TAU", "Lat", "Lon",
#          "VMAX", "MSLP", "TY", "RAD", "WINDCODE", "RAD1", "RAD2", "RAD3", "RAD4",
#          "POUTER", "ROUTER", "RMW", "GUSTS", "EYE", "SUBREGION", "MAXSEAS",
#          "INITIALS", "DIR", "SPEED", "STORMNAME", "DEPTH", "SEAS", "SEASCODE",
#          "SEAS1","SEAS2", "SEAS3", "SEAS4", "USERDEFINED", "userdata")
# 
# link1 = "ftp://ftp.nhc.noaa.gov/atcf/btk/"
# 
# flist = strsplit( RCurl::getURL(link1, dirlistonly = TRUE),"\r\n")[[1]]
# flist = flist[grep("bal",flist)]
# 
# ## helene is Hurricane 14
# tmp = readLines(paste0(link1,"bal092024.dat"))
# 
# # Split each line by commas
# split_lines <- strsplit(tmp, ",\\s*")
# # Determine the maximum length of the rows to pad the shorter rows
# max_length <- max(sapply(split_lines, length))
# # Pad the rows to ensure consistent column length
# padded_lines <- lapply(split_lines, function(row) {
#   c(row, rep("", max_length - length(row)))
# })
# # Convert the list to a data frame
# df <- as.data.frame(do.call(rbind, padded_lines), stringsAsFactors = FALSE)[,1:37]
# colnames(df) = head
# 
# helene.track.dat = df
# lat.str.val = strsplit(helene.track.dat$Lat, "(?=[A-Za-z])", perl = TRUE)
# lon.str.val = strsplit(helene.track.dat$Lon, "(?=[A-Za-z])", perl = TRUE)
# 
# helene.track.dat$Lat.DD = with(helene.track.dat,ifelse(sapply(lat.str.val,"[",2)=="N",
#                                              as.numeric(sapply(lat.str.val,"[",1))/10,
#                                              (as.numeric(sapply(lat.str.val,"[",1))/10)*-1)
# )
# helene.track.dat$Lon.DD = with(helene.track.dat,ifelse(sapply(lon.str.val,"[",2)=="N",
#                                              as.numeric(sapply(lon.str.val,"[",1))/10,
#                                              (as.numeric(sapply(lon.str.val,"[",1))/10)*-1)
# )
# 
# helene.track.dat$DateTime = with(helene.track.dat,date.fun(paste0(
#   substr(YYYYMMDDHH,1,4),"-",
#   substr(YYYYMMDDHH,5,6),"-",
#   substr(YYYYMMDDHH,7,8)," ",
#   substr(YYYYMMDDHH,9,10)
# ),tz="UTC",form="%Y-%m-%d %H"))
# 
# helene.track.dat$DateTime.EST = date.fun(helene.track.dat$DateTime,tz="EST",form="%Y-%m-%d %H")
# helene.track.dat$Date = date.fun(helene.track.dat$DateTime,tz="UTC")
# range(helene.track.dat$Date)
# write.csv(helene.track.dat,paste0(export.path,"helene_track.csv"),row.names = F)


milton.track.dat=read.csv(paste0(export.path,"milton_track.csv"))|>
  mutate(DateTime = date.fun(DateTime,tz="UTC",form="%Y-%m-%d %H"),
         DateTime.EST = date.fun(DateTime,tz="EST",form="%Y-%m-%d %H"),
         Date = date.fun(DateTime,tz="UTC"))
milton.track.dat.sp=st_as_sf(subset(milton.track.dat,is.na(Lat.DD)==F),coords=c("Lon.DD","Lat.DD"),crs=wgs84)

helene.track.dat=read.csv(paste0(export.path,"helene_track.csv"))|>
  mutate(DateTime = date.fun(DateTime,tz="UTC",form="%Y-%m-%d %H"),
         DateTime.EST = date.fun(DateTime,tz="EST",form="%Y-%m-%d %H"),
         Date = date.fun(DateTime,tz="UTC"))
helene.track.dat.sp=st_as_sf(subset(helene.track.dat,is.na(Lat.DD)==F),coords=c("Lon.DD","Lat.DD"),crs=wgs84)





MiltonLandFall=date.fun("2024-10-09 20:30",form="%F %R")
MiltonLandFall.utc = format(MiltonLandFall,"%F %R",tz="UTC",usetz=T)|>date.fun(form="%F %R",tz="UTC")
attributes(MiltonLandFall.utc)

HeleneLandFall=date.fun("2024-09-26 23:10",form="%F %R")
HeleneLandFall.utc = format(HeleneLandFall,"%F %R",tz="UTC",usetz=T)|>date.fun(form="%F %R",tz="UTC")
attributes(HeleneLandFall.utc)


# Copernacus data ---------------------------------------------------------
dat.nc<-nc_open(paste0(data.path,"ECMWFR/data_stream-oper.nc"))
print(dat.nc)


lon <- ncvar_get(dat.nc,"longitude");# extracts longitude
nlon <- dim(lon);# returns the number of records

lat <- ncvar_get(dat.nc,"latitude");# extracts latitude
nlat <- dim(lat);# returns the number of records

time <- ncvar_get(dat.nc,"valid_time");# extracts time
tunits <- ncatt_get(dat.nc,"valid_time","units");# assigns units to time
nt <- dim(time)

## Total precip
dname="tp"
tmp_array <- ncvar_get(dat.nc,dname)
dlname <- ncatt_get(dat.nc,dname,"long_name")
dunits <- ncatt_get(dat.nc,dname,"units")
fillvalue <- ncatt_get(dat.nc,dname,"_FillValue")

## wind v vec
dname="v10"
tmp_v10_array <- ncvar_get(dat.nc,dname)
dlname_v10 <- ncatt_get(dat.nc,dname,"long_name")
dunits_v10 <- ncatt_get(dat.nc,dname,"units")
fillvalue_v10 <- ncatt_get(dat.nc,dname,"_FillValue")

## wind u vec
dname="u10"
tmp_u10_array <- ncvar_get(dat.nc,dname)
dlname_u10 <- ncatt_get(dat.nc,dname,"long_name")
dunits_u10 <- ncatt_get(dat.nc,dname,"units")
fillvalue_u10 <- ncatt_get(dat.nc,dname,"_FillValue")
nc_close(dat.nc)

ws_array = sqrt((tmp_u10_array^2)+(tmp_v10_array^2))

## date time values
tustr <- strsplit(tunits$value, " ")
origin.val=unlist(tustr)[3]
time.val=as.POSIXct(origin.val,tz="UTC")+as.difftime(time,unit="secs")

tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))

### example workflow
m=2
slice.tmp <- tmp_array[,,m]
time.val[m];#corresponding day

slice.r=raster(t(tmp_array[,,m-1]),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs=wgs84)
slice.r2=raster(t(slice.tmp),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs=wgs84)
# slice.r2=calc(slice.r2,fun=function(x) x*39.3701)

plot(st_geometry(FL.shp))
image(slice.r2,add=T)
plot(st_geometry(FL.shp),add=T)
plot(helene.track.dat.sp,add=T)
plot(milton.track.dat.sp,add=T)

slice.r=stack(slice.r,slice.r2)
slice.r.sum=calc(slice.r,sum,na.rm=T)
plot(slice.r.sum)



pt_track.fun = function(points){
  points|>
    st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)|>
    dplyr::group_by(CY)|>
    dplyr::summarize(#name=dplyr::first(Name),
      minDate=min(DateTime,na.rm=T),
      maxDate=max(DateTime,na.rm=T),
      maxWind=max(as.numeric(VMAX),na.rm=T),
      do_union=FALSE)|>
    st_cast("MULTIPOINT")|>
    st_cast("LINESTRING")
}
library(paletteer)
step.ls=seq(as.POSIXct("2024-09-24 00:00:00","UTC"),as.POSIXct("2024-10-11 06:00:00","UTC"),"1 hours")
bbox.lims=st_bbox(subset(FL.shp,state_abbr=="FL"))

# cumualtive rainfall 
b=c(0,seq(0,510,1))
pal = c(NA,paletteer_c("grDevices::YlGnBu", length(b)-2,-1))|>as.character()
Sel_city=c("Miami","Perry","Tallahassee","Tampa--St. Petersburg","Sarasota--Bradenton","Cape Coral","Orlando","Palm Bay--Melbourne")
Sel_city.form=c("Miami", "Cape Coral\nFort Myers", "Sarasota\nBradenton", "Perry", "Tallahassee", 
                "Palm Bay\nMelbourne","Orlando","Tampa\nSt. Petersburg")
m <- 1
slice.tmp <- tmp_array[,,time.val==step.ls[1]]
# time.val[m];#corresponding day
time.val[time.val==step.ls[1]]

slice.r=raster(t(slice.tmp),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs="EPSG:4326")
slice.r[slice.r>0]=0# makes a null raster at the same extent of final cumualtive 

for(i in 1:length(step.ls)){
  step.val=step.ls[i]
  
  if(step.val>date.fun("2024-10-05",tz="UTC")){
    tmp.milton.track = subset(milton.track.dat,DateTime<=step.val)|>
      pt_track.fun()
    tmp.milton.pts = subset(milton.track.dat,DateTime<=step.val)|>
      st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)
    no_milton=F
  }else{no_milton=T}
  if(step.val>date.fun("2024-09-22",tz="UTC")){
    tmp.helene.track = subset(helene.track.dat,DateTime<=step.val)|>
      pt_track.fun()
    tmp.helene.pts = subset(helene.track.dat,DateTime<=step.val)|>
      st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)
    no_helene=F
  }else{no_helene=T}
  
  slice.tmp <- tmp_array[,,time.val==step.val]
  slice.r2=raster(t(slice.tmp),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs="EPSG:4326")
  slice.r2 = slice.r2*1000;# convert to mm 
  
  slice.r=stack(slice.r,slice.r2)
  slice.r.sum=calc(slice.r,sum,na.rm=T)
  
  if(format(step.val,"%H")%in%c("00","06","12","18")==T){
  png(filename=paste0(plot.path,"/pngs/rainfall/",format(step.val,"%Y%m%d_%H"),"_HeleneMilton_rainfall_animation.png"),width=6.5,height=5,units="in",res=200,type="cairo",bg="white")
  par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.1,0.1,0.1,0.1))
  layout(matrix(1:2,1,2,byrow=F),widths=c(1,0.4))
  
  plot(st_geometry(FL.shp),ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,col=NA,border=NA)
  image(slice.r.sum,add=T,breaks=b,col=pal)
  plot(st_geometry(FL.shp),add=T,col=NA,border="grey40",lwd=1)
  st_txt(subset(FLUrban,NAME%in%Sel_city),
         Sel_city.form,halo=T,font=2,cex=0.5)
  
  if(no_helene==F){
  plot(st_geometry(tmp.helene.track),lwd=2,col="red",add=T)
  plot(st_geometry(tmp.helene.pts),pch=21,bg="red",col=adjustcolor("red",0.5),add=T,cex=1.5,lwd=0.1)
  }
  if(no_milton==F){
  plot(st_geometry(tmp.milton.track),lwd=2,col="indianred1",add=T)
  plot(st_geometry(tmp.milton.pts),pch=23,bg="indianred1",col=adjustcolor("indianred1",0.5),add=T,cex=1.5,lwd=0.1)
  }
  box(lwd=1)
  mapmisc::scaleBar(subset(FL.shp,state_abbr=="FL"),"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
  mtext(side=3,adj=0,line=-1.3,paste0(" ",format(step.val,'%d %B %Y %H:%M'),' UTC'),cex=1.25)
  
  plot(0:1,0:1,ann=F,axes=F,type="n")
  leg.fun(b,pal,
          "Cumulative Rainfall (mm)\nSept 24 - Oct 11",
          x.max=0.6,x.min=0.3,
          leg.type="continuous")
  legend(0.5,0.1,c("Helene","Milton"),
         pch=c(21,23),lty=c(1,1),lwd=c(2),
         col=c("red","indianred1"),pt.bg=c(c("red","indianred1")),
         pt.cex=1.5,ncol=1,cex=0.95,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  mtext(side=1,line=-1.25,"Data Source: ERA5 ECMWF",cex=0.75)
  dev.off()
  }
  print(i)
}

file.names=list.files(paste0(plot.path,"/pngs/rainfall/"))
file.names = paste0(plot.path,"/pngs/rainfall/",file.names[grep("HeleneMilton",file.names)]);# added incase other rainfall images are generated
# file.names=paste0(plot.path,"/pngs/rainfall/",format(step.val,"%Y%m%d_%H"),"_HeleneMilton_rainfall_animation.png")

gifski::gifski(file.names, 
               delay = 15 / 100, 
               gif_file  = paste0(plot.path,"Hurricane_HeleneMilton_rainfall_animation.png.gif"),
               loop = T,
               width=6.5*200,
               height=5*200)
# 


## Cumulative windload 
b=c(0,seq(0,14000,1000))
# grDevices::Heat 2
# "grDevices::Sunset"
# pal = c(NA,paletteer_c("ggthemes::Red-Blue Diverging", length(b)-2,-1))|>as.character()
pal = c(NA,paletteer_c("grDevices::Sunset", length(b)-2,1))|>as.character()
Sel_city=c("Miami","Perry","Tallahassee","Tampa--St. Petersburg","Sarasota--Bradenton","Cape Coral","Orlando","Palm Bay--Melbourne")
Sel_city.form=c("Miami", "Cape Coral\nFort Myers", "Sarasota\nBradenton", "Perry", "Tallahassee", 
                "Palm Bay\nMelbourne","Orlando","Tampa\nSt. Petersburg")

m <- 1
slice.tmp <- ws_array[,,time.val==step.ls[1]]
# time.val[m];#corresponding day
time.val[time.val==step.ls[1]]

slice.r=raster(t(slice.tmp),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs="EPSG:4326")
slice.r[slice.r>0]=0# makes a null raster at the same extent of final cumualtive 

for(i in 2:length(step.ls)){
  step.val=step.ls[i]
  step.diff = difftime(step.ls[i], step.ls[i-1],units="secs")|>as.numeric()
  
  if(step.val>date.fun("2024-10-05",tz="UTC")){
    tmp.milton.track = subset(milton.track.dat,DateTime<=step.val)|>
      pt_track.fun()
    tmp.milton.pts = subset(milton.track.dat,DateTime<=step.val)|>
      st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)
    no_milton=F
  }else{no_milton=T}
  if(step.val>date.fun("2024-09-22",tz="UTC")){
    tmp.helene.track = subset(helene.track.dat,DateTime<=step.val)|>
      pt_track.fun()
    tmp.helene.pts = subset(helene.track.dat,DateTime<=step.val)|>
      st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)
    no_helene=F
  }else{no_helene=T}
  
  slice.tmp <- (ws_array[,,time.val==step.val]*step.diff)/1000# wind run converted to kilometers
  slice.r2=raster(t(slice.tmp),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs="EPSG:4326")
  
  slice.r=stack(slice.r,slice.r2)
  slice.r.sum=calc(slice.r,sum,na.rm=T)
  
  if(format(step.val,"%H")%in%c("00","06","12","18")==T){
    png(filename=paste0(plot.path,"/pngs/rainfall/",format(step.val,"%Y%m%d_%H"),"_HeleneMilton_cumWind_animation.png"),width=6.5,height=5,units="in",res=200,type="cairo",bg="white")
    par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.1,0.1,0.1,0.1))
    layout(matrix(1:2,1,2,byrow=F),widths=c(1,0.4))
    
    plot(st_geometry(FL.shp),ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,col=NA,border=NA)
    image(slice.r.sum,add=T,breaks=b,col=pal)
    plot(st_geometry(FL.shp),add=T,col=NA,border="grey40",lwd=1)
    st_txt(subset(FLUrban,NAME%in%Sel_city),
           Sel_city.form,halo=T,font=2,cex=0.5)
    
    if(no_helene==F){
      plot(st_geometry(tmp.helene.track),lwd=2,col="red",add=T)
      plot(st_geometry(tmp.helene.pts),pch=21,bg="red",col=adjustcolor("red",0.5),add=T,cex=1.5,lwd=0.1)
    }
    if(no_milton==F){
      plot(st_geometry(tmp.milton.track),lwd=2,col="indianred1",add=T)
      plot(st_geometry(tmp.milton.pts),pch=23,bg="indianred1",col=adjustcolor("indianred1",0.5),add=T,cex=1.5,lwd=0.1)
    }
    box(lwd=1)
    mapmisc::scaleBar(subset(FL.shp,state_abbr=="FL"),"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
    mtext(side=3,adj=0,line=-1.3,paste0(" ",format(step.val,'%d %B %Y %H:%M'),' UTC'),cex=1.25)
    
    plot(0:1,0:1,ann=F,axes=F,type="n")
    leg.fun(b,pal,
            "Cumulative Windrun (km)\n\u2211(WSP \u00D7 \u0394time)\nSept 24 - Oct 11",
            x.max=0.6,x.min=0.3,
            leg.type="continuous")
    legend(0.5,0.1,c("Helene","Milton"),
           pch=c(21,23),lty=c(1,1),lwd=c(2),
           col=c("red","indianred1"),pt.bg=c(c("red","indianred1")),
           pt.cex=1.5,ncol=1,cex=0.95,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
    mtext(side=1,line=-1.25,"Data Source: ERA5 ECMWF",cex=0.75)
    dev.off()
  }
  print(i)
}

file.names=list.files(paste0(plot.path,"/pngs/rainfall/"))
file.names = file.names[grep("HeleneMilton_cumWind",file.names)];# added incase other rainfall images are generated
# file.remove(paste0(plot.path,"/pngs/rainfall/",file.names))
# file.exists(file.names)

# NOAA Daily Rainfall -----------------------------------------------------
crs_string = "+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-105 +x_0=0 +y_0=0 +a=6371200 +b=6371200 +units=m +no_defs"
noaa_crs = st_crs(crs_string)

# download NOAA precip stage 4 daily data
# https://water.noaa.gov/about/precipitation-data-access
da.precip = seq(date.fun("2024-09-20"),date.fun("2024-10-13"),"1 days")

# https://water.noaa.gov/resources/downloads/precip/stageIV/2024/10/04/nws_precip_1day_20241004_conus.nc  
link="https://water.noaa.gov/resources/downloads/precip/stageIV"
yr = format(da.precip,"%Y")
mon = format(da.precip,"%m")
day = format(da.precip,"%d")
file = paste0("nws_precip_1day_",yr,mon,day,"_conus.nc")

for(i in 1:length(da.precip)){
download.file(paste(link,yr[i],mon[i],day[i],file[i],sep="/"),
              paste0(data.path,"NOAA/",file[i]),
              mode="wb")
}

dat.nc2<-nc_open(paste0(data.path,"NOAA/",file[13]))
print(dat.nc2)

## Workflow
time <- ncvar_get(dat.nc2,"time");# extracts time
# tunits <- ncatt_get(dat.nc2,"time","units");# assigns units to time
time.val=as.POSIXct("1970-01-01",tz="UTC")+as.difftime(time,unit="secs")

lon <- ncvar_get(dat.nc2,"x");# extracts longitude
lat <- ncvar_get(dat.nc2,"y");# extracts latitude

dname="observation"
tmp_array <- ncvar_get(dat.nc2,dname)
# dlname <- ncatt_get(dat.nc2,dname,"long_name")
# dunits <- ncatt_get(dat.nc2,dname,"units")
fillvalue <- ncatt_get(dat.nc2,dname,"_FillValue")

sum(tmp_array<0,na.rm=T)
tmp_array[tmp_array%in%fillvalue$value]=NA
tmp_array[tmp_array<0] = NA

obs.rf = raster(t(tmp_array),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs=crs_string)
obs.rf = projectRaster(obs.rf,crs="EPSG:4326")
obs.rf = obs.rf*25.4;# convert inches to mm

plot(obs.rf)
plot(st_geometry(FL.shp),add=T)

AOI.poly <- raster::extent(subset(FL.shp,state_abbr=="FL"))|>
  as("SpatialPolygons")|>
  st_as_sf()
st_crs(AOI.poly) = wgs84
plot(crop(obs.rf,AOI.poly))


noaa_RF = function(nc.file,con.to.mm=T){
  dat.nc2<-nc_open(nc.file)
  
  # time <- ncvar_get(dat.nc2,"time");# extracts time
  # tunits <- ncatt_get(dat.nc2,"time","units");# assigns units to time
  # time.val=as.POSIXct("1970-01-01",tz="UTC")+as.difftime(time,unit="secs")
  
  lon <- ncvar_get(dat.nc2,"x");# extracts longitude
  lat <- ncvar_get(dat.nc2,"y");# extracts latitude
  
  dname="observation"
  tmp_array <- ncvar_get(dat.nc2,dname)
  # dlname <- ncatt_get(dat.nc2,dname,"long_name")
  # dunits <- ncatt_get(dat.nc2,dname,"units")
  fillvalue <- ncatt_get(dat.nc2,dname,"_FillValue")
  
  sum(tmp_array<0,na.rm=T)
  tmp_array[tmp_array%in%fillvalue$value]=NA
  tmp_array[tmp_array<0] = NA
  
  obs.rf = raster(t(tmp_array),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs=crs_string);
  obs.rf = projectRaster(obs.rf,crs="EPSG:4326");# reproject data to WG84
  if(con.to.mm==T){obs.rf = obs.rf*25.4} # convert inches to mm
  return(obs.rf) 
}


plot(noaa_RF(paste0(data.path,"NOAA/",file[13])))
plot(noaa_RF(paste0(data.path,"NOAA/",file[13]),F))

AOI.poly <- raster::extent(subset(FL.shp,state_abbr%in%c("FL","GA","AL")))|>
  as("SpatialPolygons")|>
  st_as_sf()
st_crs(AOI.poly) = wgs84
plot(crop(obs.rf,AOI.poly))


r1=noaa_RF(paste0(data.path,"NOAA/",file[1]))|>
  crop(AOI.poly)
r1[r1>0]=0# makes a null raster at the same extent of final cumualtive 

b=c(0,seq(0,750,1))
pal = c(NA,paletteer::paletteer_c("grDevices::YlGnBu", length(b)-2,-1))|>as.character()
Sel_city=c("Miami","Perry","Tallahassee","Tampa--St. Petersburg","Sarasota--Bradenton","Cape Coral","Orlando","Palm Bay--Melbourne")
Sel_city.form=c("Miami", "Cape Coral\nFort Myers", "Sarasota\nBradenton", "Perry", "Tallahassee", 
                "Palm Bay\nMelbourne","Orlando","Tampa\nSt. Petersburg")
bbox.lims=st_bbox(subset(FL.shp,state_abbr=="FL"))
for(i in 1:length(da.precip)){
  step.val=da.precip[i]
  
  if(step.val>date.fun("2024-10-05",tz="EST")){
    tmp.milton.track = subset(milton.track.dat,DateTime.EST<=step.val)|>
      pt_track.fun()
    tmp.milton.pts = subset(milton.track.dat,DateTime.EST<=step.val)|>
      st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)
    no_milton=F
  }else{no_milton=T}
  if(step.val>date.fun("2024-09-22",tz="EST")){
    tmp.helene.track = subset(helene.track.dat,DateTime.EST<=step.val)|>
      pt_track.fun()
    tmp.helene.pts = subset(helene.track.dat,DateTime.EST<=step.val)|>
      st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)
    no_helene=F
  }else{no_helene=T}
  
  r2 <- noaa_RF(paste0(data.path,"NOAA/",file[i]))|>
    crop(AOI.poly)
  r1=stack(r1,r2)
  r.sum=calc(r1,sum,na.rm=T)
  
    png(filename=paste0(plot.path,"/pngs/rainfall/",format(step.val,"%Y%m%d"),"_HeleneMilton_NOAAdaRF_animation.png"),width=6.5,height=5,units="in",res=200,type="cairo",bg="white")
    par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.1,0.1,0.1,0.1))
    layout(matrix(1:2,1,2,byrow=F),widths=c(1,0.4))
    
    plot(st_geometry(FL.shp),ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,col=NA,border=NA)
    image(r.sum,add=T,breaks=b,col=pal)
    plot(st_geometry(FL.shp),add=T,col=NA,border="grey40",lwd=1)
    st_txt(subset(FLUrban,NAME%in%Sel_city),
           Sel_city.form,halo=T,font=2,cex=0.5)
    
    if(no_helene==F){
      plot(st_geometry(tmp.helene.track),lwd=2,col="red",add=T)
      plot(st_geometry(tmp.helene.pts),pch=21,bg="red",col=adjustcolor("red",0.5),add=T,cex=1.5,lwd=0.1)
    }
    if(no_milton==F){
      plot(st_geometry(tmp.milton.track),lwd=2,col="indianred1",add=T)
      plot(st_geometry(tmp.milton.pts),pch=23,bg="indianred1",col=adjustcolor("indianred1",0.5),add=T,cex=1.5,lwd=0.1)
    }
    box(lwd=1)
    mapmisc::scaleBar(subset(FL.shp,state_abbr=="FL"),"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
    mtext(side=3,adj=0,line=-1.3,paste0(" ",format(step.val,'%d %B %Y')),cex=1.25)
    
    plot(0:1,0:1,ann=F,axes=F,type="n")
    leg.fun(b,pal,
            "Cumulative Rainfall (mm)\nSept 20 - Oct 13",
            x.max=0.6,x.min=0.3,
            leg.type="continuous")
    legend(0.5,0.1,c("Helene","Milton"),
           pch=c(21,23),lty=c(1,1),lwd=c(2),
           col=c("red","indianred1"),pt.bg=c(c("red","indianred1")),
           pt.cex=1.5,ncol=1,cex=0.95,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
    mtext(side=1,line=-1.25,"Data Source: NOAA QPE",cex=0.75)
    dev.off()
    print(i)
  }

file.names=list.files(paste0(plot.path,"/pngs/rainfall/"))
file.names = paste0(plot.path,"/pngs/rainfall/",file.names[grep("HeleneMilton_NOAAdaRF",file.names)]);# added incase other rainfall images are generated
# file.names=paste0(plot.path,"/pngs/rainfall/",format(step.val,"%Y%m%d_%H"),"_HeleneMilton_rainfall_animation.png")

gifski::gifski(file.names, 
               delay = 50 / 100, 
               gif_file  = paste0(plot.path,"Hurricane_HeleneMilton_NOAAdaRF_animation.png.gif"),
               loop = T,
               width=6.5*200,
               height=5*200)

### same plot just in inches
r1=noaa_RF(paste0(data.path,"NOAA/",file[1]),F)|>
  crop(AOI.poly)
r1[r1>0]=0# makes a null raster at the same extent of final cumualtive 

b=c(0,seq(0,30,2))
pal = c(NA,paletteer::paletteer_c("grDevices::YlGnBu", length(b)-2,-1))|>as.character()
Sel_city=c("Miami","Perry","Tallahassee","Tampa--St. Petersburg","Sarasota--Bradenton","Cape Coral","Orlando","Palm Bay--Melbourne")
Sel_city.form=c("Miami", "Cape Coral\nFort Myers", "Sarasota\nBradenton", "Perry", "Tallahassee", 
                "Palm Bay\nMelbourne","Orlando","Tampa\nSt. Petersburg")
bbox.lims=st_bbox(subset(FL.shp,state_abbr=="FL"))
for(i in 1:length(da.precip)){
  step.val=da.precip[i]
  
  if(step.val>date.fun("2024-10-05",tz="EST")){
    tmp.milton.track = subset(milton.track.dat,DateTime.EST<=step.val)|>
      pt_track.fun()
    tmp.milton.pts = subset(milton.track.dat,DateTime.EST<=step.val)|>
      st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)
    no_milton=F
  }else{no_milton=T}
  if(step.val>date.fun("2024-09-22",tz="EST")){
    tmp.helene.track = subset(helene.track.dat,DateTime.EST<=step.val)|>
      pt_track.fun()
    tmp.helene.pts = subset(helene.track.dat,DateTime.EST<=step.val)|>
      st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)
    no_helene=F
  }else{no_helene=T}
  
  r2 <- noaa_RF(paste0(data.path,"NOAA/",file[i]),F)|>
    crop(AOI.poly)
  r1=stack(r1,r2)
  r.sum=calc(r1,sum,na.rm=T)
  
  png(filename=paste0(plot.path,"/pngs/rainfall/",format(step.val,"%Y%m%d"),"_HeleneMilton_NOAAdaRF_inches_animation.png"),width=6.5,height=5,units="in",res=200,type="cairo",bg="white")
  par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.1,0.1,0.1,0.1))
  layout(matrix(1:2,1,2,byrow=F),widths=c(1,0.4))
  
  plot(st_geometry(FL.shp),ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,col=NA,border=NA)
  image(r.sum,add=T,breaks=b,col=pal)
  plot(st_geometry(FL.shp),add=T,col=NA,border="grey40",lwd=1)
  st_txt(subset(FLUrban,NAME%in%Sel_city),
         Sel_city.form,halo=T,font=2,cex=0.5)
  
  if(no_helene==F){
    plot(st_geometry(tmp.helene.track),lwd=2,col="red",add=T)
    plot(st_geometry(tmp.helene.pts),pch=21,bg="red",col=adjustcolor("red",0.5),add=T,cex=1.5,lwd=0.1)
  }
  if(no_milton==F){
    plot(st_geometry(tmp.milton.track),lwd=2,col="indianred1",add=T)
    plot(st_geometry(tmp.milton.pts),pch=23,bg="indianred1",col=adjustcolor("indianred1",0.5),add=T,cex=1.5,lwd=0.1)
  }
  box(lwd=1)
  mapmisc::scaleBar(subset(FL.shp,state_abbr=="FL"),"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
  mtext(side=3,adj=0,line=-1.3,paste0(" ",format(step.val,'%d %B %Y')),cex=1.25)
  
  plot(0:1,0:1,ann=F,axes=F,type="n")
  leg.fun(b,pal,
          "Cumulative Rainfall (inches)\nSept 20 - Oct 13",
          x.max=0.6,x.min=0.3,
          leg.type="continuous")
  legend(0.5,0.1,c("Helene","Milton"),
         pch=c(21,23),lty=c(1,1),lwd=c(2),
         col=c("red","indianred1"),pt.bg=c(c("red","indianred1")),
         pt.cex=1.5,ncol=1,cex=0.95,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  mtext(side=1,line=-1.25,"Data Source: NOAA QPE",cex=0.75)
  dev.off()
  print(i)
}

file.names=list.files(paste0(plot.path,"/pngs/rainfall/"))
file.names = paste0(plot.path,"/pngs/rainfall/",file.names[grep("HeleneMilton_NOAAdaRF_inches",file.names)]);# added incase other rainfall images are generated
# file.names=paste0(plot.path,"/pngs/rainfall/",format(step.val,"%Y%m%d_%H"),"_HeleneMilton_rainfall_animation.png")

gifski::gifski(file.names, 
               delay = 50 / 100, 
               gif_file  = paste0(plot.path,"Hurricane_HeleneMilton_NOAAdaRF_inches_animation.png.gif"),
               loop = T,
               width=6.5*200,
               height=5*200)
