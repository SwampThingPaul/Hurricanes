## 
## Hurricane Ian data visualization
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

## Hurricane paths
# remotes::install_github("timtrice/HURDAT")
library(HURDAT)
AL.hurdat=get_hurdat("AL")
ian.track.dat=subset(AL.hurdat,Name=="IAN")|>
  mutate(DateTime = date.fun(DateTime,tz="UTC",form="%Y-%m-%d %H"),
         DateTime.EST = date.fun(DateTime,tz="EST",form="%Y-%m-%d %H"),
         Lat.DD=Lat,Lon.DD=Lon,VMAX=Wind,
         CY=as.numeric(format(DateTime,"%Y"))
  )|>
  subset(as.numeric(format(DateTime.EST,"%Y"))==2022)
range(ian.track.dat$DateTime.EST)

# NOAA Daily Rainfall -----------------------------------------------------
crs_string = "+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-105 +x_0=0 +y_0=0 +a=6371200 +b=6371200 +units=m +no_defs"
noaa_crs = st_crs(crs_string)

# download NOAA precip stage 4 daily data
# https://water.noaa.gov/about/precipitation-data-access
da.precip = seq(date.fun("2022-09-22",tz = "UTC"),date.fun("2022-10-02",tz = "UTC"),"1 days")

# https://water.noaa.gov/resources/downloads/precip/stageIV/2024/10/04/nws_precip_1day_20241004_conus.nc  
link="https://water.noaa.gov/resources/downloads/precip/stageIV"
yr = format(da.precip,"%Y")
mon = format(da.precip,"%m")
day = format(da.precip,"%d")
file = paste0("nws_precip_1day_",yr,mon,day,"_conus.nc")

# for(i in 1:length(da.precip)){
#   download.file(paste(link,yr[i],mon[i],day[i],file[i],sep="/"),
#                 paste0(data.path,"NOAA/",file[i]),
#                 mode="wb")
# }

AOI.poly <- raster::extent(subset(FL.shp,state_abbr=="FL"))|>
  as("SpatialPolygons")|>
  st_as_sf()
st_crs(AOI.poly) = wgs84
plot(crop(obs.rf,AOI.poly))

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


r1=noaa_RF(paste0(data.path,"NOAA/",file[1]))|>
  crop(AOI.poly)
r1[r1>0]=0# makes a null raster at the same extent of final cumualtive 

# step.ls=seq(as.POSIXct("2022-09-22","UTC"),as.POSIXct("2022-10-01","UTC"),"1 days")
# step.ls=seq(as.POSIXct("2022-09-22 22:00:00","UTC"),as.POSIXct("2022-10-01 06:00:00","UTC"),"3 hours")

b=c(0,seq(0,630,20))
pal = c(NA,paletteer::paletteer_c("grDevices::YlGnBu", length(b)-2,-1))|>as.character()
Sel_city=c("Miami","Perry","Tallahassee","Tampa--St. Petersburg","Sarasota--Bradenton","Cape Coral","Orlando","Palm Bay--Melbourne")
Sel_city.form=c("Miami", "Cape Coral\nFort Myers", "Sarasota\nBradenton", "Perry", "Tallahassee", 
                "Palm Bay\nMelbourne","Orlando","Tampa\nSt. Petersburg")
bbox.lims=st_bbox(subset(FL.shp,state_abbr=="FL"))
for(i in 1:length(da.precip)){
  step.val=da.precip[i]
  
  if(step.val>date.fun("2022-09-23",tz="EST")){
    tmp.ian.track = subset(ian.track.dat,DateTime<=step.val)|>
      pt_track.fun()
    tmp.ian.pts = subset(ian.track.dat,DateTime<=step.val)|>
      st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)
    no_ian=F
  }else{no_ian=T}
  
  r2 <- noaa_RF(paste0(data.path,"NOAA/",file[i]))|>
    crop(AOI.poly)
  r1=stack(r1,r2)
  r.sum=calc(r1,sum,na.rm=T)
  
  png(filename=paste0(plot.path,"/pngs/rainfall/2022Ian/",format(step.val,"%Y%m%d"),"_Ian_NOAAdaRF_animation.png"),width=6.5,height=5,units="in",res=200,type="cairo",bg="white")
  par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.1,0.1,0.1,0.1))
  layout(matrix(1:2,1,2,byrow=F),widths=c(1,0.4))
  
  plot(st_geometry(FL.shp),ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,col=NA,border=NA)
  image(r.sum,add=T,breaks=b,col=pal)
  plot(st_geometry(FL.shp),add=T,col=NA,border="grey40",lwd=1)
  st_txt(subset(FLUrban,NAME%in%Sel_city),
         Sel_city.form,halo=T,font=2,cex=0.5)
  
  if(no_ian==F){
    plot(st_geometry(tmp.ian.track),lwd=2,col="red",add=T)
    plot(st_geometry(tmp.ian.pts),pch=21,bg="red",col=adjustcolor("red",0.5),add=T,cex=1.5,lwd=0.1)
  }
  box(lwd=1)
  mapmisc::scaleBar(subset(FL.shp,state_abbr=="FL"),"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
  mtext(side=3,adj=0,line=-1.3,paste0(" ",format(step.val,'%d %B %Y')),cex=1.25)
  
  plot(0:1,0:1,ann=F,axes=F,type="n")
  leg.fun(b,pal,
          "Cumulative Rainfall (mm)\nSept 22 - Oct 01",
          x.max=0.6,x.min=0.3,
          leg.type="continuous")
  legend(0.5,0.1,c("Ian"),
         pch=c(21),lty=c(1),lwd=c(2),
         col=c("red"),pt.bg=c(c("red")),
         pt.cex=1.5,ncol=1,cex=0.95,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
  mtext(side=1,line=-1.25,"Data Source: NOAA QPE",cex=0.75)
  dev.off()
  print(i)
}

file.names=list.files(paste0(plot.path,"/pngs/rainfall/2022Ian"))
file.names = paste0(plot.path,"/pngs/rainfall/2022Ian/",file.names[grep("_Ian_NOAAdaRF",file.names)]);# added incase other rainfall images are generated
# file.names=paste0(plot.path,"/pngs/rainfall/",format(step.val,"%Y%m%d_%H"),"_HeleneMilton_rainfall_animation.png")

gifski::gifski(file.names, 
               delay = 50 / 100, 
               gif_file  = paste0(plot.path,"2022_Hurricane_Ian_rainfall_animation.png.gif"),
               loop = T,
               width=6.5*200,
               height=5*200)
