## 
## Hurricane Ian data visualization
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

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(gstat)
library(tmap)

# Other data and vis tools
library(httr)
library(magrittr)
library(dataRetrieval)
library(maptools)
library(maps)

## Paths
wd="C:/Julian_LaCie/_Github/HurricaneIan"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]
GIS.path.gen="C:/Julian_LaCie/_GISData"

# Helper variables
nad83.pro=CRS("+init=epsg:4269")
utm17=CRS("+init=epsg:26917")
wgs84=CRS("+proj=longlat +datum=WGS84")

proj.string= '+init=epsg:3086'
proj=CRS(proj.string)

tmap_mode("view")


tz.con=function(datetime,to.tz){
  as.POSIXct(format(datetime,tz=to.tz,usetz=T),tz=to.tz)
}
leg.fun=function(b,pal,leg.title,
                 top.val=0.8,bot.val=0.2,mid.v.val=NULL,
                 x.max=0.3,x.min=0.1,mid.val=NULL,
                 txt.offset.val=-0.01,txt.y=NULL,leg.txt=NULL,
                 txt.cex=0.75,txt.adj=0,txt.pos=4,txt.offset=0.5,
                 title.cex=0.8,title.pos=3,title.adj=0,
                 title.x=NULL,title.y=NULL,
                 leg.type=c("continuous","categorical"), ...){
  l.b=length(b)
  labs=c(paste0("< ",b[2]),paste(b[2:(l.b-2)],b[3:(l.b-1)],sep=" - "),paste(paste0(">",b[(l.b-1)])))
  n.bks=length(b)-1
  mid.v.val=if(is.null(mid.v.val)==T){bot.val+(top.val-bot.val)/2}else{mid.v.val}
  
  mid.val=if(is.null(mid.val)==T){x.min+(x.max-x.min)/2}else{mid.val}
  if(leg.type=="continuous"){
    legend_image=as.raster(matrix(rev(pal),ncol=1))
    rasterImage(legend_image,x.min,bot.val,x.max,top.val)
    txt.y=if(is.null(txt.y)==T){c(bot.val,top.val)}else(txt.y)
    leg.txt=if(is.null(leg.txt)==T){format(c(min(b),max(b)))}else(leg.txt)
    text(x=x.max, y = txt.y, labels =leg.txt,cex=txt.cex,adj=txt.adj,pos=txt.pos,offset=txt.offset, ...)
  }
  if(leg.type=="categorical"){
    bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
    rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(pal),lty=0)
    leg.txt=if(is.null(leg.txt)==T){labs}else(leg.txt)
    text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, 
         labels = rev(leg.txt),cex=txt.cex,xpd=NA,pos=txt.pos,adj=txt.adj)
  }
  
  title.x=if(is.null(title.x)==T){mid.val}else{title.x}
  title.y=if(is.null(title.y)==T){top.val}else{title.y}
  text(x=title.x,y=title.y,leg.title,adj=title.adj,cex=title.cex,pos=title.pos,xpd=NA)
}


# Basic GIS ---------------------------------------------------------------

sanibel=spTransform(readOGR(paste0(GIS.path.gen,"/LeeCounty/County_Background_Poly"),"Sanibel"),utm17)
scap=spTransform(readOGR(paste0(GIS.path.gen,"/LeeCounty/County_Background_Poly"),"SouthCaptiva"),utm17)

plot(bind(sanibel,scap),col="grey")

islands=bind(sanibel,scap)

islands2=gBuffer(gBuffer(islands,width=4000),width=-4000)

plot(islands2)
plot(islands,col="grey",add=T)
# HWL data ----------------------------------------------------------------
## SCCF

sccf.hwl=read.xlsx(paste0(data.path,"HWL/Ian_HWM_28NOV22.xlsx"))
sccf.hwl$height_above_gnd=m.to.ft(sccf.hwl$Mark_Offset+0.10) # added offset from measuring tape
# sccf.hwl=subset(sccf.hwl,Comment!="doc ford's back porch usgs previous point")

sccf.hwl[,c("Latitude","Longitude")]=apply(sccf.hwl[,c("Latitude","Longitude")],2,"as.numeric")
sccf.hwl=subset(sccf.hwl,is.na(sccf.hwl$Longitude)==F)
sccf.hwl$ID=sccf.hwl$`OBJECTID.*`

vars=c("ID","Latitude","Longitude","height_above_gnd")
tmp=sccf.hwl[,vars]
colnames(tmp)=c("Latitude","Longitude","height_above_gnd_ft")
# write.csv(tmp,paste0(export.path,"SCCF_Captiva_HWL.csv"),row.names = F)

sccf.hwl.shp=SpatialPointsDataFrame(sccf.hwl[,vars[3:2]],
                                    sccf.hwl[,vars],
                                    proj4string = wgs84)
sccf.hwl.shp=spTransform(sccf.hwl.shp,utm17)
sccf.coord=coordinates(sccf.hwl.shp)
colnames(sccf.coord)=c("UTMX",'UTMY')
sccf.hwl.shp=cbind(sccf.hwl.shp,sccf.coord)

subset(sccf.hwl.shp, ID>3)

# https://stn.wim.usgs.gov/FEV/#2022Ian
usgs.hwl=read.csv(paste0(data.path,"HWL/FilteredHWMs.csv"))

vars=c("latitude_dd","longitude_dd","height_above_gnd")
usgs.hwl.shp=SpatialPointsDataFrame(usgs.hwl[,vars[2:1]],
                                    usgs.hwl[,vars],
                                    proj4string = wgs84)
usgs.hwl.shp=spTransform(usgs.hwl.shp,utm17)
usgs.coord=coordinates(usgs.hwl.shp)
colnames(usgs.coord)=c("UTMX",'UTMY')
usgs.hwl.shp=cbind(usgs.hwl.shp,usgs.coord)

plot(islands2)
plot(usgs.hwl.shp,add=T)
plot(sccf.hwl.shp,add=T,pch=21,bg="red")

sccf.tmp=sccf.hwl.shp[,c("UTMX","UTMY","height_above_gnd")]
sccf.tmp$Source="SCCF"
USGS.tmp=raster::intersect(usgs.hwl.shp,gBuffer(islands2,width=500))[,c("UTMX","UTMY","height_above_gnd")]
USGS.tmp$Source="USGS"

hwl.shp=bind(sccf.tmp,
             USGS.tmp)
plot(hwl.shp)

# writeOGR(hwl.shp,paste0(wd,"/map/data"),"HWL_data",driver="ESRI Shapefile")

# Spline ------------------------------------------------------------------
#thin plate spline https://rspatial.org/raster/analysis/4-interpolation.html
library(fields)
region.buf.r=raster(gBuffer(islands2, width=1000))
res(region.buf.r)=10

hwl.shp2=subset(hwl.shp,is.na(height_above_gnd)==F)
m=Tps(coordinates(hwl.shp2),hwl.shp2$height_above_gnd)
tps=interpolate(region.buf.r,m)
tps.island=mask(tps,islands)

# writeRaster(tps.island,paste0(wd,"/map/data/HWL_tps.tif"),format="GTiff")

# png(filename=paste0(plot.path,"/quickplot_heightaboveground_ft.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
plot(tps.island)
plot(islands,add=T,col=NA)
plot(hwl.shp,add=T,pch=19,cex=0.5,col="red")
dev.off()

hwl.shp2$interpolate=raster::extract(tps.island,hwl.shp2)

plot(height_above_gnd~interpolate,hwl.shp2);abline(0,1)

# tm_shape(tps.island)+tm_raster()+
#   tm_shape(hwl.shp2)+tm_dots(col="red")

bbox.lims=bbox(islands2)
# png(filename=paste0(plot.path,"/HWL.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.4))

b=seq(0,10,2)
cols=viridis::viridis(length(b)-1)
plot(islands2,border=NA,
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
image(tps.island,breaks=b,col=cols,add=T)
plot(hwl.shp2,add=T,pch=19,cex=0.5,col=adjustcolor("red",0.5))
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");

plot(0:1,0:1,type="n",ann=F,axes=F)
leg.fun(b,cols,"High Water Line\n(feet above ground)",leg.type="categorical",
        leg.txt = paste(b[1:length(b)-1],b[2:length(b)],sep=" - "))
dev.off()

# png(filename=paste0(plot.path,"/HWL_points.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.4))

b=seq(0,10,1)
cols=viridis::viridis(length(b)-1)
bks.val=findInterval(hwl.shp2$height_above_gnd,b)
plot(islands,col="cornsilk",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(hwl.shp2,add=T,pch=19,cex=1,col=cols[bks.val])
plot(0:1,0:1,type="n",ann=F,axes=F)
leg.fun(b,cols,"High Water Line\n(feet above ground)",leg.type="categorical",
        leg.txt = paste(b[1:length(b)-1],b[2:length(b)],sep=" - "))
dev.off()


AOI=raster::extent(gBuffer(raster::intersect(hwl.shp2,scap),width=500))
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm17
# png(filename=paste0(plot.path,"/HWL_Captiva.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(c(1:4,5,5),2,3,byrow=F),widths=c(1,0.60,0.5))

bbox.lims=bbox(islands2)
b=seq(0,10,1)
cols=viridis::viridis(length(b)-1)
bks.val=findInterval(hwl.shp2$height_above_gnd,b)
plot(islands,col="cornsilk",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(hwl.shp2,add=T,pch=19,cex=1,col=cols[bks.val])
plot(AOI.poly,border="red",add=T)
box(lwd=1)
mtext(side=3,line=-1.25,"Sanibel & Captiva")

plot(islands2,border=NA,
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
image(tps.island,breaks=b,col=cols,add=T)
plot(hwl.shp2,add=T,pch=19,cex=0.5,col=adjustcolor("red",0.5))
plot(AOI.poly,border="red",add=T)
box(lwd=1)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");

bbox.lims2=bbox(AOI)
plot(islands,col="cornsilk",
     ylim=bbox.lims2[c(2,4)],xlim=bbox.lims2[c(1,3)],lwd=0.1)
plot(hwl.shp2,add=T,pch=19,cex=1,col=cols[bks.val])
box(lwd=1)
mtext(side=3,line=-1.25,"Captiva")

plot(islands,col=NA,border="white",lwd=1,
     ylim=bbox.lims2[c(2,4)],xlim=bbox.lims2[c(1,3)])
image(tps.island,breaks=b,col=cols,add=T)
plot(hwl.shp2,add=T,pch=19,cex=0.5,col=adjustcolor("red",0.5))
box(lwd=1)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");

plot(0:1,0:1,type="n",ann=F,axes=F)
leg.fun(b,cols,"Hurricane Ian\nHigh Water Line\n(feet above ground)",leg.type="categorical",
        leg.txt = paste(b[1:length(b)-1],b[2:length(b)],sep=" - "),x.max=0.4,x.min=0.2,title.cex=1)
dev.off()

# Elevation data ----------------------------------------------------------

elev_1=raster(paste0(GIS.path,"/elevation/Job798611/Job798611_2018_swfl_000_000.tif"))
crs(elev_1)=utm17

elev_2=raster(paste0(GIS.path,"/elevation/Job798611/Job798611_2018_swfl_001_000.tif"))
crs(elev_2)=utm17

elev_1_2=mosaic(elev_1,elev_2,fun=min)

elev_3=raster(paste0(GIS.path,"/elevation/Job798612/Job798612_2018_swfl.tif"))
crs(elev_3)=utm17

SanCap.NAVD88=mosaic(elev_1_2,elev_3,fun=min)
plot(SanCap.NAVD88)

tm_shape(SanCap.NAVD88)+tm_raster()

hwl.shp$elev=raster::extract(SanCap.NAVD88,hwl.shp,buffer=10,fun=mean)



# Colour scheme
blue.col <- colorRampPalette(c("darkblue", "lightblue"))
yellow.col <- colorRampPalette(c("lightyellow", "orange"))

mi=cellStats(SanCap.NAVD88,stat="min")-0.5
ma=cellStats(SanCap.NAVD88,stat="max")+0.5

s1=seq(from=mi,to=0,by=0-mi/8)
s2=seq(from=0,to=ma,by=ma/5)

s1=round(s1,2)
s2=round(s2,5)
s3=c(s1,s2[-1])

cols=c(blue.col(8), terrain.colors(5))

# png(filename=paste0(plot.path,"/Eelvation_NAVD88.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.4))

plot(islands,col=NA,border=NA,
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
image(SanCap.NAVD88,col=cols,breaks=s3,add=T)
plot(islands,border="white",col=NA,add=T)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");

plot(0:1,0:1,type="n",ann=F,axes=F)
leg.fun(round(s3,2),cols,"Elevation\n(feet, NAVD88)",leg.type="continuous")
dev.off()


bbox.lims=bbox(scap)
# png(filename=paste0(plot.path,"/HWL_elevation.png"),width=4.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:4,2,2,byrow=T),widths=c(1,0.4))

b=seq(0,10,1)
cols=viridis::viridis(length(b)-1)
plot(islands2,border=NA,
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
image(tps.island,breaks=b,col=cols,add=T)
plot(hwl.shp2,add=T,pch=19,cex=0.5,col=adjustcolor("red",0.5))
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");

plot(0:1,0:1,type="n",ann=F,axes=F)
leg.fun(b,cols,"High Water Line\n(feet above ground)",leg.type="categorical",
        leg.txt = paste(b[1:length(b)-1],b[2:length(b)],sep=" - "))

# Elevation
cols=c(blue.col(8), terrain.colors(5))
plot(islands,col=NA,border=NA,
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
image(SanCap.NAVD88,col=cols,breaks=s3,add=T)
plot(islands,border="white",col=NA,add=T)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");

plot(0:1,0:1,type="n",ann=F,axes=F)
leg.fun(round(s3,2),cols,"Elevation\n(feet, NAVD88)",leg.type="continuous")
dev.off()


bbox.lims=bbox(scap)
# png(filename=paste0(plot.path,"/HWL_elevation_v2.png"),width=4.5,height=6,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:4,2,2,byrow=T),widths=c(1,0.4))

b=seq(0,10,1)
cols=viridis::viridis(length(b)-1)
bks.val=findInterval(hwl.shp2$height_above_gnd,b)
plot(islands,col="cornsilk",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(hwl.shp2,add=T,pch=19,cex=1,col=cols[bks.val])
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");
plot(0:1,0:1,type="n",ann=F,axes=F)
leg.fun(b,cols,"High Water Line\n(feet above ground)",leg.type="categorical",
        leg.txt = paste(b[1:length(b)-1],b[2:length(b)],sep=" - "))

# Elevation
cols=c(blue.col(8), terrain.colors(5))
plot(islands,col=NA,border=NA,
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
image(SanCap.NAVD88,col=cols,breaks=s3,add=T)
plot(islands,border="white",col=NA,add=T)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");

plot(0:1,0:1,type="n",ann=F,axes=F)
leg.fun(round(s3,2),cols,"Elevation\n(feet, NAVD88)",leg.type="continuous")
dev.off()


## Combine Elevation and HWL

gnd.hwl=resample(SanCap.NAVD88,tps)+mask(tps,islands2)

plot(gnd.hwl)
gnd.hwl.island=mask(gnd.hwl,islands)
tm_shape(gnd.hwl.island)+tm_raster(alpha=0.5)


# png(filename=paste0(plot.path,"/HWL_NAVD88.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:2,1,2,byrow=T),widths=c(1,0.4))

b=seq(0,12,2)
cols=viridis::viridis(length(b)-1)
plot(islands2,border=NA,
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
image(gnd.hwl.island,breaks=b,col=cols,add=T)
plot(hwl.shp2,add=T,pch=19,cex=0.5,col=adjustcolor("black",0.5))
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="black");

plot(0:1,0:1,type="n",ann=F,axes=F)
leg.fun(b,cols,"High Water Line\n(feet, NAVD88)",leg.type="categorical",
        leg.txt = paste(b[1:length(b)-1],b[2:length(b)],sep=" - "))
dev.off()




