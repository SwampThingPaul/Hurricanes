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
    text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, 
         labels = rev(labs),cex=txt.cex,xpd=NA,pos=txt.pos,adj=txt.adj)
  }
  
  title.x=if(is.null(title.x)==T){mid.val}else{title.x}
  title.y=if(is.null(title.y)==T){top.val}else{title.y}
  text(x=title.x,y=title.y,leg.title,adj=title.adj,cex=title.cex,pos=title.pos,xpd=NA)
}

# -------------------------------------------------------------------------
shore=spTransform(readOGR(paste0(GIS.path.gen,"/FWC"),"FWC_Shoreline"),utm17)

elev.msl=raster(paste0(GIS.path,"/elevation/Job789907_2018_swfl_000_000.tif"))
crs(elev.msl)=utm17
elev.msl.ft=elev.msl*3.28084

plot(elev.msl.ft)

tmp=elev.msl.ft>8

image(tmp,col=adjustcolor(c("dodgerblue",NA),0.75))

# imagry ------------------------------------------------------------------
library(ceramic)
# ceramic public token for API
public.token="pk.eyJ1IjoicGp1bGlhbiIsImEiOiJjanllbmJ0eXkxMzV0M2dzNXh5NGRlYXdqIn0.g4weKGOt1WdNZLg2hxBz1w"
Sys.setenv(MAPBOX_API_KEY=public.token)

AOI=raster::extent(elev.msl)
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm17

roi=raster::extent(spTransform(AOI.poly,wgs84))
im <- cc_location(roi,zoom=14)
plotRGB(im)
im=projectRaster(im,crs=utm17)
im=setValues(im,scales::rescale(values(im), c(0,255)))


bbox.lims=bbox(AOI)
# png(filename=paste0(plot.path,"/elevationData.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5))
layout(matrix(1:4,2,2,byrow=T))

b=seq(-5,25,5)
#cols=cm.colors(length(b)-1,alpha=0.9,rev=F);#
cols=hcl.colors(length(b)-1,"BluGrn",alpha=1,rev = F)
plot(shore,border="grey",bg="black",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
image(elev.msl.ft,breaks=b,col=cols,add=T)
mapmisc::scaleBar(utm17,"bottomleft",bty="n",cex=0.75,seg.len=4,outer=F,col="white");

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(b,
        cols,
        "Elevation\n(Ft, MSL)",
        x.max=0.6,x.min=0.4,
        leg.type="continuous")
legend(0.5,0.18,legend=c("No Data"),
       pch=22,lty=c(NA),lwd=c(0.1),
       col=NA,pt.bg=c("black"),
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=2,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)

tmp=elev.msl.ft>4
plotRGB(im)
image(tmp,col=adjustcolor(c("dodgerblue",NA),0.75),add=T)
mtext(side=3,adj=0,"Water Level > 4 Ft MSL",cex=0.75,line=-2)

tmp=elev.msl.ft>8
plotRGB(im)
image(tmp,col=adjustcolor(c("dodgerblue",NA),0.75),add=T)
mtext(side=3,adj=0,"Water Level > 8 Ft MSL",cex=0.75,line=-2)

dev.off()