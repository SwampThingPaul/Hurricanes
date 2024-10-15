# Libraries
library(chron) # package for creating chronological objects
library(ncdf4)  # package to handle NetCDF

library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(tmap) # package for plotting map data
library(RColorBrewer) # package for color palettes

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
wgs=CRS("+proj=longlat +datum=WGS84")

proj.string= '+init=epsg:3086'
proj=CRS(proj.string)

tmap_mode("view")

## Custom Functions
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


# Hurricane Data Download -------------------------------------------------
library(httr)
library(magrittr)

# nhc.url <- "http://www.nhc.noaa.gov/gis/best_track/%s%s%s_best_track.zip"
# download.url <- sprintf(nhc.url, "al", "09", "2022")
# 
# dest=paste0(data.path,"hurricane/al092022_best_track.zip")
# download.file(download.url,dest,mode="wb",cacheOK = F)
# 
# unzip(dest,
#       exdir=paste0(data.path,"hurricane/al092022"))

hur.ln=spTransform(readOGR(paste0(data.path,"hurricane/al092022"),"AL092022_lin"),wgs)
hur.pt=spTransform(readOGR(paste0(data.path,"hurricane/al092022"),"AL092022_pts"),wgs)
hur.rad=spTransform(readOGR(paste0(data.path,"hurricane/al092022"),"AL092022_radii"),wgs)

hur.pt$datetime=with(hur.pt@data,as.POSIXct(paste(2022,MONTH,DAY,HHMM,sep="-"),"%Y-%m-%d-%H%M",tz="UTC"))


# states and counties -----------------------------------------------------
library(maptools)
library(maps)

map <- maps::map("state", fill=TRUE, plot = FALSE)
IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
map.state.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
pid <- sapply(slot(map.state.sp, "polygons"), function(x) slot(x, "ID")) 
state.shp=SpatialPolygonsDataFrame(map.state.sp,
                                   data.frame(ID=1:length(pid),
                                              state=pid,row.names = pid))
map.state.sp.t=spTransform(map.state.sp,proj)

map <- maps::map("county", fill=TRUE, plot = FALSE)
IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
map.county.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

pid <- sapply(slot(map.county.sp, "polygons"), function(x) slot(x, "ID")) 

counties.shp=SpatialPolygonsDataFrame(map.county.sp,
                                      data.frame(ID=1:length(pid),
                                                 state=sapply(strsplit(pid,","),"[",1),
                                                 county=sapply(strsplit(pid,","),"[",2),row.names = pid))

map.county.sp.t=spTransform(map.county.sp,proj)
# Copernacus data ---------------------------------------------------------

dat.nc<-nc_open(paste0(data.path,"copernicus/adaptor.mars.internal-1666025294.6629217-29432-14-d1c16e4e-1a4b-425c-8cc0-6036aeb430e6.nc"))
print(dat.nc)

lon <- ncvar_get(dat.nc,"longitude");# extracts longitude
nlon <- dim(lon);# returns the number of records

lat <- ncvar_get(dat.nc,"latitude");# extracts latitude
nlat <- dim(lat);# returns the number of records

time <- ncvar_get(dat.nc,"time");# extracts time
tunits <- ncatt_get(dat.nc,"time","units");# assigns units to time
nt <- dim(time)

dname="tp"
tmp_array <- ncvar_get(dat.nc,dname)
dlname <- ncatt_get(dat.nc,dname,"long_name")
dunits <- ncatt_get(dat.nc,dname,"units")
fillvalue <- ncatt_get(dat.nc,dname,"_FillValue")

nc_close(dat.nc)

tustr <- strsplit(tunits$value, " ")
origin.val=paste(unlist(tustr)[3],unlist(tustr)[4])
time.val=as.POSIXct(origin.val,tz="UTC")+as.difftime(time,unit="hours")

tmp_array[tmp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tmp_array[,,1])))


# outside of the loop

## loop
m=2
for(m in 1:120){
slice.tmp <- tmp_array[,,m]
time.val[m];#corresponding day

slice.r2=raster(t(slice.tmp),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs=wgs)
slice.r2=calc(slice.r2,fun=function(x) x*39.3701)

plot(hur.ln)
plot(state.shp,add=T)
image(slice.r2,add=T)


slice.r=stack(slice.r,slice.r2)
slice.r.sum=calc(slice.r,sum,na.rm=T)
plot(slice.r.sum)
}


step.ls=seq(as.POSIXct("2022-09-26 00:00:00","UTC"),as.POSIXct("2022-09-30 23:00:00","UTC"),"1 hours")

logo=png::readPNG(paste0(plot.path,"horiz_SCCF_Logo.png"))

bbox.lims=bbox(subset(state.shp,state=="florida"))

m <- 1
slice.tmp <- tmp_array[,,m]
time.val[m];#corresponding day

slice.r=raster(t(slice.tmp),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs=wgs)
slice.r[slice.r>0]=0

b=seq(0,18,0.1)
# b[1]=0.01
# pal=viridis::plasma(length(b)-1,alpha=0.5)
# cols=c(rgb(74/255,140/255,186/255),rgb(255/255,244/255,179/255),rgb(224/255,46/255,33/255))
cols=c("dodgerblue1","khaki","indianred1")
pal=adjustcolor(colorRampPalette(cols)(length(b)-1),0.5)
pal=colorRampPalette(cols)(length(b)-1)
for(i in 1:length(step.ls)){
  step=step.ls[i]
  
  slice.tmp <- tmp_array[,,i]
  slice.r2=raster(t(slice.tmp),xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat),crs=wgs)
  slice.r2=calc(slice.r2,fun=function(x) x*39.3701)
  # slice.r2[slice.r2<=0.00004]=NA
  
  slice.r=stack(slice.r,slice.r2)
  slice.r.sum=calc(slice.r,sum,na.rm=T)
  # slice.r.sum[slice.r.sum<0.0039]=NA
  
  png(filename=paste0(plot.path,"/pngs/rainfall/",format(step,"%Y%m%d_%H"),"_rainfall_animation.png"),width=5.5,height=5,units="in",res=200,type="windows",bg="white")
  par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.1,0.1,0.1,0.1))
  layout(matrix(1:2,1,2,byrow=F),widths=c(1,0.3))
  
  plot(counties.shp,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,col=NA,border=NA)
  image(slice.r.sum,add=T,breaks=b,col=pal)
  plot(subset(state.shp,state!="florida"),col=NA,add=T,lwd=0.1)
  plot(subset(counties.shp,state=="florida"),add=T,col=NA,lwd=0.05)
  plot(hur.ln,lwd=1.5,col="red",add=T)
  tmp.pt=subset(hur.pt,datetime<step)
  plot(tmp.pt,pch=21,bg="red",color=adjustcolor("black",0.5),add=T,cex=1.5,lwd=0.1)
  
  box(lwd=1)
  mapmisc::scaleBar(wgs,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
  mtext(side=3,adj=0,line=-1.25,paste0(" ",format(step,'%d %B %Y %H:%M'),' UTC'))
  
  plot(0:1,0:1,ann=F,axes=F,type="n")
  leg.fun(b,
          pal,
          "Cumulative Rainfall\n(inches)",
          x.max=0.6,x.min=0.3,
          leg.type="continuous")
  mtext(side=1,line=-1.25,adj=1,"Data Source: ERA5 ECMWF",cex=0.5)
  dev.off()
}
  

file.names=paste0(plot.path,"/pngs/rainfall/",format(step.ls,"%Y%m%d_%H"),"_rainfall_animation.png")

gifski::gifski(file.names, 
               delay = 10 / 100, 
               gif_file  = paste0(plot.path,"HurricaneIan_rainfall_inst_gifski.gif"),
               loop = T,
               width=5.5*200,
               height=5*200)
