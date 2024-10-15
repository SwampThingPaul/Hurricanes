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
wgs=CRS("+proj=longlat +datum=WGS84")

proj.string= '+init=epsg:3086'
proj=CRS(proj.string)

tmap_mode("view")


tz.con=function(datetime,to.tz){
  as.POSIXct(format(datetime,tz=to.tz,usetz=T),tz=to.tz)
}


# GIS data ----------------------------------------------------------------
# rainareas=spTransform(readOGR(paste0(GIS.path.gen,"/AHED_release/AHED_20171102.gdb"),"RAINAREA"),wgs)
# watershedareas=spTransform(readOGR(paste0(GIS.path.gen,"/AHED_release/AHED_20171102.gdb"),"WATERSHED"),wgs)
# 
# plot(rainareas)
# plot(watershedareas)

wmd.mon=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD_Monitoring_20200221"),"Environmental_Monitoring_Stations"),wgs)

# Hurricane Data Download -------------------------------------------------

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

plot(hur.pt)

# states and counties -----------------------------------------------------
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

# NWIS Data ---------------------------------------------------------------

pCode="00065";# gage height
startDate <- min(subset(hur.pt,STORMNAME=="IAN")$datetime)
endDate <- max(subset(hur.pt,STORMNAME=="IAN")$datetime)

# tmp=readNWISdata(
#   service="iv",
#   parameterCd=pCode,
#   sites = "02292900",
#   startDate = format(startDate,"%Y-%m-%d"),
#   endDate = format(endDate,"%Y-%m-%d"),
#   tz = "America/New_York")


sites=c("02292900","02291500","255654081350200","255534081324000",
        "255432081303900",'255327081275900',"255138081321701","02290928"," 02290930","250802081035500")

sites.info=dataRetrieval::readNWISdata(service = "site",
                                       seriesCatalogOutput=TRUE,
                                       sites=sites)
sites.info=subset(sites.info,parm_cd%in%c(pCode,"63158"))
sites.info$begin_date=as.Date(sites.info$begin_date)
sites.info$end_date=as.Date(sites.info$end_date)
sites.info=subset(sites.info,begin_date<as.Date(startDate)&end_date>as.Date(endDate))

sites.info=unique(sites.info[,c("site_no","station_nm","dec_lat_va","dec_long_va")])

tmp=readNWISdata(
  service="iv",
  parameterCd=pCode,
  sites = "255654081350200",
  startDate = format(startDate,"%Y-%m-%d"),
  endDate = format(endDate,"%Y-%m-%d"))

sites2=sites.info$site_no

nwis_data=data.frame()
for(i in 1:length(sites2)){
  if(sites2[i]%in%c("02292900")){next}
  tmp=readNWISdata(
    service="iv",
    parameterCd=pCode,
    sites = sites2[i],
    startDate = format(startDate,"%Y-%m-%d"),
    endDate = format(endDate,"%Y-%m-%d"))
  tmp=renameNWISColumns(tmp)
  nwis_data=rbind(tmp,nwis_data)
}

# Add S79 (Caloosahatchee USACE stage)
tmp=readNWISdata(
  service="iv",
  parameterCd=pCode,
  sites = "02292900",
  startDate = format(startDate,"%Y-%m-%d"),
  endDate = format(endDate,"%Y-%m-%d"))
tmp=renameNWISColumns(tmp)

head(nwis_data)
head(tmp)

tmp=tmp[,c("agency_cd","site_no","dateTime","USACE.DWS_GH_Inst","USACE.DWS_GH_Inst_cd",'tz_cd')]
colnames(tmp)=names(nwis_data)
nwis_data=rbind(nwis_data,tmp)


# NOAA data ---------------------------------------------------------------
conv.val=-4.12--2.84#(based on difference between elevation at Naples Pier NAVD88 = -4.12 and NGVD29 = -2.94)

library(noaaoceans)
startDate <- min(subset(hur.pt,STORMNAME=="IAN")$datetime)
endDate <- max(subset(hur.pt,STORMNAME=="IAN")$datetime)

tmp=query_coops_data(station_id="8725110",
                     start_date = format(startDate,'%Y%m%d'),
                     end_date = format(endDate,'%Y%m%d'),
                     data_product = "water_level",
                     datum="NAVD")
tmp$v=as.numeric(tmp$v)
tmp$t=as.POSIXct(tmp$t,tz="UTC")

tmp=merge(tmp,data.frame(station="8725110",t=seq(startDate,endDate,"6 min"),fill=1),c("t","station"),all.y=T)
tmp$WL.NGVD29=with(tmp,ifelse(is.na(v),-99,v-conv.val))

plot(WL.NGVD29~t,tmp)
noaa.tides=tmp


# SFWMD -------------------------------------------------------------------
bk.dbks=data.frame(SITE=c("GORDON 2_T","COCO1_T","HC1_T"),
                   DBKEY=c("QS283","IX014","67939"),
                   type="breakpoint")

wmd.bk=data.frame()
for(i in 1:nrow(bk.dbks)){
  tmp=DBHYDRO_breakpoint(as.Date(startDate),as.Date(endDate),bk.dbks$DBKEY[i])
  tmp$DBKEY=as.character(bk.dbks$DBKEY[i])
  wmd.bk=rbind(wmd.bk,tmp)
  print(i)
}
wmd.bk=merge(wmd.bk,bk.dbks,"DBKEY")

wmd.bk$datetime.utc=as.POSIXct(tz.con(wmd.bk$DATETIME,"UTC"))
wmd.bk$datetime.hr.utc=with(wmd.bk,as.POSIXct(paste(format(wmd.bk$datetime.utc,"%Y-%m-%d"),format(wmd.bk$datetime.utc,"%H"),":00:00"),
                                                  form="%Y-%m-%d %H :%M:%S",tz="UTC"))

wmd.xtab=dcast(wmd.bk,datetime.hr.utc~SITE,value.var = "Data.Value",mean)

plot(COCO1_T~datetime.hr.utc,wmd.xtab)
plot(GORDON_2_T~datetime.hr.utc,wmd.xtab)
plot(HC1_T~datetime.hr.utc,wmd.xtab)

subset(wmd.mon,STATION%in%bk.dbks$SITE)@data

subset(wmd.bk,is.na(Data.Value)==T)
wmd.bk$Data.Value=with(wmd.bk,ifelse(is.na(Data.Value)==T,0,Data.Value))
# Combine data ------------------------------------------------------------
attributes(nwis_data$dateTime)
nwis_data2=nwis_data[,c("site_no","dateTime","GH_Inst")]

attributes(noaa.tides$t)
noaa.tides2=noaa.tides[,c("station","t","WL.NGVD29")]

colnames(noaa.tides2)=names(nwis_data2)

attributes(wmd.bk$datetime.hr.utc)
wmd.bkb2=wmd.bk[,c('SITE',"datetime.hr.utc","Data.Value")]
colnames(wmd.bkb2)=names(nwis_data2)

dat.all=rbind(nwis_data2,noaa.tides2,wmd.bkb2)

head(dat.all)
tail(subset(dat.all,site_no=="02290928"))

## fill gaps in data
# tmp1=subset(dat.all,!(site_no%in%c("255534081324000","255327081275900","02290928")))
# 
# fill.dat=expand.grid(site_no=c("255534081324000","255327081275900","02290928"),
#                      dateTime=seq(startDate,endDate,"15 min"))
# 
# tmp2=subset(dat.all,site_no%in%c("255534081324000","255327081275900","02290928"))
# tmp2=merge(tmp2,
#            fill.dat,c("site_no","dateTime"),all.y=T)
# tmp2$GH_Inst[is.na(tmp2$GH_Inst)]=-99
# 
# dat.all=rbind(tmp1,tmp2)

### 
wmd.sites=subset(wmd.mon,STATION%in%bk.dbks$SITE)@data
wmd.sites=wmd.sites[,c("STATION","STATION_DE","LAT","LONG")]
colnames(wmd.sites)=names(sites.info)

noaa.sites=data.frame(site_no="8725110",
                      station_nm="NOAA Naples Pier",
                      dec_lat_va=26.131459,
                      dec_long_va=-81.808908)

sites.info=rbind(sites.info,wmd.sites,noaa.sites)

site.name=data.frame(
  site_no=c("02292900","02291500","COCO1_T","GORDON 2_T","8725110","HC1_T","255534081324000",
            "255327081275900",'02290928',"02290930","250802081035500"),
  site_name_plt=c("Caloosahatchee River","Imperial River",'Cocohatchee River',"Gordon River",'Naples Pier',"Henderson Creek",
                  "Pumpkin River",
                  'East River',"Barron River",'Turner River','East Side Creek'))

sites.info=merge(sites.info,site.name,"site_no")

sites.info.shp=SpatialPointsDataFrame(sites.info[,c("dec_long_va","dec_lat_va")],
                                      data=sites.info,
                                      proj4string = wgs)


# rainfall ----------------------------------------------------------------
rain.data=read.csv(paste0(data.path,"SFWMD/NexradRainData.txt"),skip=4)
rain.data$TIME=with(rain.data,ifelse(TIME>0&TIME<1000,paste0("0",TIME),ifelse(TIME==0,"0000",as.character(TIME))))

rain.data$datetime=with(rain.data,as.POSIXct(paste(DATE,TIME),"%m/%d/%Y %H%M",tz="UTC"))

rain.data=subset(rain.data,datetime>=as.POSIXct("2022-09-28 04:00:00","UTC"))
rain.data$cumRain=with(rain.data,ave(VALUE,POLYGON,FUN=function(x)cumsum(x)))

plot(VALUE~datetime,subset(rain.data,POLYGON=="BIG CYPRESS PRESERVE"))

## AHED Watershed area
rain.data2=read.csv(paste0(data.path,"SFWMD/NexradRainData_AHEDwatershed.txt"),skip=4)
rain.data2$TIME=with(rain.data2,ifelse(TIME>0&TIME<1000,paste0("0",TIME),ifelse(TIME==0,"0000",as.character(TIME))))
rain.data2$datetime=with(rain.data2,as.POSIXct(paste(DATE,TIME),"%m/%d/%Y %H%M",tz="UTC"))
rain.data2=subset(rain.data2,datetime>=as.POSIXct("2022-09-28 04:00:00","UTC"))
rain.data2$cumRain=with(rain.data2,ave(VALUE,POLYGON,FUN=function(x)cumsum(x)))

plot(VALUE~datetime,.)

# Visulize ----------------------------------------------------------------
step.ls=seq(as.POSIXct("2022-09-28 04:00:00","UTC"),as.POSIXct("2022-10-02 03:00:00","UTC"),"1 hours")
rng.vals=ddply(subset(dat.all,GH_Inst!=-99&dateTime>=step.ls[1]),"site_no",summarise,
               min.val=floor(min(GH_Inst,na.rm=T)),
               max.val=ceiling(max(GH_Inst,na.rm=T)+max(GH_Inst,na.rm=T)*0.3))
rng.vals[rng.vals$site_no=="02291500","max.val"]=13
rng.vals$diff.val=rng.vals$max.val-rng.vals$min.val
rng.vals$by.y.vals=with(rng.vals,ifelse(diff.val>6,floor(diff.val/2),
                                        ifelse(diff.val<=6&diff.val>=2,floor(diff.val/2),0.5)))
rng.vals

sites.info

site.ls=subset(site.name,!(site_name_plt%in%c("Pumpkin River","East River","East Side Creek")))$site_no
sites.info.shp=subset(sites.info.shp,!(site_name_plt%in%c("Pumpkin River","East River","East Side Creek")))

logo=png::readPNG(paste0(plot.path,"ConSWFL.png"))

bbox.lims=bbox(subset(state.shp,state=="florida"));# bbox(spTransform(gBuffer(spTransform(sites.info.shp,utm17),width=20000),wgs));#
xlim.val=as.POSIXct(c(startDate,endDate),tz="UTC");xmaj=seq(xlim.val[1],xlim.val[2],"2 days");xmin=seq(xlim.val[1],xlim.val[2],"6 hours")

# rain.bks=seq(0,9,0.5)
# pal=hcl.colors(length(rain.bks), "Blues",alpha=0.75,rev=T)
for(i in 1:length(step.ls)){
  step=step.ls[i]
  
  png(filename=paste0(plot.path,"/pngs/SWFL/",format(step,"%Y%m%d_%H"),"_animation.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
  par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1.5,1.75,0.5,2))
  layout(matrix(c(rep(1,8),2:9),8,2,byrow=F),widths=c(1,0.5))
  
  plot(counties.shp,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,col="ivory")
  # tmp=subset(rain.data,datetime==step)
  # tmp$col.val=findInterval(tmp$cumRain,rain.bks)
  # tmp.rf=merge(rainareas,tmp,by.x="NAME","POLYGON",all.x=T)
  # tmp.rf$col.val=with(tmp.rf@data,ifelse(is.na(col.val)==T,1,col.val))
  # plot(tmp.rf,col=pal[tmp.rf$col.val],lwd=0.01,border="grey",add=T)
  plot(sites.info.shp,add=T,pch=21,bg="dodgerblue1",cex=1.5,lwd=0.1)
  text(subset(sites.info.shp,!site_no%in%c("8725110","02290930","02290928")),"site_name_plt",pos=4,cex=0.5,halo=T)
  text(subset(sites.info.shp,site_no%in%c("02290930")),"site_name_plt",pos=1,cex=0.5,halo=T,offset=0.2)
  text(subset(sites.info.shp,site_no%in%c("8725110","02290928")),"site_name_plt",pos=2,cex=0.5,halo=T,offset=0.2)
  plot(hur.ln,lwd=1.5,col="red",add=T)
  tmp.pt=subset(hur.pt,datetime<step)
  # tmp.pt=subset(tmp.pt,datetime==max(tmp.pt$datetime))
  plot(tmp.pt,pch=21,bg=adjustcolor("red",0.5),color=adjustcolor("black",0.5),add=T,cex=3,lwd=0.1)
  plot(subset(state.shp,state!="florida"),col="grey",add=T,lwd=0.1)
  box(lwd=1)
  mapmisc::scaleBar(wgs,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
  mtext(side=3,line=-1.25,paste0(" ",format(step,'%d %B %Y %H:%M'),' UTC'))
  grid::grid.raster(logo,x=0.05,y=0.125,just=c("left","bottom"),width=grid::unit(1,"inches"))
  
  par(mar=c(0.5,0.1,0.7,2))
  for(j in 1:length(site.ls)){
    tmp=subset(dat.all,site_no==site.ls[j])
    tmp2=subset(dat.all,site_no==site.ls[j]&dateTime<=step)
    tmp3=subset(dat.all,site_no==site.ls[j]&dateTime==step)
    # ylim.val=c(-0.1,1.1)
    lims=subset(rng.vals,site_no==site.ls[j])
    ylim.val=c(lims$min.val,lims$max.val)
    by.y=lims$by.y.vals
    ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    
    plot(GH_Inst~dateTime,tmp,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,yaxs="i")
    with(tmp,shaded.range(dateTime,rep(-10,length(dateTime)),GH_Inst,"grey",lty=1))
    with(tmp2,shaded.range(dateTime,rep(-10,length(dateTime)),GH_Inst,"red",lty=1))
    
    axis_fun(4,ymaj,ymin,format(ymaj),cex=0.9)
    if(j==8){axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.75,cex=0.5)}else{axis_fun(1,xmaj,xmin,NA)}
    mtext(side=3,line=-0.5,cex=0.5,subset(sites.info,site_no==site.ls[j])$site_name_plt)
    
    if(nrow(tmp3)==0){next}else{
      points(GH_Inst~dateTime,tmp3,pch=21,bg="red",cex=1,lwd=0.5)  
      with(tmp3,segments(dateTime,-10,dateTime,GH_Inst,col="red",lwd=1))
    }
  }
  mtext(side=4,line=0.25,outer=T,"Stage Elevation (Ft, NGVD29)")
  
  dev.off()
  print(i)
}

file.names=paste0(plot.path,"/pngs/SWFL/",format(step.ls,"%Y%m%d_%H"),"_animation.png")

gifski::gifski(file.names, 
               delay = 20 / 100, 
               gif_file  = paste0(plot.path,"HurricaneIan_stage_inst_SWFL_gifski.gif"),
               loop = T,
               width=6.5*200,
               height=5*200)


plot(GH_Inst~dateTime,subset(dat.all,site_no=="GORDON 2_T"&dateTime>=as.POSIXct("2022-09-27 20:00:00","UTC")))
min(subset(dat.all,site_no=="GORDON 2_T"&dateTime>=as.POSIXct("2022-09-27 20:00:00","UTC"))$GH_Inst)
max(subset(dat.all,site_no=="GORDON 2_T"&dateTime>=as.POSIXct("2022-09-27 20:00:00","UTC"))$GH_Inst)

plot(GH_Inst~dateTime,subset(dat.all,site_no=="COCO1_T"&dateTime>=as.POSIXct("2022-09-27 20:00:00","UTC")))
min(subset(dat.all,site_no=="COCO1_T"&dateTime>=as.POSIXct("2022-09-27 20:00:00","UTC"))$GH_Inst)
max(subset(dat.all,site_no=="COCO1_T"&dateTime>=as.POSIXct("2022-09-27 20:00:00","UTC"))$GH_Inst)
10.09-2.09
