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


# sites=c("02299230",'02296750',"02292900","02291500","02291001","02232400")

sites=c("02359170", "02330150","02326900","02326550","02326050","02325543","02324170","02323592","02313272",
  "02299230",'02296750',"02292900","02291500","02291001","02232400")
sites.info=dataRetrieval::readNWISdata(service = "site",
                            seriesCatalogOutput=TRUE,
                            sites=sites)
sites.info=subset(sites.info,parm_cd%in%c(pCode,"63158"))
sites.info$begin_date=as.Date(sites.info$begin_date)
sites.info$end_date=as.Date(sites.info$end_date)
sites.info=subset(sites.info,begin_date<as.Date(startDate)&end_date>as.Date(endDate))

sites.info=unique(sites.info[,c("site_no","station_nm","dec_lat_va","dec_long_va")])

nwis_data=data.frame()
for(i in 1:length(sites)){
  if(sites[i]%in%c("02292900","02323592")){next}
  tmp=readNWISdata(
    service="iv",
    parameterCd=pCode,
    sites = sites[i],
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

# Add Suwannee River (stream level)
tmp=readNWISdata(
  service="iv",
  parameterCd="63158",
  sites = "02323592",
  startDate = format(startDate,"%Y-%m-%d"),
  endDate = format(endDate,"%Y-%m-%d"))
tmp=renameNWISColumns(tmp)
head(nwis_data)
head(tmp)

tmp=tmp[,c("agency_cd","site_no","dateTime","X_63158_Inst","X_63158_Inst_cd",'tz_cd')]
colnames(tmp)=names(nwis_data)
nwis_data=rbind(nwis_data,tmp)

# Lake Okeechobee ---------------------------------------------------------

bk.dbks=data.frame(SITE=c("L001","L005","L006","LZ40","S133TW","S352HW","S4TW"),
                   DBKEY=c("IX846","IX865","IX875","IY030","IY368","IY688","IY763"),
                   type="breakpoint")
## missing S308HW data
lakeO.bk=data.frame()
for(i in 1:nrow(bk.dbks)){
  tmp=DBHYDRO_breakpoint(as.Date(startDate),as.Date(endDate),bk.dbks$DBKEY[i])
  tmp$DBKEY=as.character(bk.dbks$DBKEY[i])
  lakeO.bk=rbind(lakeO.bk,tmp)
  print(i)
}
lakeO.bk=merge(lakeO.bk,bk.dbks,"DBKEY")

lakeO.bk$datetime.utc=as.POSIXct(tz.con(lakeO.bk$DATETIME,"UTC"))
lakeO.bk$datetime.hr.utc=with(lakeO.bk,as.POSIXct(paste(format(lakeO.bk$datetime.utc,"%Y-%m-%d"),format(lakeO.bk$datetime.utc,"%H"),":00:00"),
                                                  form="%Y-%m-%d %H :%M:%S",tz="UTC"))

LakeO.xtab=dcast(lakeO.bk,datetime.hr.utc~SITE,value.var = "Data.Value",mean)
LakeO.xtab$mean=rowMeans(LakeO.xtab[,2:8],na.rm=T)
plot(mean~datetime.hr.utc,LakeO.xtab)

LakeO.xtab$site_no="LOK"
attributes(LakeO.xtab$datetime.hr.utc)


sites.info=rbind(sites.info,
                 data.frame(site_no=c("LOK"),
                            station_nm=c("Lake Okeechobee"),
                            dec_lat_va=c(26.950333),
                            dec_long_va=c(-80.833114)
                 )
)

# RECON Data --------------------------------------------------------------
recon.dat=function(date_min,date_max,param,site,UTC=TRUE,print.link=TRUE){
  ## Stop functions for date, site and parameters
  if(date_max<date_min){stop("Check dates, max date can be before min date.")}
  if(!(site%in%c(13,51,16,56,18,53,11,55))){stop("Check site ID")}
  # WQ params only - Nevermind need to develop further.
  # param.vals=c("cdom","temperature","chlorophyll","turbibity",
  #              "conductivity","oxygen","depth","oxygen_sat",
  #              "oxygen_percent","nitrate","nitrate_abs_254","nitrate_abs_350",
  #              "phosphate","voltage")
  RECON.sites=data.frame(ID=c(13,51,16,56,18,53,11,55),
          name=c("Shell Point","Redfish Pass","GOM","McIntyre Creek",
                 "Beautiful Island","Fort Myers","Tarpon Bay","Wave Buoy"))
  servfull <- "http://recondata.sccf.org/cgi-data/nph-data.cgi"
  utc.val=if(UTC==TRUE){"?x=utc_date"}
  param.val=paste(param,collapse=",")
  date_min <- format(date_min,"%Y%m%d")
  date_max <- format(date_max,"%Y%m%d")
  
  link=paste0(servfull,utc.val,"&amp;y=",param.val,"&amp;min_date=",date_min,"&amp;max_date=",date_max,"&amp;node=",site,"&amp;data_format=text")
  report=read.table(link,skip=1,sep = '\t',header=T)
  colnames(report) = c("Date.EST",param,"Date.UTC")
  report$Date.EST=as.POSIXct(strptime(report$Date.EST,"%F%X"),tz="EST")
  report$Date.UTC=as.POSIXct(strptime(report$Date.UTC,"%F%X"),tz="UTC")
  report$SiteName=subset(RECON.sites,ID==site)$name
  report$SiteID=site
  if(print.link==TRUE){message(link)}
  return(report)
  
}


recon.WL=data.frame()
for(i in 1:2){
  tmp=recon.dat(as.Date(startDate),as.Date(endDate),"depth",c(56,11)[i])
  recon.WL=rbind(recon.WL,tmp)
}
plot(depth~Date.EST,tmp)

recon.WL
recon.WL$depth=m.to.ft(recon.WL$depth)

# recon.WL$depth.NAVD88.ft=m.to.ft(recon.WL$depth)
recon.WL$depth.NAVD88.ft=m.to.ft(recon.WL$depth)+0.41 # (MSL to MAVD88 conversion based on Ft Myers Tide Chart)
recon.WL$depth.NAVD29.ft=recon.WL$depth.NAVD88.ft-(-1.18); #(based on difference between elevation in Tarpon Bay NAVD88 = -4.12 and NGVD29 = -2.94)

sites.info=rbind(sites.info,
                 data.frame(site_no=c(56,11),
                            station_nm=c("McIntyre Creek","Tarpon Bay"),
                            dec_lat_va=c(26.46452,26.466292),
                            dec_long_va=c(-82.10370,-82.06471)
                            )
)

panhandle.site=data.frame(
  site_no=c("02359170", "02330150","02326900","02326550",
            "02326050","02325543","02324170","02323592","02313272"),
  site_name_plt=c("Apalachicola River","Ochlockonee River","St. Marks River", 
                  "Aucilla River","Ecofina River","Fenholloway River", 
                  "Steinhatchee River","Suwannee River","Withlacoochee River"))
other.sites=data.frame(site_no=c("02232400","LOK","02291001", "02291500", "02292900","02296750", "02299230", 
                                 "56", "11"),
                       site_name_plt=c("St Johns River","Lake Okeechobee",'Barron River',"Imperial River",
                                       "Caloosahatchee River",'Peace River',"Myakka River"
                                       ,"McIntyre Creek","Tarpon Bay"))


sites.info=merge(sites.info,
                 rbind(panhandle.site,other.sites),
                 "site_no")

sites.info.shp=SpatialPointsDataFrame(sites.info[,c("dec_long_va","dec_lat_va")],
                                      data=sites.info,
                                      proj4string = wgs)


## check date format, reformat column and combine
attributes(nwis_data$dateTime)
nwis_data2=nwis_data[,c("site_no","dateTime","GH_Inst")]

attributes(recon.WL$Date.UTC)
recon.WL2=recon.WL[,c("SiteID","Date.UTC","depth")]

colnames(recon.WL2)=names(nwis_data2)

attributes(LakeO.xtab$datetime.hr.utc)
LakeO.xtab2=LakeO.xtab[,c('site_no',"datetime.hr.utc","mean")]
colnames(LakeO.xtab2)=names(nwis_data2)

dat.all=rbind(nwis_data2,recon.WL2,LakeO.xtab2)


## Normalize water levels
library(caret)
site.ls=c(panhandle.site$site_no,other.sites$site_no)# c("02232400","02291001", "02291500", "02292900","02296750", "02299230", "56", "11")

dat.all2=data.frame()
for(i in 1:length(site.ls)){
  tmp=subset(dat.all,site_no==site.ls[i])
  data.val=tmp$GH_Inst
  
  process <- preProcess(as.data.frame(data.val), method=c("range"))
  norm_scale <- predict(process, as.data.frame(data.val))
  
  tmp$norm.val=norm_scale$data.val
  dat.all2=rbind(dat.all2,tmp)
  print(i)
}

# tail(dcast(dat.all2,dateTime~site_no,value.var = "norm.val",mean))



# Visulize ----------------------------------------------------------------
site.ls=c(other.sites$site_no)
sites.info.shp2=subset(sites.info.shp,site_no%in%other.sites$site_no)

logo=png::readPNG(paste0(plot.path,"horiz_SCCF_Logo.png"))

bbox.lims=bbox(subset(state.shp,state=="florida"))
xlim.val=as.POSIXct(c(startDate,endDate),tz="UTC");xmaj=seq(xlim.val[1],xlim.val[2],"1 days");xmin=seq(xlim.val[1],xlim.val[2],"6 hours")
step.ls=seq(as.POSIXct("2022-09-28 04:00:00","UTC"),as.POSIXct("2022-10-02 03:00:00","UTC"),"1 hours")
for(i in 1:length(step.ls)){
  step=step.ls[i]
  
  png(filename=paste0(plot.path,"/pngs/inst/",format(step,"%Y%m%d_%H"),"_animation.png"),width=5.5,height=5,units="in",res=200,type="windows",bg="white")
  par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1.5,0.5,0.5,0.5))
  layout(matrix(c(rep(1,9),2:10),9,2,byrow=F),widths=c(1,0.5))
  
  plot(counties.shp,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,col="ivory")
  plot(sites.info.shp2,add=T,pch=21,bg="dodgerblue1",cex=1.5,lwd=0.1)
  text(subset(sites.info.shp2,!site_no%in%c("11","02299230","02296750")),"site_name_plt",pos=4,cex=0.5,halo=T)
  text(subset(sites.info.shp2,site_no%in%c("11","02299230","02296750")),"site_name_plt",pos=2,cex=0.5,halo=T)
  plot(hur.ln,lwd=1.5,col="red",add=T)
  tmp.pt=subset(hur.pt,datetime<step)
  # tmp.pt=subset(tmp.pt,datetime==max(tmp.pt$datetime))
  plot(tmp.pt,pch=21,bg=adjustcolor("red",0.5),color=adjustcolor("black",0.5),add=T,cex=3,lwd=0.1)
  plot(subset(state.shp,state!="florida"),col="grey",add=T,lwd=0.1)
  box(lwd=1)
  mapmisc::scaleBar(wgs,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
  mtext(side=3,adj=0,line=-1.25,paste0(" ",format(step,'%d %B %Y %H:%M'),' UTC'))
  grid::grid.raster(logo,x=0.02,y=0.125,just=c("left","bottom"),width=grid::unit(1.25,"inches"))
  
  par(mar=c(0.5,0.1,0.5,0.1))
  for(j in 1:length(site.ls)){
    tmp=subset(dat.all2,site_no==site.ls[j])
    tmp2=subset(dat.all2,site_no==site.ls[j]&dateTime<=step)
    tmp3=subset(dat.all2,site_no==site.ls[j]&dateTime==step)
    ylim.val=c(-0.1,1.1)
    
    plot(norm.val~dateTime,tmp,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,yaxs="i")
    with(tmp,shaded.range(dateTime,rep(-10,length(dateTime)),norm.val,"grey",lty=1))
    with(tmp2,shaded.range(dateTime,rep(-10,length(dateTime)),norm.val,"red",lty=1))
    
    if(j==9){axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.75,cex=0.5)}else{axis_fun(1,xmaj,xmin,NA)}
    mtext(side=3,adj=0,line=-1.1,cex=0.7,subset(sites.info,site_no==site.ls[j])$site_name_plt)
    if(j==1){mtext(side=3,adj=1,"Normalized stage",cex=0.5,line=-0.1)}
    if(nrow(tmp3)==0){next}else{
    with(tmp3,points(norm.val~dateTime,pch=21,bg="red",cex=1,lwd=0.5))
    with(tmp3,segments(dateTime,-10,dateTime,norm.val,col="red",lwd=1))
    }
  }
  dev.off()
  print(i)
}


file.names=paste0(plot.path,"/pngs/inst/",format(step.ls,"%Y%m%d_%H"),"_animation.png")

gifski::gifski(file.names, 
               delay = 20 / 100, 
               gif_file  = paste0(plot.path,"HurricaneIan_stage_inst_gifski.gif"),
               loop = T,
               width=5.5*200,
               height=5*200)



# Visulize2 ----------------------------------------------------------------
step.ls=seq(as.POSIXct("2022-09-28 04:00:00","UTC"),as.POSIXct("2022-10-02 03:00:00","UTC"),"1 hours")
rng.vals=ddply(subset(dat.all2,dateTime>=step.ls[1]),"site_no",summarise,
      min.val=floor(min(GH_Inst,na.rm=T)),
      max.val=ceiling(max(GH_Inst,na.rm=T)+max(GH_Inst,na.rm=T)*0.3))
rng.vals[rng.vals$site_no=="02326900","max.val"]=6.5
rng.vals[rng.vals$site_no=="LOK","max.val"]=14
rng.vals$diff.val=rng.vals$max.val-rng.vals$min.val
rng.vals$by.y.vals=with(rng.vals,ifelse(diff.val>6,floor(diff.val/2),
                                        ifelse(diff.val<=6&diff.val>=2,floor(diff.val/2),0.5)))
rng.vals

sites.info

site.ls1=panhandle.site$site_no
site.ls2=other.sites$site_no

logo=png::readPNG(paste0(plot.path,"horiz_SCCF_Logo.png"))

bbox.lims=bbox(subset(state.shp,state=="florida"))
xlim.val=as.POSIXct(c(startDate,endDate),tz="UTC");xmaj=seq(xlim.val[1],xlim.val[2],"2 days");xmin=seq(xlim.val[1],xlim.val[2],"6 hours")

for(i in 1:length(step.ls)){
  step=step.ls[i]
  
  png(filename=paste0(plot.path,"/pngs/statewide/",format(step,"%Y%m%d_%H"),"_animation.png"),width=6.5,height=5.5,units="in",res=200,type="windows",bg="white")
  par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1.5,1.75,0.5,2))
  layout(matrix(c(1:9,rep(10,9),11:19),9,3,byrow=F),widths=c(0.5,1,0.5))
  
  par(mar=c(0.5,2,0.7,0.1))
  for(j in 1:length(site.ls1)){
    tmp=subset(dat.all2,site_no==site.ls1[j])
    tmp2=subset(dat.all2,site_no==site.ls1[j]&dateTime<=step)
    tmp3=subset(dat.all2,site_no==site.ls1[j]&dateTime==step)
    # ylim.val=c(-0.1,1.1)
    lims=subset(rng.vals,site_no==site.ls1[j])
    ylim.val=c(lims$min.val,lims$max.val)
    by.y=lims$by.y.vals
    ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    
    plot(GH_Inst~dateTime,tmp,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,yaxs="i")
    with(tmp,shaded.range(dateTime,rep(-10,length(dateTime)),GH_Inst,"grey",lty=1))
    with(tmp2,shaded.range(dateTime,rep(-10,length(dateTime)),GH_Inst,"red",lty=1))
    
    axis_fun(2,ymaj,ymin,format(ymaj))
    if(j==9){axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.75,cex=0.5)}else{axis_fun(1,xmaj,xmin,NA)}
    mtext(side=3,line=-0.5,cex=0.5,subset(sites.info,site_no==site.ls1[j])$site_name_plt)
    
    if(nrow(tmp3)==0){next}else{
      with(tmp3,points(GH_Inst~dateTime,pch=21,bg="red",cex=1,lwd=0.5))
      with(tmp3,segments(dateTime,-10,dateTime,GH_Inst,col="red",lwd=1))
    }
  }
  mtext(side=2,line=0.25,outer=T,"Stage Elevation (Ft, NGVD29)")
  
  par(mar=c(0.1,0.1,0.1,0.1))
  plot(counties.shp,ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,col="ivory")
  plot(sites.info.shp,add=T,pch=21,bg="dodgerblue1",cex=1.5,lwd=0.1)
  text(subset(sites.info.shp,!site_no%in%c("11","02299230","02296750",
                                           "02330150","LOK","02326900",
                                           "02325543","02326550","02326050")),"site_name_plt",pos=4,cex=0.5,halo=T)
  text(subset(sites.info.shp,site_no%in%c("11","02299230","02296750","02330150")),"site_name_plt",pos=2,cex=0.5,halo=T)
  text(subset(sites.info.shp,site_no%in%c("LOK","02326900")),"site_name_plt",pos=3,cex=0.5,halo=T)
  
  plot(hur.ln,lwd=1.5,col="red",add=T)
  tmp.pt=subset(hur.pt,datetime<step)
  # tmp.pt=subset(tmp.pt,datetime==max(tmp.pt$datetime))
  plot(tmp.pt,pch=21,bg=adjustcolor("red",0.5),color=adjustcolor("black",0.5),add=T,cex=3,lwd=0.1)
  plot(subset(state.shp,state!="florida"),col="grey",add=T,lwd=0.1)
  box(lwd=1)
  mapmisc::scaleBar(wgs,"bottomleft",bty="n",cex=1,seg.len=4,outer=F)
  mtext(side=3,line=-1.25,paste0(" ",format(step,'%d %B %Y %H:%M'),' UTC'))
  grid::grid.raster(logo,x=0.28,y=0.125,just=c("left","bottom"),width=grid::unit(1.25,"inches"))
  
  par(mar=c(0.5,0.1,0.7,2))
  for(j in 1:length(site.ls2)){
    tmp=subset(dat.all2,site_no==site.ls2[j])
    tmp2=subset(dat.all2,site_no==site.ls2[j]&dateTime<=step)
    tmp3=subset(dat.all2,site_no==site.ls2[j]&dateTime==step)
    # ylim.val=c(-0.1,1.1)
    lims=subset(rng.vals,site_no==site.ls2[j])
    ylim.val=c(lims$min.val,lims$max.val)
    by.y=lims$by.y.vals
    ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    
    plot(GH_Inst~dateTime,tmp,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,yaxs="i")
    with(tmp,shaded.range(dateTime,rep(-10,length(dateTime)),GH_Inst,"grey",lty=1))
    with(tmp2,shaded.range(dateTime,rep(-10,length(dateTime)),GH_Inst,"red",lty=1))
    
    axis_fun(4,ymaj,ymin,format(ymaj))
    if(j==9){axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.75,cex=0.5)}else{axis_fun(1,xmaj,xmin,NA)}
    mtext(side=3,line=-0.5,cex=0.5,subset(sites.info,site_no==site.ls2[j])$site_name_plt)
    
    if(nrow(tmp3)==0){next}else{
      with(tmp3,points(GH_Inst~dateTime,pch=21,bg="red",cex=1,lwd=0.5))
      with(tmp3,segments(dateTime,-10,dateTime,GH_Inst,col="red",lwd=1))
    }
  }
  mtext(side=4,line=0.25,outer=T,"Stage Elevation (Ft, NGVD29)")
  
  dev.off()
  print(i)
}


file.names=paste0(plot.path,"/pngs/statewide/",format(step.ls,"%Y%m%d_%H"),"_animation.png")

gifski::gifski(file.names, 
               delay = 20 / 100, 
               gif_file  = paste0(plot.path,"HurricaneIan_stage_inst_statewide_gifski.gif"),
               loop = T,
               width=6.5*200,
               height=5.5*200)




# quick summary stats -----------------------------------------------------


range.val.sum=ddply(
  subset(merge(dat.all2,sites.info,"site_no"),dateTime>=step.ls[1]),
  c("site_name_plt","site_no"),summarise,
      min.val=min(GH_Inst,na.rm=T),
      max.val=max(GH_Inst,na.rm=T))
range.val.sum$diff=range.val.sum$max.val-range.val.sum$min.val
range.val.sum
