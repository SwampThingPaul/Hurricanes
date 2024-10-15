## 
## Hurricane Milton data visualization
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

#Paths
wd="C:/Julian_LaCie/_GitHub/Hurricanes"
## double check set the wd correctly
# getwd()==wd
paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/","/_documents/"))
plot.path=paths[1]
export.path = paths[2]

utm17=st_crs("EPSG:26917")
wgs84=st_crs("EPSG:4326")


# GIS Data ----------------------------------------------------------------

SW.US=c("Georgia","Alabama","South Carolina",
        "Mississippi","Louisiana","Arkansas",
        "Tennessee","North Carolina",
        "Texas")
FL.shp=us_states(resolution ="low",states=SW.US)|>
  st_transform(utm17)

world <- ne_countries(scale = "medium", returnclass = "sf")
unique(world$admin)[order(unique(world$admin))]

xlim <- c(-120, -74.12)
ylim <- c(7.65, 38.5)

GOM = matrix(c(
  xlim[1], ylim[1],  # bottom-left corner
  xlim[2], ylim[1],  # bottom-right corner
  xlim[2], ylim[2],  # top-right corner
  xlim[1], ylim[2],  # top-left corner
  xlim[1], ylim[1]   # back to bottom-left to close the polygon
), ncol = 2, byrow = TRUE)

GOM.poly = st_polygon(list(GOM))|>
  st_sfc(crs=wgs84)|>
  st_sf()
plot(st_geometry(GOM.poly))
plot(st_geometry(world),add=T)

GOM_world=st_intersection(world,GOM.poly)|>
  st_transform(utm17)


xlim <- c(-102.15, -79.8) # c(-102.15, -74.12)
ylim <- c(17.2, 31.4) # c(7.65, 33.97)
GOM = matrix(c(
  xlim[1], ylim[1],  # bottom-left corner
  xlim[2], ylim[1],  # bottom-right corner
  xlim[2], ylim[2],  # top-right corner
  xlim[1], ylim[2],  # top-left corner
  xlim[1], ylim[1]   # back to bottom-left to close the polygon
), ncol = 2, byrow = TRUE)

GOM.poly2 = st_polygon(list(GOM))|>
  st_sfc(crs=wgs84)|>
  st_sf()

## USA counties
# link = "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Counties_Generalized/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

## Florida Counties 
link = "https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Counties_Generalized/FeatureServer/0/query?where=STATE_NAME=%27Florida%27&outFields=*&outSR=4326&f=json"

FLCounty = link|>
  st_read()|>
  st_transform(utm17)

plot(st_geometry(FLCounty))


## Hurricane best track 2024
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
# ## Milton is Hurricane 14
# tmp = readLines(paste0(link1,"bal142024.dat"))
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
# milton.track.dat = df
# lat.str.val = strsplit(milton.track.dat$Lat, "(?=[A-Za-z])", perl = TRUE)
# lon.str.val = strsplit(milton.track.dat$Lon, "(?=[A-Za-z])", perl = TRUE)
# 
# milton.track.dat$Lat.DD = with(milton.track.dat,ifelse(sapply(lat.str.val,"[",2)=="N",
#                                              as.numeric(sapply(lat.str.val,"[",1))/10,
#                                              (as.numeric(sapply(lat.str.val,"[",1))/10)*-1)
# )
# milton.track.dat$Lon.DD = with(milton.track.dat,ifelse(sapply(lon.str.val,"[",2)=="N",
#                                              as.numeric(sapply(lon.str.val,"[",1))/10,
#                                              (as.numeric(sapply(lon.str.val,"[",1))/10)*-1)
# )
# 
# milton.track.dat$DateTime = with(milton.track.dat,date.fun(paste0(
#   substr(YYYYMMDDHH,1,4),"-",
#   substr(YYYYMMDDHH,5,6),"-",
#   substr(YYYYMMDDHH,7,8)," ",
#   substr(YYYYMMDDHH,9,10)
# ),tz="UTC",form="%Y-%m-%d %H"))
# 
# milton.track.dat$DateTime.EST = date.fun(milton.track.dat$DateTime,tz="EST",form="%Y-%m-%d %H")
# milton.track.dat$Date = date.fun(milton.track.dat$DateTime,tz="UTC")
# write.csv(milton.track.dat,paste0(export.path,"milton_track.csv"),row.names = F)

milton.track.dat=read.csv(paste0(export.path,"milton_track.csv"))|>
  mutate(DateTime = date.fun(DateTime,tz="UTC",form="%Y-%m-%d %H"),
         DateTime.EST = date.fun(DateTime,tz="EST",form="%Y-%m-%d %H"),
         Date = date.fun(DateTime,tz="UTC"))

milton.track.dat.sp=st_as_sf(subset(milton.track.dat,is.na(Lat.DD)==F),coords=c("Lon.DD","Lat.DD"),crs=wgs84)|>
  st_transform(utm17)
plot(st_geometry(milton.track.dat.sp))

milton.track.dat.sp.ln=subset(milton.track.dat,is.na(DateTime)==F&is.na(Lat.DD)==F&is.na(Lon.DD)==F)|>
  st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)|>
  st_transform(utm17)|>
  dplyr::group_by(CY)|>
  dplyr::summarize(#name=dplyr::first(Name),
    minDate=min(DateTime,na.rm=T),
    maxDate=max(DateTime,na.rm=T),
    maxWind=max(as.numeric(VMAX),na.rm=T),
    do_union=FALSE)|>
  st_cast("MULTIPOINT")|>
  st_cast("LINESTRING")

plot(st_geometry(milton.track.dat.sp.ln))

# NWIS Data ---------------------------------------------------------------
library(dataRetrieval)
pCode="00065";# gage height
startDate <- date.fun("2024-10-01",tz="UTC")# min(milton.track.dat$Date)
endDate <- date.fun("2024-10-14",tz="UTC")# max(milton.track.dat$Date)

sites=c("02310674","02306028","273511082371300","023000095","02300042",
        "02299861","02299230","02296750","02292900","263949081505501",
        "02291500","02291001","02301721")
sites.info=dataRetrieval::readNWISdata(service = "site",
                                       seriesCatalogOutput=TRUE,
                                       sites=sites)
sites.info=subset(sites.info,parm_cd%in%c(pCode,"63158"))
sites.info$begin_date=as.Date(sites.info$begin_date)
sites.info$end_date=as.Date(sites.info$end_date)
# sites.info=subset(sites.info,begin_date<as.Date(startDate)&end_date>=as.Date(endDate))
sites.info=subset(sites.info,end_date>=max(milton.track.dat$Date))

sites.info=unique(sites.info[,c("site_no","station_nm","dec_lat_va","dec_long_va","alt_datum_cd")])

subset(sites.info,alt_datum_cd=="NAVD88")
sites.info=merge(sites.info,
                 data.frame(site_no = c("02292900","02299230","02310674","02301721"),
                            WL_con = c(-0.376,-0.339,-0.247,-0.272)# in meters from vertcon
                            ),
                 "site_no",all.x=T)
sites.info$WL_con=with(sites.info,ifelse(is.na(WL_con),0,m.to.ft(WL_con)))

nwis_data=data.frame()
for(i in 1:nrow(sites.info)){
  if(sites.info$site_no[i]%in%c("02292900","02323592")){next}
  tmp=readNWISdata(
    service="iv",
    parameterCd=pCode,
    sites = sites.info$site_no[i],
    startDate = format(startDate,"%Y-%m-%d"),
    endDate = format(endDate,"%Y-%m-%d"))
  tmp=renameNWISColumns(tmp)
  nwis_data=rbind(tmp,nwis_data)
}
unique(nwis_data$site_no)

subset(sites.info,!(site_no%in%unique(nwis_data$site_no)))

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

tmp=tmp[,c("agency_cd","site_no","dateTime","USACE.DWS...DWS._GH_Inst","USACE.DWS...DWS._GH_Inst_cd",'tz_cd')]
colnames(tmp)=names(nwis_data)
nwis_data=rbind(nwis_data,tmp)


nwis_fill =expand.grid(site_no = unique(nwis_data$site_no),
                       dateTime = seq(startDate,endDate,"15 mins"))
nwis_data = merge(nwis_data,nwis_fill,
                  c("site_no","dateTime"),all.y=T)

## Lake Okeechobee
bk.dbks=data.frame(SITE=c("L001","L005","L006","LZ40","S133TW","S352HW","S4TW"),
                   DBKEY=c("IX846","IX865","IX875","IY030","IY368","IY688","IY763"),
                   type="breakpoint")
## missing S308HW data
lakeO.bk=data.frame()
for(i in 1:nrow(bk.dbks)){
  tmp=DBHYDRO_daily(as.Date(startDate),as.Date(endDate),bk.dbks$DBKEY[i],vert_datum=2)
  tmp$DBKEY=as.character(bk.dbks$DBKEY[i])
  lakeO.bk=rbind(lakeO.bk,tmp)
  print(i)
}
lakeO.bk=merge(lakeO.bk,bk.dbks,"DBKEY")

lakeO.bk$datetime.utc=date.fun(lakeO.bk$DATETIME,tz="UTC",form="%Y-%m-%d %H:%M:%S")
lakeO.bk$datetime.hr.utc=date.fun(paste(format(lakeO.bk$datetime.utc,"%Y-%m-%d"),
                                        format(lakeO.bk$datetime.utc,"%H"),":00:00"),form="%Y-%m-%d %H :%M:%S",tz="UTC")

LakeO.xtab=dcast(lakeO.bk,datetime.hr.utc~SITE,value.var = "Data.Value",mean)
LakeO.xtab$mean=rowMeans(LakeO.xtab[,2:8],na.rm=T)
plot(mean~datetime.hr.utc,LakeO.xtab)

LakeO.xtab$site_no="LOK"
attributes(LakeO.xtab$datetime.hr.utc)

sites.info=rbind(sites.info,
                 data.frame(site_no=c("LOK"),
                            station_nm=c("Lake Okeechobee"),
                            dec_lat_va=c(26.950333),
                            dec_long_va=c(-80.833114),
                            alt_datum_cd="NGVD29",
                            WL_con=1.25
                 )
)


# sites.info= subset(sites.info,site_no!="02291001");# excluded Barron river site

## check date format, reformat column and combine
attributes(nwis_data$dateTime)
nwis_data2=nwis_data[,c("site_no","dateTime","GH_Inst")]
nwis_data2=merge(nwis_data2,sites.info[,c("site_no","WL_con")],"site_no",all.x=T)
nwis_data2$GH_Inst_NGVD29 = with(nwis_data2,GH_Inst+abs(WL_con))
# plot(GH_Inst~dateTime,nwis_data2)
# points(GH_Inst_NGVD29~dateTime,nwis_data2,pch=21,bg="blue",col="blue")

attributes(LakeO.xtab$datetime.hr.utc)
LakeO.xtab2=LakeO.xtab[,c('site_no',"datetime.hr.utc","mean")]
LOK_fill =expand.grid(site_no = "LOK",
                      datetime.hr.utc = seq(startDate,endDate,"1 hour"))
LakeO.xtab2=merge(LakeO.xtab2,LOK_fill,
                  c("site_no","datetime.hr.utc"),all.y=T)
LakeO.xtab2$WL_con=1.25
LakeO.xtab2$GH_Inst_NGVD29=LakeO.xtab2$mean+LakeO.xtab2$WL_con
colnames(LakeO.xtab2)=names(nwis_data2)

dat.all=rbind(nwis_data2,LakeO.xtab2)
# dat.all= subset(dat.all,site_no!="02291001");# excluded Barron river site

# ddply(dat.all,"site_no",summarise,min.diff = min(diff(dateTime)),max.diff = max(diff(dateTime)))

plot(GH_Inst_NGVD29~dateTime,subset(dat.all, site_no=="02300042"),type="l")
plot(GH_Inst_NGVD29~dateTime,subset(dat.all, site_no=="LOK"),type="l")

subset(dat.all, site_no=="02300042"&is.na(GH_Inst_NGVD29))

# Map_figures -------------------------------------------------------------
pt_track.fun = function(points){
  points|>
    st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)|>
    st_transform(utm17)|>
    dplyr::group_by(CY)|>
    dplyr::summarize(#name=dplyr::first(Name),
      minDate=min(DateTime,na.rm=T),
      maxDate=max(DateTime,na.rm=T),
      maxWind=max(as.numeric(VMAX),na.rm=T),
      do_union=FALSE)|>
    st_cast("MULTIPOINT")|>
    st_cast("LINESTRING")
}

sites.info.sp = sites.info[order(-sites.info$dec_lat_va),]|>
  st_as_sf(coords=c("dec_long_va","dec_lat_va"),crs=wgs84)|>
  st_transform(utm17)

site.ls=sites.info.sp$site_no # sites.info[order(-sites.info$dec_lat_va),]
site.names = c("Chassahowitzka R.","Hillsborough R.","Alafia R.","Manatee R.","Braden R.","Walker Cr.","Peace R.",
               "Myakka R.","Lake\nOkeechobee","Caloosahatchee R.","Imperial R.","Barron R.")
site.names2 = site.names
site.names2[site.names2=="Lake\nOkeechobee"] = "Lake Okeechobee"

sites.info.sp$station_nm

dat.all$site_no = factor(dat.all$site_no,levels = site.ls)

bbox.lims=st_bbox(FLCounty)
bbox.lims2 = st_bbox(st_buffer(st_transform(GOM.poly2,utm17),-10000))
xlim.val=date.fun(c("2024-10-04","2024-10-14"),tz="UTC");xmaj=seq(xlim.val[1],xlim.val[2],"3 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")
step.ls=seq(as.POSIXct("2024-10-04 06:00:00","UTC"),as.POSIXct("2024-10-13 06:00:00","UTC"),"1 hours")

tmp = ddply(dat.all,"site_no",summarise,min.val = min(GH_Inst_NGVD29,na.rm=T),max.val = max(GH_Inst_NGVD29,na.rm=T))
tmp = tmp[match(site.ls,tmp$site_no),]
min.y = floor(tmp$min.val);min.y[5]=min.y[5]-1# min.y[min.y==14]=13; min.y[5] = min.y[5]-1
max.y = ceiling(tmp$max.val+tmp$max.val*0.05);max.y[2]=max.y[2]+1;# max.y[tmp$site_no=="LOK"]=max.y[tmp$site_no=="LOK"]-1#  max.y[1:3]=max.y[1:3]+1;max.y[max.y==18]=17

AOI.poly <- raster::extent(FLCounty)|>
  as("SpatialPolygons")|>
  st_as_sf()
st_crs(AOI.poly) = utm17

MiltonLandFall=date.fun("2024-10-09 20:30",form="%F %R")
MiltonLandFall.utc = format(MiltonLandFall,"%F %R",tz="UTC",usetz=T)|>date.fun(form="%F %R",tz="UTC")
attributes(MiltonLandFall.utc)

(step.ls-MiltonLandFall.utc)/60  

for(i in 1:length(step.ls)){
  step.val=step.ls[i]
  t_landall = difftime(step.val,MiltonLandFall.utc,units="hours")
  
  tmp.track = subset(milton.track.dat,DateTime<=step.val)|>
    pt_track.fun()
  tmp.pts = subset(milton.track.dat,DateTime<=step.val)|>
    st_as_sf(coords=c("Lon.DD","Lat.DD"),crs=wgs84)|>
    st_transform(utm17)
  
  dat.site.tmp = subset(dat.all,dateTime<=step.val)
  
  png(filename=paste0(plot.path,"/pngs/inst/",format(step.val,"%Y%m%d_%H"),"_animation.png"),width=6.5,height=6,units="in",res=200,type="windows",bg="white")
  layout(matrix(c(1:6,rep(7,2),rep(8,4),9:14),6,3,byrow=F),widths=c(1,1,1))
  par(family="serif",mar=c(1,3.5,0.25,0.4),oma=c(3,0.1,1,0.3))
  for(j in 1:6){
    ylim.val = c(min.y[j],max.y[j]);
    by.y=floor((max.y[j]-min.y[j])/2)
    # if(by.y==0){by.y=0.5}
    ymaj=seq(ylim.val[1],ylim.val[2],by.y);
    ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    dat.site.tmp2 = subset(dat.site.tmp,site_no==site.ls[j])
    
    plot(GH_Inst_NGVD29~dateTime,dat.site.tmp2,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,yaxs="i")
    abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
    lines(GH_Inst_NGVD29~dateTime,dat.site.tmp2,col="red",lwd=2)
    points(GH_Inst_NGVD29~dateTime,subset(dat.site.tmp2,dateTime==max(dat.site.tmp2$dateTime)),bg="grey",pch=21,cex=1.25)
    with(subset(dat.site.tmp2,dateTime==max(dat.site.tmp2$dateTime)),text(dateTime,GH_Inst_NGVD29,round(GH_Inst_NGVD29,1),pos=4))
    abline(h=0)
    if(j==6){
      axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
      mtext(side=1,line=1.75,"Date (Month-Day)")
      }else{axis_fun(1,xmaj,xmin,NA)}
    axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
    mtext(side=3,adj=0,line=-1,paste0(" ",site.names2[j]),cex=0.75)
    # if(j==4){mtext(side=2,at=3,line=2.25,"Stage Elevation (Ft, NGVD29)")}
    abline(v=MiltonLandFall.utc,col=adjustcolor("pink",0.5),lwd=2)
  }
  mtext(side=2,outer=T,line=-1.25,"Stage Elevation (Ft, NGVD29)")
  
  par(mar=c(0.1,0.2,0.25,0.2))
  plot(st_geometry(GOM_world),ylim=bbox.lims2[c(2,4)],xlim=bbox.lims2[c(1,3)],lwd=0.05,col="ivory",border="grey")
  box(lwd=1)
  plot(tmp.track,add=T,col=adjustcolor("red",0.5),lwd=2)
  plot(tmp.pts,add=T,pch=21,bg=adjustcolor("red",0.5),lwd=0.1)
  st_txt(tmp.pts[nrow(tmp.pts),],tmp.pts[nrow(tmp.pts),]$TY,pos=3,cex=0.75)
  plot(st_geometry(AOI.poly),add=T,border="black",lty=2)
  mtext(side=3,paste(format(step.val,"%d %b %H:00"),"UTC"))
  mtext(side=3,line=-1.25,paste(t_landall,"hrs till landfall"),col="red",font=2,cex=0.8)

  plot(st_geometry(FLCounty),ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.05,col="ivory",border="grey")
  plot(st_geometry(FL.shp),add=T,lwd=0.05,col="ivory",border="grey")
  plot(st_geometry(sites.info.sp),add=T,pch=21,cex=1.5,bg="pink",lwd=0.1)
    st_txt(subset(sites.info.sp,!(site_no%in%c("LOK","02301721","02300042"))),
           site.names[!(sites.info.sp$site_no%in%c("LOK","02301721","02300042"))],halo=T,pos=2,font=2,cex=0.5)
    st_txt(subset(sites.info.sp,site_no=="LOK"),site.names[sites.info.sp$site_no=="LOK"],halo=T,pos=3,font=2,cex=0.5)
    st_txt(subset(sites.info.sp,site_no%in%c("02301721","02300042")),
           site.names[sites.info.sp$site_no%in%c("02301721","02300042")],halo=T,pos=4,font=2,cex=0.5)
    plot(tmp.track,add=T,col=adjustcolor("red",0.5),lwd=2)
  plot(tmp.pts,add=T,pch=21,bg=adjustcolor("red",0.5),lwd=0.1,cex=1.25)
  st_txt(tmp.pts[nrow(tmp.pts),],tmp.pts[nrow(tmp.pts),]$TY,pos=3,cex=0.75)
  box(lwd=1)
  mapmisc::scaleBar(FLCounty,"bottomleft",bty="n",cex=1,outer=F)
 
  par(mar=c(1,3.5,0.25,0.25))
  for(j in 7:12){
    ylim.val = c(min.y[j],max.y[j]);
    by.y=floor((max.y[j]-min.y[j])/2)
    if(by.y==0){by.y=(max.y[j]-min.y[j])/2}
    ymaj=seq(ylim.val[1],ylim.val[2],by.y);
    ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
    dat.site.tmp2 = subset(dat.site.tmp,site_no==site.ls[j])
    
    plot(GH_Inst_NGVD29~dateTime,dat.site.tmp2,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,yaxs="i")
    abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
    lines(GH_Inst_NGVD29~dateTime,dat.site.tmp2,col="red",lwd=2)
    points(GH_Inst_NGVD29~dateTime,subset(dat.site.tmp2,dateTime==max(dat.site.tmp2$dateTime)),bg="grey",pch=21,cex=1.25)
    with(subset(dat.site.tmp2,dateTime==max(dat.site.tmp2$dateTime)),text(dateTime,GH_Inst_NGVD29,round(GH_Inst_NGVD29,1),pos=4))
    abline(h=0)
    if(j==12){
      axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
      mtext(side=1,line=1.75,"Date (Month-Day)")
    }else{axis_fun(1,xmaj,xmin,NA)}
    axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
    mtext(side=3,adj=0,line=-1,paste0(" ",site.names2[j]),cex=0.75)
    # if(j==9){mtext(side=2,line=2.25,"Stage Elevation (Ft, NGVD29)")}
    abline(v=MiltonLandFall.utc,col=adjustcolor("pink",0.5),lwd=2)
  }
  mtext(side=2,outer=T,line=-34.5,"Stage Elevation (Ft, NGVD29)")
  dev.off()
  print(i)
}
  
file.names=c(paste0(plot.path,"/pngs/inst/",format(step.ls,"%Y%m%d_%H"),"_animation.png"),
             paste0(plot.path,"/pngs/inst/",format(step.ls[length(step.ls)],"%Y%m%d_%H"),"_animation.png") # to add an extra frame
)

gifski::gifski(file.names, 
               delay = 15 / 100, 
               gif_file  = paste0(plot.path,"HurricaneMilton_stage_inst_statewide_gifski.gif"),
               loop = T,
               width=6.5*200,
               height=6*200)
# 
# gifski::gifski(file.names, 
#                delay = 15 / 100, 
#                gif_file  = paste0(plot.path,"HurricaneMilton_stage_inst_statewide_gifski_LARGE.gif"),
#                loop = T,
#                width=(6.5*200)*2,
#                height=(5.5*200)*2)


# Alafia Sites ------------------------------------------------------------
alafia.sites = c("02301721","02301500")

alafia.dat=readNWISdata(
  service="iv",
  parameterCd=pCode,
  sites =alafia.sites,
  startDate = format(startDate,"%Y-%m-%d"),
  endDate = format(endDate,"%Y-%m-%d"))|>
  renameNWISColumns()

head(alafia.dat)
alafia.site.info = dataRetrieval::readNWISdata(service = "site",
                            seriesCatalogOutput=TRUE,
                            sites=alafia.sites)



alafia.site.info=unique(alafia.site.info[,c("site_no","station_nm","dec_lat_va","dec_long_va","alt_datum_cd")])
alafia.site.info=alafia.site.info|>
  merge(
    data.frame(site_no = c("02301721","02301500"),
               WL_con = c(-0.272,-0.281)# in meters from vertcon
               ),"site_no",all.x=T)|> 
  merge(data.frame(site_no = c("02301721","02301500"),
                   shrt.nme=c("Gibsonton","Lithia")
                   ),"site_no",all.x=T)

alafia.dat=merge(alafia.dat,alafia.site.info[,c("site_no","WL_con","shrt.nme")],"site_no",all.x=T)
alafia.dat$GH_Inst_NGVD29 = with(alafia.dat,GH_Inst+abs(WL_con))

alafia.dat.xtab = dcast(alafia.dat,dateTime~shrt.nme,value.var="GH_Inst_NGVD29",mean)
ddply(alafia.dat,"site_no",summarise,min.val = min(GH_Inst_NGVD29,na.rm=T),max.val = max(GH_Inst_NGVD29,na.rm=T))

# png(filename=paste0(plot.path,"/alfia_stg.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3.5,0.25,0.4),oma=c(3,0.1,1,0.3))
layout(matrix(1:2,2,1))
xlim.val=date.fun(c("2024-10-04","2024-10-14"),tz="UTC");xmaj=seq(xlim.val[1],xlim.val[2],"3 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")

ylim.val = c(-3,4);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Gibsonton~dateTime,alafia.dat.xtab,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lines(Gibsonton~dateTime,alafia.dat.xtab,col="dodgerblue1",lwd=2)
abline(h=0)
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
with(subset(alafia.site.info,shrt.nme=="Gibsonton"),mtext(side=3,adj=0,line=-1,paste0(" ",shrt.nme," (USGS Site #: ",site_no,")"),cex=0.75))
abline(v=MiltonLandFall.utc,col=adjustcolor("pink",0.75),lwd=2)

ylim.val = c(8,25);by.y=4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(Lithia~dateTime,alafia.dat.xtab,xlim=xlim.val,ylim=ylim.val,type="n",ann=F,axes=F,yaxs="i")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lines(Lithia~dateTime,alafia.dat.xtab,col="dodgerblue1",lwd=2)
abline(h=0)
axis_fun(1,xmaj,xmin,format(xmaj,"%b %d"))
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
with(subset(alafia.site.info,shrt.nme=="Lithia"),mtext(side=3,adj=0,line=-1,paste0(" ",shrt.nme," (USGS Site #: ",site_no,")"),cex=0.75))
abline(v=MiltonLandFall.utc,col=adjustcolor("pink",0.75),lwd=2)
mtext(side=1,line=1.75,"Date (Month-Day)")

mtext(side=2,outer=T,line=-1.25,"Stage Elevation (Ft, NGVD29)")
dev.off()



# LOK Seiche --------------------------------------------------------------
bk.NS.dbks=data.frame(SITE=c("S133","S3"),
                   DBKEY=c("IY368","06636"),
                   type="breakpoint")
LOK_NS = DBHYDRO_daily(as.Date(startDate),as.Date(endDate),bk.NS.dbks$DBKEY)
LOK_NS = merge(LOK_NS,bk.NS.dbks,"DBKEY")

LOK_NS$datetime.utc=date.fun(LOK_NS$DATETIME,tz="UTC",form="%Y-%m-%d %H:%M:%S")
LOK_NS$datetime.hr.utc=date.fun(paste(format(LOK_NS$datetime.utc,"%Y-%m-%d"),
                                        format(LOK_NS$datetime.utc,"%H"),":00:00"),form="%Y-%m-%d %H :%M:%S",tz="UTC")

LOK_NS.xtab = dcast(LOK_NS,datetime.hr.utc~SITE,value.var="Data.Value",max,na.rm=T)
LOK_NS.xtab$N_S_diff = with(LOK_NS.xtab,S133-S3)
max(LOK_NS.xtab$N_S_diff)
summary(LOK_NS.xtab)
subset(LOK_NS.xtab,date.fun(datetime.hr.utc,tz="UTC")%in%date.fun("2024-10-10",tz="UTC"))

# png(filename=paste0(plot.path,"/LOK_stg_seiche.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,3.5,0.25,0.4),oma=c(3,0.1,1,0.3))

ylim.val = c(12,20);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(startDate,endDate);xmaj=seq(xlim.val[1],xlim.val[2],"1 days");xmin=seq(xlim.val[1],xlim.val[2],"3 hours")
plot(S133~datetime.hr.utc,LOK_NS.xtab,xlim=xlim.val,ylim=ylim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
lines(S133~datetime.hr.utc,LOK_NS.xtab,col="dodgerblue1",lwd=1.25,lty=1)
lines(S3~datetime.hr.utc,LOK_NS.xtab,col="indianred1",lwd=1.25)
# lines(GH_Inst_NGVD29~ dateTime,LakeO.xtab2)
axis_fun(1,xmaj,xmin,format(xmaj,"%d"))
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
abline(v=MiltonLandFall.utc,col=adjustcolor("pink",0.5),lwd=2)

text(MiltonLandFall.utc,20,"Milton",font=2)
mtext(side=1,line=2,"Oct 2024 (UTC)")
mtext(side=2,outer=T,line=-1.25,"Stage Elevation (Ft, NGVD29)")
legend("topleft",c("North (S133 TW)","South (S3 TW)"),
       pch=c(NA),lty=c(1,1),lwd=c(1.25),
       col=c("dodgerblue1","indianred1"),pt.bg=c(NA),
       pt.cex=1.5,ncol=1,cex=0.95,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()