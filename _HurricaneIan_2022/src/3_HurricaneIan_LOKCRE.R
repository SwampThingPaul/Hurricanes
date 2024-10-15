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

## Paths
wd="C:/Julian_LaCie/_Github/HurricaneIan"

paths=paste0(wd,c("/Plots/LOK_Est/","/Export/","/Data/","/GIS"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]

tz.con=function(datetime,to.tz){
  as.POSIXct(format(datetime,tz=to.tz,usetz=T),tz=to.tz)
}
# Lake Okeechobee ---------------------------------------------------------
dates=date.fun(c("2022-05-01",as.character(Sys.Date())))

CurWY=WY(Sys.Date())
#LORS
# load("LORS.RData")
LORS=read.csv("C:/Julian_LaCie/_Github/CRE_Conditions/report/LORS.csv")
LORS$Year=CurWY-1
LORS$Date2=with(LORS,date.fun(paste(Year,Month,Day,sep="-")))

LORS2=LORS
LORS2$Year=CurWY
LORS2$Date2=with(LORS2,date.fun(paste(Year,Month,Day,sep="-")))

LORS.gaph2Yrs=rbind(LORS,LORS2)
rm(LORS,LORS2)
LORS.gaph2Yrs$Date2=date.fun(LORS.gaph2Yrs$Date2)

#Lake Okeechobee
comp.dbkey=data.frame(DBKEY=c("N3466","06832"),Priority=c("P2","P1"))

stg.da=data.frame()
for(i in 1:nrow(comp.dbkey)){
  tmp=DBHYDRO_daily(date.fun(paste(CurWY-4,"05-01",sep="-")),dates[2],comp.dbkey$DBKEY[i])
  tmp$DBKEY=as.character(comp.dbkey$DBKEY[i])
  stg.da=rbind(stg.da,tmp)
}
stg.da=merge(stg.da,comp.dbkey,"DBKEY")
stg.da$DATE=date.fun(stg.da$Date)

LakeO.xtab=dcast(stg.da,DATE~Priority,value.var="Data.Value",mean)
LakeO.xtab$Mean=with(LakeO.xtab,ifelse(is.na(P1)==T,P2,P1))
tail(LakeO.xtab)

LakeO.xtab=merge(x=LakeO.xtab,y=LORS.gaph2Yrs,by.x="DATE",by.y="Date2",all.x=T)
subset(LakeO.xtab,Mean<WSM)
subset(LakeO.xtab,DATE==date.fun("2022-09-28"))

LakeO.xtab$WY=WY(LakeO.xtab$DATE)
LakeO.xtab$month=as.numeric(format(LakeO.xtab$DATE,"%m"))
LakeO.xtab$day=as.numeric(format(LakeO.xtab$DATE,"%d"))
LakeO.xtab$plot.dat=with(LakeO.xtab,date.fun(ifelse(month>4,paste(CurWY-1,month,day,sep="-"),paste(CurWY,month,day,sep="-"))))
LakeO.xtab$hydro.day=hydro.day(LakeO.xtab$DATE)

YEST=date.fun(dates[2]-lubridate::ddays(1))
land.fall=date.fun("2022-09-28 15:05",form="%F %R",tz="America/New_York")

HighLakeLab.x=as.POSIXct(paste(CurWY-1,8,1,sep="-"))
WSMLab.x=as.POSIXct(paste(CurWY-1,9,1,sep="-"))
BENLab.x=as.POSIXct(paste(CurWY-1,7,15,sep="-"))
BASELab.x=as.POSIXct(paste(CurWY,4,1,sep="-"))
HIGHLab.x=as.POSIXct(paste(CurWY,3,15,sep="-"))
InterLab.x=as.POSIXct(paste(CurWY,3,1,sep="-"))
LowLab.x=as.POSIXct(paste(CurWY,2,15,sep="-"))

lwd.val=1
xlim.vals=as.POSIXct(strptime(c(as.Date(paste(CurWY-1,05,01,sep="-")),as.Date(paste(CurWY,05,01,sep="-"))),"%Y-%m-%d"),tz="EST")#as.POSIXct(strptime(dates,"%Y-%m-%d"),tz="EST")
xmaj=seq(xlim.vals[1],xlim.vals[2],by="2 months");xmin=seq(xlim.vals[1],xlim.vals[2],by="1 months")
ylim.val=c(9,18);by.y=1
ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

# png(filename=paste0(plot.path,"LOK_Stage.png"),width=7,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.5,2,1,1),oma=c(0.5,3,1,1));
layout(matrix(1:2,2,1,byrow=T),heights=c(1,0.25))
plot(High~Date2,LORS.gaph2Yrs,ylim=ylim.val,xlim=xlim.vals,type="n",lwd=2,ylab=NA,xlab=NA,yaxs="i",xaxs="i",xaxt="n",yaxt="n")
abline(h=seq(9,18,1),lwd=1,col="grey",lty=3)
abline(v=seq(xlim.vals[1],xlim.vals[2],by="1 months"),lwd=1,col="grey",lty=3)
with(LORS.gaph2Yrs,lines(High~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(Intermediate~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(Low~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(BaseFlow~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(BeneficialUse~Date2,lwd=2,col="black"))
with(LORS.gaph2Yrs,lines(WSM~Date2,lwd=2,col="grey"))
with(LORS.gaph2Yrs,lines(Inter1ft~Date2,lwd=2,lty=5,col="black"))
with(LORS.gaph2Yrs,lines(LowLow~Date2,lwd=2,lty=5,col="grey"))
with(LORS.gaph2Yrs,lines(LowMid~Date2,lwd=2,lty=5,col="grey"))
text(HighLakeLab.x,17.5,"High Lake Management Band",font=2,cex=0.75)
text(WSMLab.x,9.5,"Water Shortage Management Band",font=2,cex=0.75)
text(BENLab.x,12,"Beneficial Use",font=2,cex=0.75)
text(BASELab.x,13,"Base Flow",font=2,cex=0.75)
text(HIGHLab.x,17,"High",font=2,cex=0.75)
text(InterLab.x,16.25,"Intermediate ",font=2,cex=0.75)
text(LowLab.x,14.5,"Low",font=2,cex=0.75)
with(subset(LakeO.xtab,WY==CurWY-2),lines(plot.dat,Mean,lwd=4,col=adjustcolor("forestgreen",0.4),lty=1))
with(subset(LakeO.xtab,WY==CurWY-1),lines(plot.dat,Mean,lwd=4,col=adjustcolor("blue",0.4),lty=1))
with(LakeO.xtab,lines(DATE,Mean,lwd=4,col="red",lty=1))
if(is.na(subset(LakeO.xtab,DATE==YEST)$Mean)==T|nrow(subset(LakeO.xtab,DATE==YEST))==0){NA}else{
  with(subset(LakeO.xtab,DATE==YEST),points(DATE,Mean,pch=21,bg=adjustcolor("red",0.5),col="red",lwd=0.1,cex=1.25))
  with(subset(LakeO.xtab,DATE==YEST),segments(DATE,Mean,DATE,10.1,lty=2))
  with(subset(LakeO.xtab,DATE==YEST),text(DATE,10,paste(format(round(Mean,2),nsmall=2),"Ft"),cex=0.8,xpd=NA))}

abline(v=land.fall,col="red",lwd=2,lty=2)
# text(land.fall,ylim.val[2]-0.25,"Landfall",pos=4,cex=0.8,font=3)
f1=paste0(wd,"/BW_hurricaneIcon.png")
img=png::readPNG(f1)
x.off=0.25
rasterImage(img,
            ytop=(ylim.val[2]+0.25)-x.off,
            ybottom=(ylim.val[2]+0.25)+x.off,
            xleft=date.fun(land.fall-lubridate::ddays(8)),
            xright=date.fun(land.fall+lubridate::ddays(8)),xpd=NA)
# aplpack::puticon(x=land.fall, ylim.val[2]+0.1, icon = f1, icon.cex = 40, color = NA)
axis_fun(1,xmaj,xmin,format(xmaj,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=lwd.val)
mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation (Feet, NGVD29)",line=2.5,cex=1.25)
mtext(side=3,adj=0,"Lake Okeechobee")
plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend(0.5,0.5,
       legend=c(paste0("WY",CurWY),paste0("WY",CurWY-1),paste0("WY",CurWY-2)),
       col=c("red",adjustcolor(c("blue","forestgreen"),0.4)),lty=c(1,1),lwd=c(4,4,4),ncol=3,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)
dev.off()


# LOK seiche --------------------------------------------------------------
# Irma
dates=date.fun(c("2017-09-01","2017-09-20"))
dat.irma=DBHYDRO_breakpoint(dates[1],dates[2],c("IY368","06636"))

dates=date.fun(c("2022-09-21","2022-10-03"))
dat.ian=DBHYDRO_breakpoint(dates[1],dates[2],c("IY368","06636"))

dat.ian$DATETIME=date.fun(dat.ian$DATETIME,form="%F %T")
dat.ian.xtab=dcast(dat.ian,DATETIME~Station,value.var = "Data.Value",mean,na.rm=T)
colnames(dat.ian.xtab)=c("DATETIME","S133_T","S3_T")

min(dat.ian.xtab$S3_T,na.rm=T)
max(dat.ian.xtab$S133_T,na.rm=T)

# png(filename=paste0(plot.path,"LOK_seiche_WL.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.5),oma=c(2,2.5,0.75,0.25));

ylim.val=c(9,20);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2022-09-27","2022-10-01"));xmaj=seq(xlim.val[1],xlim.val[2],"1 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")
plot(Data.Value~DATETIME,dat.ian,xlim=xlim.val,ylim=ylim.val,type="n",axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.75)
lines(Data.Value~DATETIME,subset(dat.ian,Station=="S133-T"),col="dodgerblue1",lwd=1.25,lty=2)
lines(Data.Value~DATETIME,subset(dat.ian,Station=="S3-T"),col="indianred1",lwd=1.25)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)

abline(v=date.fun("2022-09-28 15:05",form="%F %R"),col="red",lwd=2,lty=2)
# text(land.fall,ylim.val[2]-0.25,"Landfall",pos=4,cex=0.8,font=3)
x.off=0.4
rasterImage(img,
            ytop=(ylim.val[2]+1)-x.off,
            ybottom=(ylim.val[2]+1)+x.off,
            xleft=date.fun(land.fall-lubridate::duration(3,"hours"),form="%F %R"),
            xright=date.fun(land.fall+lubridate::duration(3,"hours"),form="%F %R"),xpd=NA)
mtext(side=2,line=2,"Water Level (Ft, NAVD29)")
mtext(side=1,line=1.5,"Date (M-D-2022)")
legend("topleft",c("North","South"),
       pch=c(NA),lty=c(2,1),lwd=c(1.25),
       col=c("dodgerblue1","indianred1"),pt.bg=c(NA),
       pt.cex=1.5,ncol=1,cex=0.95,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
mtext(side=3,adj=0,"Lake Okeechobee")
dev.off()

# Shalmiar ----------------------------------------------------------------
logo=png::readPNG("C:/Julian_LaCie/_GitHub/hyd_seaturtle/horiz_SCCF_Logo.png")

dat=read.xlsx("C:/Julian_LaCie/_GitHub/hyd_seaturtle/data/Shalmiar_WLL_Final.xlsx")
colnames(dat)=c("rownum","datetime","abs_press","temp","depth.m","datetime2","depth.ft")
dat$datetime=date.fun(convertToDateTime(dat$datetime),form="%F %R",tz="America/New_York")

with(subset(dat,depth.ft>4),diff(range(datetime)))

# recon.WL$depth.NAVD88.ft=m.to.ft(recon.WL$depth)
dat$depth.NAVD88.ft=dat$depth.ft+0.41 # (MSL to MAVD88 conversion based on Ft Myers Tide Chart)
dat$depth.NAVD29.ft=dat$depth.NAVD88.ft-(-1.18); #(based on difference between elevation in GOM NAVD88 = 1.65 and NGVD29 = 2.83; 1.65-2.83)

pre.storm.period=seq(date.fun("2022-09-24"),date.fun("2022-09-27"),"1 days")

max(subset(dat,date.fun(datetime)==date.fun("2022-09-28"))$depth.NAVD29.ft,na.rm=T)-min(subset(dat,date.fun(datetime)%in%pre.storm.period)$depth.NAVD29.ft,na.rm=T)

# png(filename=paste0(plot.path,"Shalmiar_WL.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.5),oma=c(2,2.5,0.75,0.25));

bk.val=c(-1,2.5,4,6,8,10,12)
bks=findInterval(dat$depth.ft,bk.val)
cols=viridisLite::rocket(length(bk.val),alpha=0.5)# viridisLite::viridis(length(bk.val),option="E")

land.fall=date.fun("2022-09-28 15:05",form="%F %R",tz="America/New_York")
ylim.val=c(0,14);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2022-09-22","2022-10-02"),tz="America/New_York");xmaj=seq(xlim.val[1],xlim.val[2],"2 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")

plot(depth.NAVD29.ft~datetime,dat,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lines(depth.NAVD29.ft~datetime,dat,col="grey",lwd=2)
points(depth.NAVD29.ft~datetime,dat,pch=21,bg=cols[bks],col=cols[bks])
abline(v=land.fall,col="red",lty=2)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
box(lwd=1)

# text(land.fall,max(dat$depth.NAVD29.ft,na.rm=T),"Landfall",pos=4,cex=0.8,font=3)
x.off=0.5
rasterImage(img,
            ytop=(ylim.val[2]+1.25)-x.off,
            ybottom=(ylim.val[2]+1.25)+x.off,
            xleft=date.fun(land.fall-lubridate::duration(6,"hours"),form="%F %R",tz="America/New_York"),
            xright=date.fun(land.fall+lubridate::duration(6,"hours"),form="%F %R",tz="America/New_York"),xpd=NA)

mtext(side=2,line=2,"Water Level (Ft, NAVD29)")
mtext(side=1,line=1.5,"Date (M-D-2022)")
mtext(side=3,adj=0,"Site: Shalimar Beach")
grid::grid.raster(logo,x=0.13,y=0.8,just=c("left","bottom"),width=grid::unit(1.75,"inches"))
dev.off()


## S79 Water levels
library(dataRetrieval)
pCode="00065";# gage height
startDate=date.fun("2022-09-21")
endDate=date.fun("2022-10-03")

S79wl=readNWISdata(
  service="iv",
  parameterCd=pCode,
  sites = "02292900",
  startDate = format(startDate,"%Y-%m-%d"),
  endDate = format(endDate,"%Y-%m-%d"))
S79wl=renameNWISColumns(S79wl)
S79wl=data.frame(S79wl)
attributes(S79wl$dateTime)

S79wl$dateTime=tz.con(S79wl$dateTime,"America/New_York")
head(S79wl)

plot(USACE.DWS_GH_Inst~dateTime,S79wl)

max(subset(S79wl,date.fun(dateTime)==date.fun("2022-09-28"))$USACE.DWS_GH_Inst,na.rm=T)
subset(S79wl,dateTime==date.fun("2022-09-28 15:00",form="%F %R",tz="America/New_York"))$USACE.DWS_GH_Inst

# png(filename=paste0(plot.path,"S79_WL.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.5),oma=c(2,2.5,0.75,0.25));

bk.val=c(-1,2.5,4,6,8,10,12)
bks=findInterval(S79wl$USACE.DWS_GH_Inst,bk.val)
cols=viridisLite::rocket(length(bk.val),alpha=0.5)# viridisLite::viridis(length(bk.val),option="E")

land.fall=date.fun("2022-09-28 15:05",form="%F %R",tz="America/New_York")
ylim.val=c(0,14);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2022-09-22","2022-10-02"),tz="America/New_York");xmaj=seq(xlim.val[1],xlim.val[2],"2 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")

plot(USACE.DWS_GH_Inst~dateTime,S79wl,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lines(USACE.DWS_GH_Inst~dateTime,S79wl,col="grey",lwd=2)
points(USACE.DWS_GH_Inst~dateTime,S79wl,pch=21,bg=cols[bks],col=cols[bks])
abline(v=land.fall,col="red",lty=2)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
box(lwd=1)

# text(land.fall,max(dat$depth.NAVD29.ft,na.rm=T),"Landfall",pos=4,cex=0.8,font=3)
x.off=0.5
rasterImage(img,
            ytop=(ylim.val[2]+1.25)-x.off,
            ybottom=(ylim.val[2]+1.25)+x.off,
            xleft=date.fun(land.fall-lubridate::duration(6,"hours"),form="%F %R",tz="America/New_York"),
            xright=date.fun(land.fall+lubridate::duration(6,"hours"),form="%F %R",tz="America/New_York"),xpd=NA)

mtext(side=2,line=2,"Water Level (Ft, NAVD29)")
mtext(side=1,line=1.5,"Date (M-D-2022)")
mtext(side=3,adj=0,"Site: S-79 (Franklin Lock - Tailwater)")
dev.off()


# png(filename=paste0(plot.path,"ShalimarS79_WL.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,1.5,0.5,0.5),oma=c(2,2.5,1,0.25));
layout(matrix(1:2,2,1))

bk.val=c(-1,2.5,4,6,8,10,12)
bks=findInterval(dat$depth.ft,bk.val)
cols=viridisLite::rocket(length(bk.val),alpha=0.5)# viridisLite::viridis(length(bk.val),option="E")

land.fall=date.fun("2022-09-28 15:05",form="%F %R",tz="America/New_York")
ylim.val=c(0,14);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c("2022-09-22","2022-10-02"),tz="America/New_York");xmaj=seq(xlim.val[1],xlim.val[2],"2 days");xmin=seq(xlim.val[1],xlim.val[2],"1 days")

plot(depth.NAVD29.ft~datetime,dat,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lines(depth.NAVD29.ft~datetime,dat,col="grey",lwd=2)
points(depth.NAVD29.ft~datetime,dat,pch=21,bg=cols[bks],col=cols[bks])
abline(v=land.fall,col="red",lty=2)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
box(lwd=1)

# text(land.fall,max(dat$depth.NAVD29.ft,na.rm=T),"Landfall",pos=4,cex=0.8,font=3)
x.off=0.75
rasterImage(img,
            ytop=(ylim.val[2]+1.25)-x.off,
            ybottom=(ylim.val[2]+1.25)+x.off,
            xleft=date.fun(land.fall-lubridate::duration(6,"hours"),form="%F %R",tz="America/New_York"),
            xright=date.fun(land.fall+lubridate::duration(6,"hours"),form="%F %R",tz="America/New_York"),xpd=NA)

mtext(side=3,adj=0,line=-1.25," Shalimar Beach (SCCF)")

##
bk.val=c(-1,2.4,4,6,8,10,12)
bks=findInterval(S79wl$USACE.DWS_GH_Inst,bk.val)
cols=viridisLite::rocket(length(bk.val),alpha=0.5)# viridisLite::viridis(length(bk.val),option="E")

plot(USACE.DWS_GH_Inst~dateTime,S79wl,xlim=xlim.val,ylim=ylim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
lines(USACE.DWS_GH_Inst~dateTime,S79wl,col="grey",lwd=2)
points(USACE.DWS_GH_Inst~dateTime,S79wl,pch=21,bg=cols[bks],col=cols[bks])
abline(v=land.fall,col="red",lty=2)
axis_fun(2,ymaj,ymin,ymaj)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d"),line=-0.5)
box(lwd=1)

mtext(side=1,line=1.5,"Date (M-D-2022)")
mtext(side=3,adj=0,line=-1.25," S-79 (Franklin Lock - Tailwater; USACE)")

mtext(side=2,outer=T,line=1,"Water Level (Ft, NAVD29)")
dev.off()


# Cal ---------------------------------------------------------------------
library(RcppRoll)
dates=date.fun(c("2022-05-01",as.character(Sys.Date())))

cal.dbkeys=data.frame(DBKEY=c("DJ237","00865"),priority=c("P1","P2"))
cal.q=DBHYDRO_daily(dates[1],dates[2],cal.dbkeys$DBKEY)
cal.q$Date=date.fun(cal.q$Date)
cal.q=merge(cal.q,cal.dbkeys,"DBKEY")
cal.q=dcast(cal.q,Date~priority,value.var="Data.Value",mean)
cal.q$Data.Value=with(cal.q,ifelse(is.na(P1)==T,P2,P1))
cal.q$Q.30=with(cal.q,roll_meanr(Data.Value,n=30))

plot(Data.Value~Date,cal.q)

cal.flow=data.frame(DBKEY=c("DJ235","88280","DJ237","00865"),SITE=c(rep("S77",2),rep("S79",2)),priority=rep(c("P1","P2"),2))
cal.flow.dat=data.frame()
for(i in 1:nrow(cal.flow)){
  tmp=DBHYDRO_daily(dates[1],dates[2],cal.flow$DBKEY[i])
  tmp$DBKEY=as.character(cal.flow$DBKEY[i])
  cal.flow.dat=rbind(tmp,cal.flow.dat)
}
cal.flow.dat$Date=date.fun(cal.flow.dat$Date)
cal.flow.dat$WY=WY(cal.flow.dat$Date)
cal.flow.dat=merge(cal.flow.dat,cal.flow,"DBKEY")
cal.flow.dat.xtab=dcast(cal.flow.dat,SITE+Date+WY~priority,value.var="Data.Value",mean)
cal.flow.dat.xtab$P1=with(cal.flow.dat.xtab,ifelse(SITE=="S77"&P1>=7587,NA,P1)); #Extreme value reported for 6/7/2021
cal.flow.dat.xtab$Data.Value=with(cal.flow.dat.xtab,ifelse(is.na(P1)==T,P2,P1))

cal.flow.dat.xtab=dcast(cal.flow.dat.xtab,Date+WY~SITE,value.var="Data.Value",mean)
cal.flow.dat.xtab$hydro.day=hydro.day(cal.flow.dat.xtab$Date)
cal.flow.dat.xtab$S77=with(cal.flow.dat.xtab,ifelse(S77<0,0,S77))
cal.flow.dat.xtab$C43=with(cal.flow.dat.xtab,ifelse(S79<S77,0,S79-S77))

cal.flow.dat.xtab$cum.S79=with(cal.flow.dat.xtab,ave(cfs.to.acftd(S79),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
cal.flow.dat.xtab$cum.S77=with(cal.flow.dat.xtab,ave(cfs.to.acftd(S77),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
cal.flow.dat.xtab$Q.14=with(cal.flow.dat.xtab,roll_meanr(S79,n=14))
cal.flow.dat.xtab$Q.30=with(cal.flow.dat.xtab,roll_meanr(S79,n=30))
cal.flow.dat.xtab$SalEnv.cat=with(cal.flow.dat.xtab,ifelse(Q.14<750,"low",
                                                           ifelse(Q.14>=750&Q.14<2100,"optimum",
                                                                  ifelse(Q.14>=2100&Q.14<2600,"stress",
                                                                         ifelse(Q.14>2600,"damaging",NA)))))

consec.startend=function(var){
  runs=rle(var)
  myruns = which(runs$values == TRUE)
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  rslt=list(starts=starts,ends=ends)
  return(rslt)
}
tmp=rle(cal.flow.dat.xtab$SalEnv.cat)
tmp$lengths
tmp$values

S79dat2=cal.flow.dat.xtab
S79dat2$Low=with(S79dat2,ifelse(Q.14<750,1,0))
S79dat2$Opt=with(S79dat2,ifelse(Q.14>=750&Q.14<2100,1,0))
S79dat2$Stress=with(S79dat2,ifelse(Q.14>=2100&Q.14<2600,1,0))
S79dat2$Dam=with(S79dat2,ifelse(Q.14>=2600,1,0))
S79dat2$Date=date.fun(S79dat2$Date)


S79dat2$Low.consec=0
for(i in 2:nrow(S79dat2)){
  S79dat2$Low.consec[i]=with(S79dat2,ifelse(Low[i-1]==0&Low[i]>0,1,
                                            ifelse(Low[i-1]>0&Low[i]>0,1,0)))
}
Low.consec.val=consec.startend(S79dat2$Low.consec)
S79dat2$sum.Low.consec=0
if(length(Low.consec.val$ends)!=0){
  for(i in 1:length(Low.consec.val$ends)){
    S79dat2[Low.consec.val$ends[i],]$sum.Low.consec=with(S79dat2[c(Low.consec.val$starts[i]:Low.consec.val$ends[i]),],sum(Low.consec,na.rm=T))
  }
}
#
S79dat2$Opt.consec=0
for(i in 2:nrow(S79dat2)){
  S79dat2$Opt.consec[i]=with(S79dat2,ifelse(Opt[i-1]==0&Opt[i]>0,1,
                                            ifelse(Opt[i-1]>0&Opt[i]>0,1,0)))
}
Opt.consec.val=consec.startend(S79dat2$Opt.consec)
#
S79dat2$sum.Opt.consec=0
if(length(Opt.consec.val$ends)!=0){
  for(i in 1:length(Opt.consec.val$ends)){
    S79dat2[Opt.consec.val$ends[i],]$sum.Opt.consec=with(S79dat2[c(Opt.consec.val$starts[i]:Opt.consec.val$ends[i]),],sum(Opt.consec,na.rm=T))
  }
}
# 
S79dat2$Stress.consec=0
for(i in 2:nrow(S79dat2)){
  S79dat2$Stress.consec[i]=with(S79dat2,ifelse(Stress[i-1]==0&Stress[i]>0,1,
                                               ifelse(Stress[i-1]>0&Stress[i]>0,1,0)))
}
Stress.consec.val=consec.startend(S79dat2$Stress.consec)
S79dat2$sum.Stress.consec=0
if(length(Stress.consec.val$ends)!=0){
  for(i in 1:length(Stress.consec.val$ends)){
    S79dat2[Stress.consec.val$ends[i],]$sum.Stress.consec=with(S79dat2[c(Stress.consec.val$starts[i]:Stress.consec.val$ends[i]),],sum(Stress.consec,na.rm=T))
  }
}
#
S79dat2$Dam.consec=0
for(i in 2:nrow(S79dat2)){
  S79dat2$Dam.consec[i]=with(S79dat2,ifelse(Dam[i-1]==0&Dam[i]>0,1,
                                            ifelse(Dam[i-1]>0&Dam[i]>0,1,0)))
}
Dam.consec.val=consec.startend(S79dat2$Dam.consec)
S79dat2$sum.Dam.consec=0
if(length(Dam.consec.val$ends)!=0){
  for(i in 1:length(Dam.consec.val$ends)){
    S79dat2[Dam.consec.val$ends[i],]$sum.Dam.consec=with(S79dat2[c(Dam.consec.val$starts[i]:Dam.consec.val$ends[i]),],sum(Dam.consec,na.rm=T))
  }
}
# S79dat2=subset(S79dat2,is.na(Q.14)==F)
salenv.vals=subset(S79dat2,Date==max(S79dat2$Date))


diff(date.fun(c("2022-09-07","2022-09-28")))
diff(date.fun(c("2022-09-29","2022-10-20")))

land.fall=date.fun("2022-09-28 15:05",form="%F %R")
# png(filename=paste0(plot.path,"S79Q.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
ylim.val=c(0,25000);by.y=5000;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=c(dates[1],date.fun("2023-04-30"));xmaj=seq(xlim.val[1],xlim.val[2],by="3 month");xmin=seq(xlim.val[1],xlim.val[2],by="1 months")

layout(matrix(1:2,2,1,byrow=F),widths=c(2,1),heights=c(1,0.5))
par(family="serif",mar=c(2,2,1,0.5),oma=c(0.5,3,1,1));
plot(S79~Date,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
# xx=c(xlim.val[1],xlim.val[1],xlim.val[2],xlim.val[2])
# yy3=c(2600,ylim.max,ylim.max,2600);polygon(x=xx,y=yy3,col=adjustcolor("red",0.5))
# yy4=c(2100,2600,2600,2100);polygon(x=xx,y=yy4,col=adjustcolor("yellow",0.5))
# yy5=c(750,2100,2100,750);polygon(x=xx,y=yy5,col=adjustcolor("green",0.5))
abline(h=ymaj,v=xmaj,lty=3,col="grey")

bk.val=c(0,750,2100,2600,65000)
bks=findInterval(cal.flow.dat.xtab$S79,bk.val)
cols=c("white",adjustcolor(c("green","yellow","red"),0.5))
# with(cal.flow.dat.xtab,pt_line(Date,S79,2,"black",1,21,"grey",cex=1,pt.lwd=0.1))
with(cal.flow.dat.xtab,pt_line(Date,S79,2,"black",1,21,cols[bks],cex=1,pt.lwd=0.1))
with(cal.flow.dat.xtab,lines(Date,Q.14,lwd=3,col=adjustcolor("dodgerblue3",0.5)))
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%d-%y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
abline(v=land.fall,col="red",lty=2)

x.off=800
rasterImage(img,
            ytop=(ylim.val[2]+800)-x.off,
            ybottom=(ylim.val[2]+800)+x.off,
            xleft=date.fun(land.fall-lubridate::ddays(8)),
            xright=date.fun(land.fall+lubridate::ddays(8)),xpd=NA)


mtext("Date (M-D-Y)",side=1,line=2,cex=1)
mtext("Discharge (cfs)",side=2,line=3.5,cex=1.25)
mtext(side=3,adj=0,"S-79 (Franklin Lock)")

par(mar=c(2,2,0.5,0.5));
plot(0:1,0:1,type="n",yaxt="n",xaxt="n",bty="n")
legend(0.25,0.75,
       legend=c( "Damaging (>2600 cfs)","Stress (2100 - 2600 cfs)","Optimum (750 - 2100 cfs)"),pch=22,
       pt.bg=adjustcolor(c("red","yellow","green"),0.5),pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,title.adj = 0,title='Salinity Envelope')

legend(0.75,0.75,
       legend=c("Daily Discharge","14-Day moving average"),
       pch=c(21,NA),pt.bg=c("grey",NA),pt.cex=c(1.5,NA),lty=c(NA,1),lwd=c(0.5,2),col=c("black",adjustcolor(c("dodgerblue1"),0.5)),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,title.adj = 0,title='')
dev.off()