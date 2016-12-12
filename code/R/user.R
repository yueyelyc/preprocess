## source functions
library(plotrix)
library(plyr)
files <- system("ls /nas1/cao/bopp/SNet/SNet/R/*.R", intern=T)
for(f in files) try(source(f))
library(RgoogleMaps)
source("~cao/bopp/mobility/May2013/fun.R")
source("~cao/bopp/geoLocation/fun.R")
source("~cao/bopp/mobility/May2013/test1.R")
source("~cao/bopp/mobility/May2013/fun-40day.R")

## add this
load("~cao/bopp/mobility/May2013/data/bs.RData")
load("~cao/bopp/mobility/May2013/data/larry.bs.RData")
load("~cao/bopp/mobility/May2013/homeOffice.RData")
load("~cao/bopp/mobility/homeOffice.new.RData")

## readVerizonData: script to read verizon's raw data

## extract the raw data for those 1000 users, and use these as the test samples for ideas
system("cat ~cao/bopp/packet/writings/infocom2017/data/homedf.sel.removeHomeOffice.uid1000.csv | awk -F',' '{print "^"$1"\t"}' | grep -f - /raid/backup/cao/BellLabs/data/BellLabs5miles.collapsed.dat > ~cao/bopp/packet/writings/infocom2017/data/BellLabs5miles.collapsed.uid1000.csv")
## this is making sure it is from the first column

## raw data for 1000 people
if(F){
  dat.uid1000 <- readVerizonRawData(file="/nas1/cao/bopp/packet/writings/infocom2017/data/BellLabs5miles.collapsed.uid1000.csv",header=F, sep="\t")
}



load("/raid/backup/cao/ELIS/output.RData")
## data model
datamodel.sel.uid1000 <- ldply(loc.profile.lst, function(x){x$datamodel})
names(datamodel.sel.uid1000)[1] <- "uid"
length(unique(datamodel.sel.uid1000$uid))
table(datamodel.sel.uid1000$label)

## test if the naive results make sense
tmp <- tapply(datamodel.sel.uid1000$label, datamodel.sel.uid1000$uid, function(x){
  paste(sort(unique(x)), collapse="/")
})
rev(sort(table(tmp)))
## the 186 people might be wrong since they have both home office and possibleHomeOffice
##                          home/in.transit/UNKNOWN 
##                                             4170 
##                   home/in.transit/office/UNKNOWN 
##                                             3331 
##       home/in.transit/PossibeHomeOffice/UNKNOWN 
##                                             1881 
##home/in.transit/office/PossibleHomeOffice/UNKNOWN 
##                                              186 

## create some rules
tmpdf <- data.frame(uid=names(tmp), rawtype=nullname(c(unlist(tmp))))
typemap <- data.frame(rawtype=sort(unique(tmpdf$rawtype)))
typemap$type <- c("mixed", "fulltime", "parttime", "homemaker", "fulltime", "fulltime")
tmpdf <- merge(tmpdf, typemap)
tmpdf$usergroup <- tmpdf$type
table(tmpdf$type)

## let's see how many people have two primary locations
out <- ddply(datamodel.sel.uid1000, .(uid), function(x){
  id.non.home.transit <- (x$geoPlace!=0 & x$label!="home") ## not home, not transit
  sum(as.integer(x[id,"end"])-as.integer(x[id,"start"]))/sum(as.integer(x$end)-as.integer(x$start))
})


##
zoom <- 10
##loc.pick <- c(mean(homedf[,"lat"]), mean(homedf[,"long"]))
##loc.pick <- c(40.524459,-74.4244339)
pdf("~/projects/nds/output/gmap.pdf")
loc.pick <- c(40.6839901, -74.4012165)
MyMap <- GetMap(center=loc.pick, zoom=zoom)
id <- datamodel.sel.uid1000$label=="home"
PlotOnStaticMap(MyMap, datamodel.sel.uid1000[id,"start.lat"], datamodel.sel.uid1000[id,"start.long"],
                pch=1, col="red", add=F, cex=0.25,
                lwd=2)  # all pass-by bs
dev.off()

zoom <- 9
MyMap <- GetMap(center=loc.pick, zoom=zoom)
id <- datamodel.sel.uid1000$label=="office"
PlotOnStaticMap(MyMap, datamodel.sel.uid1000[id,"start.lat"], datamodel.sel.uid1000[id,"start.long"],
                pch=1, col="red", add=F, cex=0.25,
                lwd=2)  # all pass-by bs

## find out the bounding box for Yanchi to obtain POI.
zoom <- 9
##loc.pick <- c(mean(homedf[,"lat"]), mean(homedf[,"long"]))
##loc.pick <- c(40.524459,-74.4244339)
loc.pick <- c(40.6839901, -74.4012165)
MyMap <- GetMap(center=loc.pick, zoom=zoom)
samp <- sample(1:nrow(datamodel.sel.uid1000), size=10000)
PlotOnStaticMap(MyMap, datamodel.sel.uid1000[samp,"start.lat"], datamodel.sel.uid1000[samp,"start.long"],
                pch=2, col="red", add=F, cex=0.75,
                lwd=2)  # all pass-by bs
PlotOnStaticMap(MyMap, qlat[1], qlon[1], 
                pch=2, col="blue", add=T, cex=5,
                lwd=2)  # all pass-by bs
PlotOnStaticMap(MyMap, qlat[2], qlon[2], 
                pcha=2, col="blue", add=T, cex=5,
                lwd=2)  # all pass-by bs

qlat <- quantile(datamodel.sel.uid1000[,"start.lat"], p=c(0.025, 0.975))
qlon <- quantile(datamodel.sel.uid1000[,"start.long"], p=c(0.025, 0.975))
distKm(qlat[1], qlon[1], qlat[2], qlon[2])

## figure out weekly visits and how that is related to the next frequented place
datamodel.sel.removeHomeOffice.uid1000 <- datamodel.sel.uid1000[datamodel.sel.uid1000$label!="home" & datamodel.sel.uid1000$label!="office" & datamodel.sel.uid1000$label!="in.transit" & datamodel.sel.uid1000$label!="PossibleHomeOffice",]

if(F) d <- merge(datamodel.sel.removeHomeOffice.uid1000, tmpdf[,c("uid","usergroup")])
d <- merge(datamodel.sel.uid1000, tmpdf[,c("uid","usergroup")])
aa <- unique(d[,c("uid","usergroup")])
rev(sort(table(aa[,"usergroup"])))
write.csv(d, file="~/bopp/packet/writings/infocom2017/data/datamodel.sel.uid1000.ugroup.csv", quote=F, row.names=F)
##write.csv(tmpdf[,c("uid","usergroup")], file="~/bopp/packet/writings/infocom2017/data/usergroup.csv", quote=F, row.names=F)

## look at these people high frequency visits and see if we can identify them??
dout <- ddply(d, .(uid, geoPlace), function(x){
  nvisit <- nrow(x)
  nday <- length(unique(x$day))
  data.frame(nvisit=nvisit, nday=nday)
})
mean(dout$nvisit-dout$nday>0) # 6%, so most days have only one visit
mean(dout$nday>=4) ## 12% is more than 4 times
histogram(~log2(dout$nday), nclass=3)
histogram(~sqrt(dout$nday), nclass=3)
histogram(~pmin(dout$nday, 10), nclass=3)

## visit by number of days
tb <- table(dout$nday)
cc <- cumsum(tb*as.numeric(names(tb)))
plot(cc/max(cc))

histogram(~sqrt(dout$nday))
sum(dout$nday[dout$nday>=3])/sum(dout$nday) 

## just focus on locations that are frequentedly visited
gloc.sel <- dout[dout$nday>=4,]
d.sel <- merge(d, gloc.sel)
d.sel <- d.sel[order(d.sel$uid, d.sel$geoPlace, d.sel$wday.type, d.sel$start),]
quantile(d.sel$dur.in.min, p=seq(0,1,0.1))
d.sel.out <- ddply(d.sel, .(uid, geoPlace), function(x){
  nvisit <- nrow(x)
  nday <- length(unique(x$day))
  avg.dur.in.min <- mean(x$dur.in.min)
  data.frame(nvisit=nvisit, nday=nday, avg.dur.in.min=avg.dur.in.min)
})

d.sel.nstream <- ddply(d.sel, .(uid, geoPlace), function(x){
  nstream <- numberUserByTime(x$start, x$end, by="wkday.wkend")
  nstream
})

## for everybody what are the typical routine places
## understand the routine behavior
oo <- ddply(d.sel.nstream, .(uid, geoPlace, floor(time)), function(x, thre=0.6){
  nday <- x$nday[1]
  nstream.max <- max(x$nstream)
  tm <- x$time - floor(x$time)
  high <- x$nstream > thre  ## these are the time when there is consistency in event occuring times
  if(sum(high)>0) { ## if there is consistency in event occurring times
    common.start <- max(tm[high]) * 24  ## common event start time in hours
    common.end <- min(tm[high]) * 24 ## common event end time in hours
    common.dur.in.min <- (common.end-common.start)*60  #in minutes
  } else {
    common.dur.in.min <- NA; common.start <- NA; common.end <- NA
  }
  data.frame(nday=nday, nstream.max, common.dur.in.min=common.dur.in.min, common.start=common.start, common.end=common.end)
})

## how many only occur on weekends or weekdays

mean(oo$nday>=3) ## 64% 
mean(oo$nday>=3 & oo$nstream.max>=0.6)/mean(oo$nday>=3)  ## 40\% such locations has somewhat regular visiting patterns
mean(oo$nday>=3 & oo$nstream.max>=0.8)/mean(oo$nday>=3)  ## 30\% such locations are very regular






##1003223, geoPlace 6
## we definitely need better algorithm for combining places. 
idx <- 2
x <- d.sel[d.sel$uid==gloc.sel$uid[idx] & d.sel$geoPlace==gloc.sel$geoPlace[idx],]
## this is a case where it should be the job location, the same job location was splitted into two geoPlaces

idx <- 4
x <- d.sel[d.sel$uid==gloc.sel$uid[idx] & d.sel$geoPlace==gloc.sel$geoPlace[idx],]

## analyze 1003223 and see if we can get somewhere for this individual
## maybe merge with the cell classification map
area.bsid.type <- read.csv("~/bopp/packet/writings/infocom2017/data/area.bsid.txt")
dm <- merge(dm, area.bsid.type, by.x="start.bsid", by.y="bsid")
dm[dm$geoPlace==6,]
dm[dm$geoPlace==7,]
dm[dm$geoPlace==8,]




## 1004102
