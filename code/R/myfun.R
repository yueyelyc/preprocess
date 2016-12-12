source("~cao/bopp/mobility/May2013/fun.R")
source("~cao/bopp/geoLocation/fun.R")

#####
## util functions
isInNYC <- function(lat, lon){
  ## check if the given location is in NYC
  if (lat < 40.837115 & lat > 40.700311 & lon < -73.917961 & lon > -74.024018)
    TRUE
  else
    FALSE
}

isInBellLabs20by20 <- function (lat, lon){
  ## check if the given location is in Bell Labs 20by20 area
  if (lat < 40.79 & lat > 40.58 & lon < -74.30 & lon > -74.51)
    TRUE
  else
    FALSE
}

## given lat, lon, find the nearest k pois from google_poi
knnPOIs <- function(lat, lon, k=10){
  # if not exist, read the poi dataset
  if(!exists("google_poi")){
    google_poi <<- read.csv(file="~/projects/elis/data/google_poi_bell_labs.csv",  quote = "", sep="\t", header=T, stringsAsFactors = FALSE)
    colnames(google_poi)[which(names(google_poi) == "latitude")] <- "lat"
    colnames(google_poi)[which(names(google_poi) == "longitude")] <- "lon"
    transform(google_poi, lat = as.numeric(lat))
    transform(google_poi, lon = as.numeric(lon))
  }
  library(FNN)
  nind <- knnx.index(google_poi[,c("lat","lon")], t(as.matrix(c(lat, lon))), 5*k)
  ndist <- rep(NA, length(nind))
  for(i in 1:length(nind)){
    ndist[i] <- distKm(lat, lon, google_poi[nind[i],c("lat")], google_poi[nind[i],c("lon")])
  }
  sind <- unlist(as.matrix(sort.int(ndist, index.return=TRUE))[2])
  kind <- nind[sind[1:k]]
  kdist <- ndist[sind[1:k]]
  
  return(list(index=kind, dist=kdist)); 
}
## an example of knnPOIs
knn <- knnPOIs(40.6853, -74.3923, 10)
knn$index
knn$dist
knn.pois <- google_poi[knn$index,]
View(knn.pois)


## given lat, lon, find the arounding pois from google_poi
aroundPOIs <- function(lat, lon){
  # if not exist, read the poi dataset
  if(!exists("google_poi")){
    google_poi <<- read.csv(file="~/projects/elis/data/google_poi_bell_labs.csv",  quote = "", sep="\t", header=T, stringsAsFactors = FALSE)
    colnames(google_poi)[which(names(google_poi) == "latitude")] <- "lat"
    colnames(google_poi)[which(names(google_poi) == "longitude")] <- "lon"
    transform(google_poi, lat = as.numeric(lat))
    transform(google_poi, lon = as.numeric(lon))
  }
  nind <- knnx.index(google_poi[,c("lat","lon")], t(as.matrix(c(lat, lon))), 5*k)
  ndist <- rep(NA, length(nind))
  for(i in 1:length(nind)){
    ndist[i] <- distKm(lat, lon, google_poi[nind[i],c("lat")], google_poi[nind[i],c("lon")])
  }
  sind <- unlist(as.matrix(sort.int(ndist, index.return=TRUE))[2])
  kind <- nind[sind[1:k]]
  kdist <- ndist[sind[1:k]]
  
  return(list(index=kind, dist=kdist)); 
}

#####
## assign nearest cell for POIs

load("~cao/bopp/mobility/May2013/data/bs.RData")
load("~cao/bopp/mobility/May2013/bsinfo-extended-forGlenn.RData")
## calculate the stationary period center location
new.bs.info4$enb.cell.hex <- tolower(new.bs.info4$enb.cell.hex)
my.bs.info <- merge(bs.info, new.bs.info4[,c("enb.cell.hex","elat","elong")], by.x="bsid", by.y="enb.cell.hex")

#poi2 <- read.csv(file="~cao/bopp/mobility/May2013/BellLabs/poi2.csv", header=T)
google_poi <- read.csv(file="~/projects/elis/data/google_poi_bell_labs.csv",  quote = "", sep="\t", header=T, stringsAsFactors = FALSE)
colnames(google_poi)[which(names(google_poi) == "latitude")] <- "lat"
colnames(google_poi)[which(names(google_poi) == "longitude")] <- "lon"
transform(google_poi, lat = as.numeric(lat))
transform(google_poi, lon = as.numeric(lon))
ind_belllabs_20by20 <- which(apply(google_poi, 1, function(x) isInBellLabs20by20(as.numeric(x['lat']), as.numeric(x['lon']))))
poi2 <- google_poi[ind_belllabs_20by20,]

## closest cell for poi
poi2$distKm.closestCell <- rep(NA, nrow(poi2))
poi2$closestCell <- rep(NA, nrow(poi2))
poi2$closestCell.lat <- rep(NA, nrow(poi2))
poi2$closestCell.long <- rep(NA, nrow(poi2))
for(i in 1:nrow(poi2)){
  d <- distKm(poi2$lat[i], poi2$lon[i], my.bs.info$elat, my.bs.info$elong)
  id <- seq(d)[d==min(d)][1]
  poi2[i,"distKm.closestCell"] <- min(d)
  poi2[i,"closestCell"] <- my.bs.info[id,"bsid"]
  poi2[i,"closestCell.lat"] <- my.bs.info[id,"lat"]
  poi2[i,"closestCell.long"] <- my.bs.info[id,"long"]
}
## 90% of these POI are within 1km of the elat/elong
quantile(poi2$distKm.closestCell, p=seq(0,1,0.1), na.rm=T)

#####
## poi category frequency for each cell
for (catename in c("l9","l22","category")){
  rm(poi2.cate)
  rm(poi2.cell.cate)
  poi2.cate <- t(as.data.frame(table(poi2[,catename])))
  rownames(poi2.cate) <- NULL
  colnames(poi2.cate) <- c(poi2.cate[1,])
  poi2.cate <- as.data.frame(t(poi2.cate[2,]))
  poi2.cate[,colnames(poi2.cate)] = apply(poi2.cate[,colnames(poi2.cate)], 2, function(x) as.numeric(as.character(x)))
  poi2.cate$cellid <- c("all")
  cells <- unique(poi2$closestCell)
  for (cell in cells){
    ind <- which(poi2$closestCell == cell)
    poi2.cell <- poi2[ind,]
    poi2.cell.cate <- t(as.data.frame(table(poi2.cell[,catename])))
    rownames(poi2.cell.cate) <- NULL
    colnames(poi2.cell.cate) <- c(poi2.cell.cate[1,])
    poi2.cell.cate <- as.data.frame(t(poi2.cell.cate[2,]))
    if (ncol(poi2.cell.cate) == 1) {
      poi2.cell.cate[,colnames(poi2.cell.cate)] = as.numeric(as.character(poi2.cell.cate[,colnames(poi2.cell.cate)]))
    } else {
      poi2.cell.cate[,colnames(poi2.cell.cate)] = apply(poi2.cell.cate[,colnames(poi2.cell.cate)], 2, function(x) as.numeric(as.character(x)))
    }
    poi2.cell.cate$cellid <- c(cell)
    colnames.missing <- colnames(poi2.cate)[!(colnames(poi2.cate) %in% colnames(poi2.cell.cate))]
    poi2.cell.cate[,colnames.missing] <- rep(0, length(colnames.missing))
    poi2.cate <- rbind(poi2.cate, poi2.cell.cate)
  }
  write.csv(poi2.cate, file=paste0("~/projects/elis/output/cell_cate_frequency_",catename,".csv"), row.names=FALSE)
}

poi2.service <- poi2[which(poi2$l9=="Shopping"),c("place_id","category")]
write.csv(table(poi2.service$category),file="~/projects/elis/output/cell_shopping_frequency.csv", row.names=FALSE)

##### 
## tf-idf calculation for understanding region function
library(tm)
library(slam)
### method 1
### calculate tf-idf for each poi first
### then aggregate pois in a region as the region tf-idf
## build corpus from poi category
for (catename in c("l9","l22","category")){
  corpus = Corpus(VectorSource(poi2[,catename])) # can use different levels of poi taxonomy, e.g., categories, l9, l22
  #inspect(corpus)
  ## creating term matrix with TF-IDF weighting
  tdm <-DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
  mat <- inspect(tdm[1,])
  tf.idf <- as.data.frame(mat)
  tf.idf <- cbind(cellid="", tf.idf)
  tf.idf <- tf.idf[-c(1), ]
  cells <- unique(poi2$closestCell)
  for (cell in cells){
    ind <- which(poi2$closestCell == cell)
    ## sum
    cell_sum <- col_sums(tdm[ind,])
    cell_nnz <- col_sums(tdm[ind,] != 0)
    ## log scale sum
    for (i in 1:length(cell_nnz)){
      if(cell_nnz[i] > 0){
        cell_sum[i] <- cell_sum[i] * log10(cell_nnz[i] + 1) / cell_nnz[i]
      }
    }
    cell.tf.idf <- cbind(cellid=cell, t(as.data.frame(cell_sum)))
    tf.idf <- rbind(tf.idf, cell.tf.idf)
  }
  ## output to file
  rownames(tf.idf) <- NULL
  write.csv(tf.idf, file=paste0("~/projects/elis/output/cell_cate_weight_poi_",catename,".csv"))
}
### method 2
### aggregate pois in a region first
### then calculate the region tf-idf directly
## aggregate pois in a region
for (catename in c("l9","l22","category")){
  cells <- unique(poi2$closestCell)
  cates <- rep("", length(cells))
  for (i in 1:length(cells)){
    ind <- which(poi2$closestCell == cells[i])
    for (j in ind){
      cates[i] <- paste(cates[i], poi2[j, catename]) # can use different levels of poi taxonomy, e.g., categories, l9
    }
  }
  cell.cates <- data.frame(cid=cells, categories=cates)
  ## build corpus from poi category
  corpus = Corpus(VectorSource(cell.cates$categories)) 
  ## creating term matrix with TF-IDF weighting
  tdm <-DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
  mat <- inspect(tdm[1,])
  tf.idf <- as.data.frame(mat)
  tf.idf <- cbind(cellid="", tf.idf)
  tf.idf <- tf.idf[-c(1), ]
  for (cell in cells){
    ind <- which(cell.cates$cid == cell)
    ## sum
    cell_sum <- col_sums(tdm[ind,])
    cell.tf.idf <- cbind(cellid=cell, t(as.data.frame(cell_sum)))
    tf.idf <- rbind(tf.idf, cell.tf.idf)
  }
  rownames(tf.idf) <- NULL
  write.csv(tf.idf, file=paste0("~/projects/elis/output/cell_cate_weight_region_",catename,".csv"))
}

## analysis
tf.idf <- read.csv(file="~/projects/elis/output/cell_cate_weight.csv", header=T, stringsAsFactors = FALSE)
tf.idf.log <- read.csv(file="~/projects/elis/output/cell_l22_cate_weight_log.csv", header=T, stringsAsFactors = FALSE)
View(tf.idf[which(tf.idf$train_station > 0),])
View(tf.idf.log[which(tf.idf.log$train_station > 0),])
cells.trainstation <- unique(tf.idf[which(tf.idf$train_station > 0),])

##### 
# user activity

# histogram
act <- read.csv(file="~cao/bopp/packet/writings/infocom2017/data/datamodel.sel.uid1000.ugroup.csv", header=T, stringsAsFactors = FALSE)
#act <- read.csv(file="~/projects/nds/data/datamodel.sel.removeHomeOffice.uid1000.ugroup.csv", header=T, stringsAsFactors = FALSE)
act$starttime <- strptime(act$start, "%Y-%m-%d %H:%M:%S")
act$endtime <- strptime(act$end, "%Y-%m-%d %H:%M:%S")
max(act$starttime[[3]])
min(act$starttime[[3]])
jpeg("~/projects/nds/output/plots/cell_act_hist/all.jpg") # histogram of all cell activities
res <- hist(act$starttime[[3]], breaks=seq(0,24,by=1))
dev.off()

df.cell_act_hist <- t(as.data.frame(res[2]))
df.cell_act_hist <- cbind(cellid="", df.cell_act_hist)
df.cell_act_hist <- df.cell_act_hist[-c(1), ]
# histogram of each cell activities
for (cell in unique(poi2$closestCell)){
  # plot hist
  jpeg(paste0("~/projects/nds/output/plots/cell_act_hist/",cell,".jpg"))
  ind <- which(act$start.bsid == cell)
  hist(act[ind,]$starttime[[3]], breaks=seq(0,24,by=1))
  dev.off()
  # save hist data
  res <- hist(act[ind,]$starttime[[3]], breaks=seq(0,24,by=1))
  cell_hist_df <- cbind(cellid=cell, t(as.data.frame(res[2])))
  df.cell_act_hist <- rbind(df.cell_act_hist, cell_hist_df)
}
rownames(df.cell_act_hist) <- NULL
# histogram of each cell activities to file
# each row stands for one cell
# each col stands for one time slot
write.csv(df.cell_act_hist, file="~/projects/nds/output/cell_act_hist.csv")


##### 
# clustering on cell act hist
# cos.sim <- function(ix) 
# {
#   A = X[ix[1],]
#   B = X[ix[2],]
#   return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
# }   
# n <- nrow(X) 
# cmb <- expand.grid(i=1:n, j=1:n) 
# C <- matrix(apply(cmb,1,cos.sim),n,n)

cell_cluster <- kmeans(df.cell_act_hist[,11:20], 8, nstart = 20)
cell_cluster$cluster <- as.factor(cell_cluster$cluster)
df.cell_act_hist$cluster <- cell_cluster$cluster


#####
# plot user activities
library(RgoogleMaps)
# users office in nyc
ind_office_nyc <- which(apply(act[which(act$label == "office"),], 1, function(x) isInNYC(as.numeric(x['start.lat']), as.numeric(x['start.long']))))
user.nyc <- unique(act[which(act$label == "office"),][ind_office_nyc,]$uid)
act.user.nyc <- act[which(act$uid %in% user.nyc),]
table(act.user.nyc$uid)
act.user.nyc.nonhomeoffice <- act[which(act$uid %in% user.nyc
                                        & act$label!="home" 
                                        & act$label!="office"),]
ind <- which(act$uid == user                                    # user
             & act$label!="home" & act$label!="office"          # location
             & act$starttime[[3]]>=16 & act$starttime[[3]]<=21) # time
View(act[ind,])
table(act.user.nyc$uid)
table(act.user.nyc.nonhomeoffice$uid)
length(user.nyc)
length(unique(act[,'uid']))

# one plot for each user each weekday
for (user in unique(user.nyc)){
  for (day in unique(act$wday.name)){
    # plot act
    user
    day
    png(paste0("~/projects/nds/output/plots/user_act_day/",user,".",day,".png"),
        width     = 960,
        height    = 960,
        units     = "px")
    ##pdf(paste0("~/projects/nds/output/plots/user_act/",user,".pdf"))
    ind <- which(act$uid == user & act$wday.name==day               # user and wday
                 & act$label!="home" & act$label!="office"          # location
                 & act$starttime[[3]]>=16 & act$starttime[[3]]<=21) # time
    zoom <- 10
    ##loc.pick <- c(mean(homedf[,"lat"]), mean(homedf[,"long"]))
    ##loc.pick <- c(40.524459,-74.4244339)
    loc.pick <- c(40.6839901, -74.4012165)
    MyMap <- GetMap(center=loc.pick, zoom=zoom)
    PlotOnStaticMap(MyMap, 40.489, -74.111,
                    pch=1, col="black", add=F, cex=1,
                    lwd=2)  # circle size for 0.5 hour
    PlotOnStaticMap(MyMap, 40.475, -74.111,
                    FUN=text, labels="0.5",
                    add=T)  # time info
    PlotOnStaticMap(MyMap, 40.489, -74.091,
                    pch=1, col="black", add=T, cex=2,
                    lwd=2)  # circle size for 1 hour
    PlotOnStaticMap(MyMap, 40.475, -74.091,
                    FUN=text, labels="1",
                    add=T)  # time info
    PlotOnStaticMap(MyMap, 40.489, -74.061,
                    pch=1, col="black", add=T, cex=4,
                    lwd=2)  # circle size for 2 hours
    PlotOnStaticMap(MyMap, 40.475, -74.061,
                    FUN=text, labels="2",
                    add=T)  # time info
    PlotOnStaticMap(MyMap, act[which(act$uid == user & act$label=="home"),"start.lat"], 
                    act[which(act$uid == user & act$label=="home"),"start.long"],
                    pch=3, col="dimgray", add=T, cex=0.75,
                    lwd=2)  # all home bs
    PlotOnStaticMap(MyMap, act[which(act$uid == user & act$label=="office"),"start.lat"], 
                    act[which(act$uid == user & act$label=="office"),"start.long"],
                    pch=4, col="dimgray", add=T, cex=0.75,
                    lwd=2)  # all office bs
    
    PlotOnStaticMap(MyMap, act[ind,"start.lat"], 
                    act[ind,"start.long"],
                    pch=1, col="red", add=T, 
                    cex=act[ind,"dur.in.min"]/30, # in half hour
                    lwd=2)  # all pass-by bs
    if (length(ind) > 0){
      PlotOnStaticMap(MyMap, act[ind,"start.lat"], act[ind,"start.long"],
                      FUN=text, labels=strftime(act[ind,"starttime"], "%H:%M"),
                      add=T)  # time info
    }

    dev.off()
  }
}

# one plot for each user
wday.name <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
color <- c("red", "darkorange", "yellow", "green", "cyan", "blue", "purple")
wday.color <- data.frame(wday.name, color, stringsAsFactors=FALSE)
for (user in unique(act.nyc$uid)){
  # plot act
  png(paste0("~/projects/nds/output/plots/user_act/",user,".png"),
      width     = 960,
      height    = 960,
      units     = "px")
  ##pdf(paste0("~/projects/nds/output/plots/user_act/",user,".pdf"))
  zoom <- 10
  loc.pick <- c(40.6839901, -74.4012165)
  MyMap <- GetMap(center=loc.pick, zoom=zoom)  
  
  PlotOnStaticMap(MyMap, 40.489, -74.111,
                  pch=1, col="black", add=F, cex=1,
                  lwd=2)  # circle size for 0.5 hour
  PlotOnStaticMap(MyMap, 40.475, -74.111,
                  FUN=text, labels="0.5",
                  add=T)  # time info
  PlotOnStaticMap(MyMap, 40.489, -74.091,
                  pch=1, col="black", add=T, cex=2,
                  lwd=2)  # circle size for 1 hour
  PlotOnStaticMap(MyMap, 40.475, -74.091,
                  FUN=text, labels="1",
                  add=T)  # time info
  PlotOnStaticMap(MyMap, 40.489, -74.061,
                  pch=1, col="black", add=T, cex=4,
                  lwd=2)  # circle size for 2 hours
  PlotOnStaticMap(MyMap, 40.475, -74.061,
                  FUN=text, labels="2",
                  add=T)  # time info
  PlotOnStaticMap(MyMap, act[which(act$uid == user & act$label=="home"),"start.lat"], 
                  act[which(act$uid == user & act$label=="home"),"start.long"],
                  pch=3, col="dimgray", add=T, cex=0.75,
                  lwd=2)  # all home bs
  PlotOnStaticMap(MyMap, act[which(act$uid == user & act$label=="office"),"start.lat"], 
                  act[which(act$uid == user & act$label=="office"),"start.long"],
                  pch=4, col="dimgray", add=T, cex=0.75,
                  lwd=2)  # all office bs
  
  for (day in unique(act$wday.name)){
    dcol <- wday.color[which(wday.color$wday.name == day), 'color']
    ind <- which(act$uid == user & act$wday.name==day
                 & act$label!="home" & act$label!="office"          # location
                 & act$starttime[[3]]>=16 & act$starttime[[3]]<=21) # time
    PlotOnStaticMap(MyMap, act[ind,"start.lat"], 
                    act[ind,"start.long"],
                    pch=1, col=dcol, add=T, 
                    cex=act[ind,"dur.in.min"]/30, # in half hour
                    lwd=2)  # all pass-by bs
    if (length(ind) > 0){
      PlotOnStaticMap(MyMap, act[ind,"start.lat"], act[ind,"start.long"],
                      FUN=text, labels=strftime(act[ind,"starttime"], "%H"),
                      add=T)  # time info
    }
  }
  
  dev.off()
}


##### 
# user activity
# histogram for uses office in nyc
jpeg("~/projects/nds/output/plots/cell_act_hist_office_nyc/all.jpg") # histogram of all cell activities
res <- hist(act.user.nyc$starttime[[3]], breaks=seq(0,24,by=1))
dev.off()

df.cell_act_hist <- t(as.data.frame(res[2]))
df.cell_act_hist <- cbind(cellid="", df.cell_act_hist)
df.cell_act_hist <- df.cell_act_hist[-c(1), ]
# histogram of each cell activities
for (cell in cells.trainstation$cellid){
  # plot hist
  jpeg(paste0("~/projects/nds/output/plots/cell_act_hist_office_nyc/",cell,".jpg"))
  ind <- which(act.user.nyc$start.bsid == cell)
  hist(act.user.nyc[ind,]$starttime[[3]], breaks=seq(0,24,by=1))
  dev.off()
  # save hist data
  res <- hist(act.user.nyc[ind,]$starttime[[3]], breaks=seq(0,24,by=1))
  cell_hist_df <- cbind(cellid=cell, t(as.data.frame(res[2])))
  df.cell_act_hist <- rbind(df.cell_act_hist, cell_hist_df)
}
rownames(df.cell_act_hist) <- NULL
# histogram of each cell activities to file
# each row stands for one cell
# each col stands for one time slot
write.csv(df.cell_act_hist, file="~/projects/nds/output/cell_act_hist_office_nyc.csv")

# plot cell.trainstation
png(paste0("~/projects/nds/output/plots/cell_train_station_map.png"),
    width     = 960,
    height    = 960,
    units     = "px")
ind <- which(my.bs.info$bsid %in% cells.trainstation$cellid)
zoom <- 11
loc.pick <- c(40.6839901, -74.4012165)
MyMap <- GetMap(center=loc.pick, zoom=zoom)
PlotOnStaticMap(MyMap, my.bs.info[ind,"elat"], my.bs.info[ind,"elong"],
                pch=1, col="red", add=F, cex=2,
                lwd=2)  
PlotOnStaticMap(MyMap, my.bs.info[ind,"elat"], my.bs.info[ind,"elong"],
                FUN=text, labels=my.bs.info[ind,"bsid"],
                add=T)
dev.off()
# plot poi.trainstation
png(paste0("~/projects/nds/output/plots/poi_train_station_map.png"),
    width     = 960,
    height    = 960,
    units     = "px")
zoom <- 11
loc.pick <- c(40.6839901, -74.4012165)
MyMap <- GetMap(center=loc.pick, zoom=zoom)
PlotOnStaticMap(MyMap, poi.trainstation[,"lat"], poi.trainstation[,"lon"],
                pch=1, col="red", add=F, cex=2,
                lwd=2)  
PlotOnStaticMap(MyMap, poi.trainstation[,"lat"], poi.trainstation[,"lon"],
                FUN=text, labels=poi.trainstation[,"name"],
                add=T)
dev.off()


#####
## matching 4sq poi in google poi
library(stringdist)
jaccardDisRef <- function(s1, s2){
  s11 <- strsplit(as.character(tolower(s1)), " ")[[1]]
  s22 <- strsplit(as.character(tolower(s2)), " ")[[1]]
  inter.len <- length(intersect(s11, s22))
  short.len <- min(length(s11), length(s22))
  sim <- as.double(inter.len)/as.double(short.len)
  dis <- 1-sim
  dis
}
## match s1 in s2
matchingPOI <- function(s1, s2, distance.method){
  dist.name.enh<-matrix(NA, ncol = length(s2$name),nrow = length(s1$name))
  for(i in 1:length(s2$name)) {
    for(j in 1:length(s1$name)) { 
      if (distance.method == "jaccard.ref")
        dist.name.enh[j,i]<-jaccardDisRef(tolower(s2[i,]$name),tolower(s1[j,]$name))
      else
        dist.name.enh[j,i]<-stringdist(tolower(s2[i,]$name),tolower(s1[j,]$name),method = distance.method)      
      #dist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
    }  
  }
  dist.matrix<-as.matrix(dist.name.enh)
  min.name.enh<-apply(dist.matrix, 1, base::min)
  match.s1.s2.enh <- NULL
  for(i in 1:nrow(dist.matrix))
  {
    s2.i<-match(min.name.enh[i],dist.matrix[i,])
    s1.i<-i
    match.s1.s2.enh<-rbind(match.s1.s2.enh,
                            data.frame(s1.i=s1.i,
                                      s2.i=s2.i,
                                      s1name=s1[s1.i,]$name,
                                      s2name=s2[s2.i,]$name,
                                      dist=min.name.enh[i],
                                      method=distance.method))
  }
  match.s1.s2.enh
}

poi.4sq <- read.csv(file="~cao/bopp/mobility/May2013/BellLabs/poi2.csv", header=T)
# sample.index <- ceiling(runif(100, 0, nrow(poi.4sq)))
sample.index <- 1:nrow(poi.4sq)
index.matched <- c()
dist.matched <- c()
for (i in sample.index){
  knn.index.matched <- knnPOIs(poi.4sq[i,"lat"], poi.4sq[i,"lon"], 20)$index
  cur.matched <- matchingPOI(poi.4sq[i,], google_poi[knn.index.matched,], 'lv') # 'lv'
  cur.matched.i <- cur.matched$s2.i
  cur.matched.dist <- cur.matched$dist
  cur.matched.index <- knn.index.matched[cur.matched.i]
  index.matched <- c(index.matched, cur.matched.index)
  dist.matched <- c(dist.matched, cur.matched.dist)
}
pois.google.matched <- google_poi[index.matched,c("place_id","name","category")]
pois.4sq.matched <- poi.4sq[sample.index, c("id","name","new.cat")]
pois.google.matched$key <- 1:nrow(pois.google.matched)
pois.4sq.matched$key <- 1:nrow(pois.4sq.matched)
dist.matched <- data.frame(dist=dist.matched, key=1:length(dist.matched))
pois.matched <- merge(pois.4sq.matched, pois.google.matched, by="key")
pois.matched <- merge(pois.matched, dist.matched, by="key")

## pair match again to see if correct
string.matching <- matchingPOI(pois.4sq.matched, pois.google.matched, 'lv')
pois.matched.correct <- pois.matched[which(string.matching$s1.i==string.matching$s2.i),]

## use dist<t as correct
pois.matched.correct <- pois.matched[which(pois.matched$dist <= 4),]


# for (i in 1:nrow(pois.matched.correct)){
#   pois.matched.correct[i,]$dist <- stringdist(tolower(as.character(pois.matched.correct[i,]$name.x)),tolower(as.character(pois.matched.correct[i,]$name.y)),method = 'lv') 
# }
write.csv(pois.matched, file="~/projects/elis/output/pois_matched.csv")
write.csv(pois.matched.correct, file="~/projects/elis/output/pois_matched_correct.csv")

png(paste0("~/projects/elis/output/plots/pois_matched_dist_hist.png"),
    width     = 960,
    height    = 960,
    units     = "px")
#hist(pois.matched$dist)
hist(pois.matched$dist, breaks=seq(0,ceiling(max(pois.matched$dist))+1,by=0.1))
dev.off()

png(paste0("~/projects/elis/output/plots/pois_matched_correct_dist_hist.png"),
    width     = 960,
    height    = 960,
    units     = "px")
hist(pois.matched.correct$dist, breaks=seq(0,50,by=2))
dev.off()

## turn pairs into count of matrix
## a <- 1 2 3
## b <- x y z
## output:
##    x  y  z
## 1  1  0  0
## 2  0  1  0
## 3  0  0  1
countMatrix <- function(a, b){
  a <- as.character(a)
  b <- as.character(b)
  a[is.na(a)] <- "NA"
  b[is.na(b)] <- "NA"
  rowlabs <- unique(a)
  collabs <- unique(b)
  mat=matrix(0,nrow=length(rowlabs),ncol=length(collabs),dimnames=list(rowlabs,collabs))
  for (i in 1:length(a)){
    mat[a[i],b[i]]=mat[a[i],b[i]]+1
  }
  mat
}
cateMatrix.l1.name <- countMatrix(pois.matched.correct$l1.name, pois.matched.correct$category)
write.csv(cateMatrix, file="~/projects/elis/output/pois_matched_cate_matrix_l1.name.csv")
cateMatrix.l2.name <- countMatrix(pois.matched.correct$l2.name, pois.matched.correct$category)
write.csv(cateMatrix, file="~/projects/elis/output/pois_matched_cate_matrix_l2.name.csv")
cateMatrix.new.cat <- countMatrix(pois.matched.correct$new.cat, pois.matched.correct$category)
write.csv(cateMatrix, file="~/projects/elis/output/pois_matched_cate_matrix_new.cat.csv")

library(corrplot)
cateMatrix.l1.name.nor <- apply(cateMatrix.l1.name, 2, function(x)(x)/(sum(x)))
png(paste0("~/projects/elis/output/plots/pois_matched_cate_matrix_l1.name.png"),
    width     = 960,
    height    = 960,
    units     = "px")
corrplot(cateMatrix.l1.name.nor, is.corr=FALSE, method="square")
dev.off()
cateMatrix.l2.name.nor <- apply(cateMatrix.l2.name, 1, function(x)(x)/(sum(x)))
png(paste0("~/projects/elis/output/plots/pois_matched_cate_matrix_l2.name.png"),
    width     = 1960,
    height    = 1960,
    units     = "px")
corrplot(cateMatrix.l2.name.nor, is.corr=FALSE, method="square")
dev.off()
cateMatrix.new.cat.nor <- apply(cateMatrix.new.cat, 2, function(x)(x)/(sum(x)))
png(paste0("~/projects/elis/output/plots/pois_matched_cat_matrix_new.cate.png"),
    width     = 1960,
    height    = 1960,
    units     = "px")
corrplot(cateMatrix.new.cat.nor, is.corr=FALSE, method="square")
dev.off()


## using Lisa's matching result
poi3 <- read.csv(file="~/projects/elis/data/poi3_activitylabel.csv", quote = "", sep="\t", header=T)
match <- read.csv(file="~/projects/elis/output/match.csv",  
                  quote = "", sep="\t", header=F, col.names=c("id","name.x","name.y","place_id"), 
                  stringsAsFactors = FALSE)
match$id <- apply(match, 1, function(x) strsplit(x['id'], " ")[[1]][length(strsplit(x['id'], " ")[[1]])])
poi.match.lisa <- merge(x=match, y=poi3[,c("id","new.cat","l1.name","l2.name")], by="id", all.x=TRUE)
poi.match.lisa <- merge(x=poi.match.lisa, y=google_poi[,c("place_id","category")], by="place_id", all.x=TRUE)

cateMatrix.new.cat.lisa <- countMatrix(poi.match.lisa$new.cat, poi.match.lisa$category)
write.csv(cateMatrix.new.cat.lisa, file="~/projects/elis/output/pois_matched_cate_matrix_new.cat_lisa.csv")
cateMatrix.l1.name.lisa <- countMatrix(poi.match.lisa$l1.name, poi.match.lisa$category)
write.csv(cateMatrix.l1.name.lisa, file="~/projects/elis/output/pois_matched_cate_matrix_l1.name_lisa.csv")
cateMatrix.l2.name.lisa <- countMatrix(poi.match.lisa$l2.name, poi.match.lisa$category)
write.csv(cateMatrix.l2.name.lisa, file="~/projects/elis/output/pois_matched_cate_matrix_l2.name_lisa.csv")

library(corrplot)
cateMatrix.new.cat.nor.lisa <- apply(cateMatrix.new.cat.lisa, 2, function(x)(x)/(sum(x)))
png(paste0("~/projects/elis/output/plots/pois_matched_cat_matrix_new.cate_lisa.png"),
    width     = 1960,
    height    = 1960,
    units     = "px")
corrplot(cateMatrix.new.cat.nor.lisa, is.corr=FALSE, method="square")
dev.off()
cateMatrix.l1.name.nor.lisa <- apply(cateMatrix.l1.name.lisa, 2, function(x)(x)/(sum(x)))
png(paste0("~/projects/elis/output/plots/pois_matched_cat_matrix_l1.name_lisa.png"),
    width     = 1960,
    height    = 1960,
    units     = "px")
corrplot(cateMatrix.l1.name.nor.lisa, is.corr=FALSE, method="square")
dev.off()
cateMatrix.l2.name.nor.lisa <- apply(cateMatrix.l2.name.lisa, 1, function(x)(x)/(sum(x)))
png(paste0("~/projects/elis/output/plots/pois_matched_cat_matrix_l2.name_lisa.png"),
    width     = 1960,
    height    = 1960,
    units     = "px")
corrplot(cateMatrix.l2.name.nor.lisa, is.corr=FALSE, method="square")
dev.off()




#####
# google poi statistics
library(plyr)
google_poi.latlon.freq <- count(google_poi, c("lat", "lon"))
google_poi.latlon.freq.sort <- google_poi.latlon.freq[with(google_poi.latlon.freq, order(-freq)), ]
View(google_poi[which(google_poi$lat == 40.745005 & google_poi$lon == -73.88561),])

google_poi_belllabs.latlon.freq <- count(poi2, c("lat", "lon"))
google_poi_belllabs.latlon.freq.sort <- google_poi_belllabs.latlon.freq[with(google_poi_belllabs.latlon.freq, order(-freq)), ]
View(poi2[which(poi2$lat > 40.68398 & poi2$lat < 40.683985),])
