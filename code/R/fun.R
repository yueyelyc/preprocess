
addECRSSI <- function(ho, rssi){
  ## find matches using CPU_Time, and location
  ## RSSI is the total power at the location
  ## browser()
  key1 <- paste(round(ho$CPU_Time), ho$X, ho$Y, sep=":")
  key2 <- paste(round(rssi$CPU_Time), rssi$X, rssi$Y, sep=":")
  mat <- match(key1, key2)
  ## remove unmatched data
  ho <- ho[!is.na(mat),]
  cat(round(100*mean(is.na(mat))), "% of the data is unmatched\n")
  key1 <- key1[!is.na(mat)]
  key2 <- key2[!is.na(mat)]
  mat <- mat[!is.na(mat)]

  ## add RSSI and Ec
  ho$RSSI <- rssi[mat, "RSSI_dBm"]
  ho$Ec <- ho$Active_Pilots__Ec_Io__dB + ho$RSSI
  ho
}

localxy2latlon <- function(gpslat1, gpslon1, x, y){
  ##localxy2latlon converts local (x,y) cordinator in meter 
  ##to latitude, longitude in degree
  ##with x pointing east and y pointing north
  ##by Susan Sanders Sept 28, 2010
  ## gpslat1, gpslon1: the gps for 1st cordinate
  ## x, y, local corrdinate in meters respect to the 1st point

  ## gpslat, gpslon: lat, long in degree
  ## convert degree into radius
  lat1 = gpslat1 * pi/180;  
  lon1 = gpslon1 * pi/180;

  ##Calculate the Earth's radius at geodetic latitude,
  aearth = 6378135;   # The Earth's equatorial radius, or semi-major axis, is the distance from its center to the equator (m)
  bearth = 6356750;   # The Earth's polar radius, or semi-minor axis, is the distance from its center to the North and South Poles (m)
  Rearth=sqrt( ((aearth^2 * cos(lat1))^2 + (bearth^2 * sin(lat1))^2 )/ ((aearth * cos(lat1))^2 + (bearth * sin(lat1))^2) );

  ## x, y,  local x,y coordinates respect to point 1: (gpslat1,gpslon1)
  lat2=y/Rearth +lat1;                #y = (lat2-lat1)*Rearth;
  if(cos(lat2)!=0){
    lon2=x/Rearth/cos(lat2)+lon1;   #x = (lon2-lon1)*Rearth*cos(lat2);
  } else lon2=lon1;

  gpslat2=lat2*180/pi;
  gpslon2=lon2*180/pi;

  cbind(gpslat2, gpslon2)
}

latlon2localxy <- function(gpslat1,gpslon1,gpslat2,gpslon2){
  ## latlon2localxy converts latitude, longitude in degree
  ## to local (x,y) cordinator in meter with respect to point 1,
  ## with x pointing east and y pointing north
  ## by Susan Sanders Sept 28, 2010, adapted by Jin Cao, Mar 18, 2011

  ## (gpslat1, gpslon1) is the reference point
  ## (gpslat2, gpslon2) can be vectors

  ## gpslat, gpslon: lat, long in degree
  ## convert degree into radius
  lat1 = gpslat1 * pi/180;  
  lon1 = gpslon1 * pi/180;
  lat2 = gpslat2 * pi/180;
  lon2 = gpslon2 * pi/180;

  ##Calculate the Earth's radius at geodetic latitude,
  aearth = 6378135;   ## The Earth's equatorial radius, or semi-major axis, is the distance from its center to the equator (m)
  bearth = 6356750;   ## The Earth's polar radius, or semi-minor axis, is the distance from its center to the North and South Poles (m)

  Rearth=sqrt( ((aearth^2 * cos(lat1))^2 + (bearth^2 * sin(lat1))^2 )/ ((aearth * cos(lat1))^2 + (bearth * sin(lat1))^2) );

  x = (lon2-lon1)*Rearth*cos(lat2);
  y = (lat2-lat1)*Rearth;

  cbind(x, y)
}

sinc <- function(x){
  x <- pi*x/180
  sin(x)/x
}

plotLayout <- function(dat, psfile=NULL, ntopcell, showcell=T, xlim, ylim, expand=1){

  if(!is.null(psfile)){
    trellis.device(postscript, file=psfile, col=T, horizontal=F)
  }

  ##browser()
  if(missing(xlim)) xlim <- range(pretty(c(dat$x, dat$cellx)))
  mid.xlim <- mean(xlim)
  xlim <- c((xlim[1]-mid.xlim)* expand + mid.xlim,
            (xlim[2]-mid.xlim) * expand + mid.xlim)
  if(missing(ylim)) ylim <- range(pretty(c(dat$y, dat$celly)))
  mid.ylim <- mean(ylim)
  ylim <- c((ylim[1]-mid.ylim)* expand + mid.ylim,
            (ylim[2]-mid.ylim)* expand + mid.ylim)
  if(F){  ## sometimes the same location will see different cells at different times
    uloc <- !duplicated(paste(dat$x, dat$y, sep="/"))
    dat <- dat[uloc,]
  }
  plot(dat$x, dat$y, pch=".", xlab="Local coordinate x (meters)",
       ylab="local coordinate y (meters)", xlim=xlim, ylim=ylim)
  if(F){
    plot(dat$x, dat$y, pch=".", xlab="Local coordinate x (meters)",
         cex=5, 
         ylab="local coordinate y (meters)", xlim=xlim, ylim=ylim)
  }

  if(showcell){
    uid <- !duplicated(dat$cellid)
    points(dat$cellx[uid], dat$celly[uid], pch="*", cex=1.5, col=4)

    ##browser()
    tab <- rev(sort(table(dat$cellid)))
    cat("unique number of cells = ", length(tab), "\n")
    topcell <- names(tab)[1:min(c(ntopcell, length(tab)))]
    matid <- match(topcell, dat$cellid)
    points(dat$cellx[matid], dat$celly[matid], pch="*", cex=2, col=2)
  }

  
  if(!is.null(psfile)) dev.off()
}

showcellFromCelldf <- function(celldf, pch, col){
  points(celldf$x, celldf$y, cex=2, pch=pch, col=col)
}

showcell <- function(dat, ntopcell, arrow=F){
  uid <- !duplicated(dat$cellid)
  points(dat$cellx[uid], dat$celly[uid], pch="*", cex=1.5, col=4)

  ##browser()
  tab <- rev(sort(table(dat$cellid)))
  cat("unique number of cells = ", length(tab), "\n")
  if(missing(ntopcell)) ntopcell <- length(tab)
  topcell <- names(tab)[1:min(c(ntopcell, length(tab)))]
  matid <- match(topcell, dat$cellid)
  cdf <- dat[matid,]
  points(cdf$cellx, cdf$celly, pch="*", cex=2, col=2)

  if(arrow){
    point0 <- cbind(cdf$cellx, cdf$celly)
    alpha <- 90-cdf$azimuth
    dpoint <- c(cos(alpha/180*pi), sin(alpha/180*pi))*1000
    point1 <- point0+dpoint
    arrows(x0=point0[,1], y0=point0[,2],
           x1=point1[,1], y1=point1[,2],
           col=2, length=0.1)
  }
  cdf[,c("cellid", "cellx", "celly", "azimuth")]
}

locationSignature <- function(dat, grid.Ec=0.5){
  ## create the location specific likelihood
  ## output: for each location, a list contain the cell probablity, 
  ## and Ec histogram for the cells

  if(F){
    locsig <- locationSignature(sel)
  }

  locs <- unique(dat$gridloc)
  locsig <- initialList(locs)

  for(i in 1:length(locsig)){
    print(i)
    tmp <- dat[dat$gridloc==locs[i],]
    ##browser()
    cellp <- table(tmp$cellid)
    cellp <- cellp/sum(cellp)  ## cell probabilities
    ncell <- length(cellp)
    cellname <- names(cellp)

    ##for each location, what is the histogram for power
    out <- tapply(floor(tmp$Ec/grid.Ec)*grid.Ec, tmp$cellid, function(x){
      tab <- table(x)
      tab/sum(tab)
    })
    locsig[[i]] <- list(cellp=cellp, cellEc=out, cellname=names(cellp), grid.Ec=grid.Ec)
  }
  locsig
}

locationProb <- function(cellid, Ec, locsig, scale="I"){
  ## same program as locationProb.old, but much faster
  ## compute the probability of each location given the locationSignature
  ## cellid: a vector of cellids, Ec is a vector of Ecs
  ## I am going to loop through locsig, use vector operation maybe faster
  
  if(F){
    locsig <- locationSignature(sel)
    locationProb(sel[1,"cellid"], sel[1,"Ec"], locsig)
  }

  ##browser()
  if(length(cellid)!=length(Ec)){
    stop("length(cellid) != length(Ec)\n")
  }
  grid.Ec <- sapply(locsig, function(x){x$grid.Ec})
  if(max(abs(grid.Ec-min(grid.Ec)))>0){
    stop("grid.Ec should all equal!!!")
  }
  grid.Ec <- grid.Ec[1]
  Ec <- floor(Ec/grid.Ec)*grid.Ec

  ## for each potential location, and potential estimate
  nobs <- length(Ec)  ## number of rows or observations
  nloc <- length(locsig) ## number of location estimates
  allp <- matrix(0, nobs, nloc)
  dimnames(allp) <- list(loc=1:length(cellid), estimate=names(locsig))
  for(i in 1:nloc){  ## for each potential location estimate
    print(i)
    mat <- match(cellid, locsig[[i]]$cellname)
    cellp <- rep(0, nobs)
    powp <- rep(0, nobs)
    id <- (!is.na(mat))  ## those observed cells at show up in locsig[[i]]$cellname
    cellid.mat <- cellid[id] ## cellid for those matched cells
    cellp[id] <- locsig[[i]]$cellp[cellid.mat]
    Ec.mat <- Ec[id]
    powp.mat <- powp[id]
    for(k in 1:sum(id)){  ## for each obs. that has matched cellname
      powp.mat[k] <- locsig[[i]]$cellEc[[cellid.mat[k]]][as.character(Ec.mat[k])]
    }
    powp[id] <- powp.mat
    powp[is.na(powp)] <- rep(0, sum(is.na(powp)))  ## sometimes the power does not have a match in the signature
    if(scale=="I") allp[,i] <- cellp*powp
    if(scale=="log") {
      cellp[cellp==0] <- rep(exp(-10), sum(cellp==0)) ## give a lower bound
      powp[powp==0] <- rep(exp(-10), sum(powp==0))   ## give a lower bound
      pout <- log(cellp)+log(powp)
      allp[,i] <- pout
    }
  }
  allp
}


locationProb.old <- function(cellid, Ec, locsig){
  ## compute the probability of each location given the locationSignature
  ## cellid: a vector of cellids, Ec is a vector of Ecs
  if(F){
    locsig <- locationSignature(sel)
    locationProb.old(sel[1,"cellid"], sel[1,"Ec"], locsig)
  }

  ##browser()
  if(length(cellid)!=length(Ec)){
    stop("length(cellid) != length(Ec)\n")
  }
  grid.Ec <- sapply(locsig, function(x){x$grid.Ec})
  if(max(abs(grid.Ec-min(grid.Ec)))>0){
    stop("grid.Ec should all equal!!!")
  }
  grid.Ec <- grid.Ec[1]
  Ec <- floor(Ec/grid.Ec)*grid.Ec

  ## for each potential location, and potential estimate
  allp <- matrix(0, length(cellid), length(locsig))
  dimnames(allp) <- list(loc=1:length(cellid), estimate=names(locsig))
  for(j in 1:nrow(allp)){  ## for each observed location
    for(i in 1:ncol(allp)){  ## for each potential location estimate
      if(!is.na(match(cellid[j], locsig[[i]]$cellname))){
        cellp <- locsig[[i]]$cellp[cellid[j]]
        ## if the names do not match, then return 0
        tmp <- locsig[[i]]$cellEc[[cellid[j]]][as.character(Ec[j])] 
        if(!is.na(tmp)) allp[j,i] <- cellp*tmp 
      }
    }
  }
  allp
  t(apply(allp, 1, function(x){x/sum(x)}))  ## normalize so the sum is 1
}

locationEstimate <- function(locprob, loccord, type="mean"){
  ## given location proablity and coordinates, compute the estimated locations
  ## locprob: a matrix with dimension (#observations x #potential locations)
  ## loccord: a matrix with dimension (#potential locations x 2) (for lat, long or x,y)

  ##browser()
  
  if(ncol(locprob) != nrow(markdf)) stop("Number of potential locations should be the same!!")
  loccord <- loccord[dimnames(locprob)[[2]],]

  nloc <- ncol(locprob)
  nobs <- nrow(locprob)
 
  nmes <- names(loccord)
  if(type=="mean") estm <- locprob %*% as.matrix(loccord)
  if(type=="max") {
    idx <- apply(locprob, 1, function(x){seq(x)[x==max(x)][1]})
    estm <- loccord[idx,]
  }

  if(type=="mean" || type=="max"){
    estdf <- list(estm[,1], estm[,2]) ## convert this to data frame
    names(estdf) <- nmes
    estdf <- data.frame(estdf)
  } else {warnings("Wrong specification of markdf!"); estdf <- NULL}
  
  estdf
}


breakByTime <- function(x, period=10){
  x <- x-min(x)
  disx <- x %/% period
  disx
}

locationProbBlock <- function(locprob, timeblock){
  ## ptype: probability type
  foo1 <- function(x){
    x <- as.matrix(x[,-ncol(x)])
    lp <- apply(x, 2, function(a){exp(sum(log(a)))})
    lp
  }
  foo <- function(x){
    ##browser()
    x <- as.matrix(x[,-ncol(x)])
    lp <- apply(x, 2, function(a){sum(log(a))})
    lp <- lp-max(lp)
    newp <- exp(lp)
    newp/sum(newp)
  }
  l2 <- ddply(locprobdf, .(timeblock), "foo1")
  l3 <- ddply(locprobdf, .(timeblock), "foo")
  l3
}

gridLocXY <- function(sel, grid){
  ## marks are those gridloc observed in sel
  markx <- tapply(sel$gridx, sel$gridloc, function(x){unique(x)})*grid
  marky <- tapply(sel$gridy, sel$gridloc, function(x){unique(x)})*grid
  markdf <- data.frame(x=markx, y=marky)
  markdf
}

antPatternDiff <- function(dist, type="DB950F85T2EM", ant.height, tilt,
                           angle1, angle2,
                           dir="/home/cao/Aware/geoLocation/JinCao_AntPattern",
                           ...){
  browser()
  out1 <- antPattern(dist, type, ant.height, tilt, angle1, dir, ...)$gain
  out2 <- antPattern(dist, type, ant.height, tilt, angle2, dir, ...)$gain
  out1-out2
}

antPattern <- function(dist, type="DB950F85T2EM", ant.height, tilt,
                       angle, dir="/home/cao/Aware/geoLocation/JinCao_AntPattern",
                       draw=T, adjust=T){
  ## angle is a number between 0 to 180, could be negative

  ## rel_azimuth=azimuth-AzimuthBS;
  if(F){
    antPattern(dist=500, type="AP17_1900_090D", ant.height=90, tilt=0, angle=0:100)
  }

  ##browser()
  angle.old <- angle
  id <- angle<0
  angle[id] <- angle[id]+360  ## convert between (-180,180) to (0,360)
  
  ms.height=1.5
  rel.azimuth = angle
  d <- dist

  rel.elevation=atan((ant.height-ms.height)/d)*(180/pi) -tilt
  type <- sub(".pdf", "", type)
  filename <- paste(dir, "/", type, ".m", sep="")
    
  if(F){
    cmd <- paste("grep -i -n '_pat'", filename)
    lineno <- system(cmd, intern=T)
  }
  tmpfile <- tempfile()
  cmd <- paste("cat", filename, "| sed -e 's/;//g' -e 's/\\[//g' -e 's/\\]//g' -e 's/hor_pat=//g' -e 's/vert_pat_front=//g' -e 's/vert_pat_back=//g' >", tmpfile)
  system(cmd)
  df <- read.table(tmpfile)
  unlink(tmpfile)

  names(df) <- c("angle", "dB")
  if(nrow(df)!=722) {stop(paste("error in", filename, "!!! Not standard format !!!\n"))}
  hor.pat = as.matrix(df[1:360,])
  vert.pat.front = as.matrix(df[361:(360+181),])
  vert.pat.back = as.matrix(df[(360+182):(360+181+181),])
  if(draw) {
    par(mfrow=c(2,1))
    plot(hor.pat[,1], hor.pat[,2], type="l", xlab="horizontal patterns",
         ylab="dB", main=type)
    plot(vert.pat.front[,1], vert.pat.front[,2], type="l", xlab="vertical pattern",
         ylab="dB")
  }
  
  ##index=find(rel_azimuth<0);
  ##rel_azimuth(index)=360+rel_azimuth(index);

  ##index=find(rel_azimuth>359);
  ##rel_azimuth(index)=0;

  ##ant_filename=['DB982F30EM'];     % 30 degree beam width 0 Electric Tilt 
  ##ant_filename=['DB950F65EM'];     % 65 degree beam width 0 Electric Tilt
  ##ant_filename=['DB950F85EM'];     % 85 degree beam width 0 Electric Tilt
  ##ant_filename=['DB950F85T2EM'];     % 85 degree beam width 2 Electric Tilt
  ##ant_filename=['X7C_FRO_440_6'];    % 40 degree beam width 6 Electric Tilt
  ##ant_filename=['X7C_465_6'];        % 65 degree beam width 6 Electric Tilt
  ##eval(ant_filename);                            % Set the antenna pattern

  ## CE4 way to calculate antenna pattern
  h.bore=max(hor.pat[,2])
  h.back=approx(hor.pat[,1],hor.pat[,2],180)$y
  v.front=approx(vert.pat.front[,1],vert.pat.front[,2],rel.elevation)$y
  v.back=approx(vert.pat.back[,1],vert.pat.back[,2],rel.elevation)$y

  ## modify by Susan 12/2/2010, smooth out h.back v.back
  if(F){
    tmpindxback= (hor.pat[,1]<=180 & hor.pat[,1]>=160);
    h.back= mean( hor.pat[tmpindxback,2]);
    tmpindxback= vert.pat.back[,1]<=10 & vert.pat.back[,1]>=-10;
    v.back= mean( vert.pat.back(tmpindxback,2));
    ## end of modify by Susan 12/2/2010, smooth out h.back v.back
  }

  ##browser()
  add <- v.front-h.bore+2.15+abs(angle.old)/180*((h.bore-h.back)-(v.front-v.back))
  gain = approx(hor.pat[,1],hor.pat[,2],angle)$y
  if(adjust) gain <- gain+add

  list(gain=gain, hor.pat=hor.pat, vert.pat.back=vert.pat.back,
       vert.pat.front=vert.pat.front)
}


cellsFromSameTower <- function(cells){
  ## check if there are cellids from the same tower (BS)
  cells <- unique(cells)
  bs <- extract(cells, sep="/", n=1)
  tab <- table(bs)
  max(tab)>=2
}

getcellsFromSameTower <- function(df){
  foo <- function(x){
    mEc <- tapply(x$Ec, x$cellid, mean)
    mEc
  }
}
