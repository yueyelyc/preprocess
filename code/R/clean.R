##### 
## clean google poi
## 1. combine different poi datasets
## 2. remove unnecessary attributes
## 3. map category to poi taxonomy
poi1 <- read.csv(file="~/projects/elis/data/google_poi_bell_labs_50by50.csv",  quote = "", sep="\t", header=T, stringsAsFactors = FALSE)
poi2 <- read.csv(file="~/projects/elis/data/google_poi_nyc.csv",  quote = "", sep="\t", header=T, stringsAsFactors = FALSE)
poi.all <- rbind(poi1, poi2)
dup_ind <- duplicated(poi.all$place_id)
poi <- poi.all[!dup_ind, c("place_id", "name", "latitude", "longitude", "categories", "vicinity", "rating", "price_level")]

poi.cate.google <- read.csv(file="~/projects/elis/output/poi_cate_google.csv",  quote = "", sep=",", header=T, stringsAsFactors = FALSE)
colnames(poi)[which(names(poi) == "latitude")] <- "lat"
colnames(poi)[which(names(poi) == "longitude")] <- "lon"

poi[,"categories"] <- gsub("[\"]", "", as.matrix(poi[,"categories"]))
poi[,"categories"] <- gsub("[[]", "", as.matrix(poi[,"categories"]))
poi[,"categories"] <- gsub("[]]", "", as.matrix(poi[,"categories"]))
poi[,"categories"] <- gsub(",", " ", as.matrix(poi[,"categories"]))
poi <- poi[which(grepl("point_of_interest",poi$categories)),]
poi[,"categories"] <- gsub("point_of_interest", "", as.matrix(poi[,"categories"]))
poi[,"categories"] <- trimws(as.matrix(poi[,"categories"]))
poi[,"categories"] <- gsub("  ", " ", as.matrix(poi[,"categories"]))

poi[,"category"] <- apply(poi, 1, function(x) strsplit(x['categories'], " ")[[1]][1])
require(dplyr)
poi.new <- join(poi,poi.cate.google, by = "category", type = 'left')
table(poi.new[,c("l9")])/nrow(poi.new)
write.table(poi.new, "~/projects/elis/data/google_poi_bell_labs.csv", quote = FALSE, sep = "\t", row.names = FALSE)
## end clean google poi
