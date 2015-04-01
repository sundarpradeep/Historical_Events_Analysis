library(taRifx.geo)

# merge all input files

consolidated.in <- rbind(manmade.in,
                         epidemics.in,
                         natural.in)

# clean-up location field


consolidated.in <- separate(consolidated.in, location, c("location.old","location.new"),
                            sep = "now", extra="merge", remove=F)

consolidated.in$location.new <- gsub("\\)", "", consolidated.in$location.new)

for (i in 1:nrow(consolidated.in)){    
    consolidated.in$location[i] <- ifelse(is.na(consolidated.in$location.new[i]),
                                          consolidated.in$location[i], 
                                          consolidated.in$location.new[i])
}


consolidated.in <- separate(consolidated.in, location, c("location.check","drop"),
                            sep = "\\(", extra="merge", remove=F)

consolidated.in$location <- consolidated.in$location.check

consolidated.in$location.old <- consolidated.in$location.new <- consolidated.in$drop <- consolidated.in$location.check <- NULL 

# clean-up location field

options(BingMapsKey='AgyqIM0pl1I8dJr8Tfb7efnQAChYXjdaSi73yGP1XVDITzldbFAaJKzTfZR3QzJX')

coordinates <- sapply(consolidated.in$location,
                    function(x) taRifx.geo::geocode(x,service='bing',returntype='coordinates'))

coordinates.in <- as.data.frame(t(as.data.frame(coordinates)))

consolidated.final <- cbind(consolidated.in,coordinates.in)
names(consolidated.final)[7:8] <- c("lat","lon")

consolidated.final$major <- as.factor(consolidated.final$major)
consolidated.final$minor <- as.factor(consolidated.final$minor)
consolidated.final$decade <- round(as.numeric(consolidated.final$date),-1)

consolidated.final <- arrange(consolidated.final,date)
#str(consolidated.final)
