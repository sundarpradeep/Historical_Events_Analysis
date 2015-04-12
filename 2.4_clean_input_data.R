
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


consolidated.in$major <- as.factor(consolidated.in$major)
consolidated.in$minor <- as.factor(consolidated.in$minor)
consolidated.in$decade <- round(as.numeric(consolidated.in$date),-1)

for (i in 1:nrow(consolidated.in)){    
    if (consolidated.in$decade[i] >= 1500 & consolidated.in$decade[i] < 1600) {
        consolidated.in$century[i] = "16th Century"}
    else if (consolidated.in$decade[i] >= 1600 & consolidated.in$decade[i] < 1700) {
        consolidated.in$century[i] = "17th Century"}   
    else if (consolidated.in$decade[i] >= 1700 & consolidated.in$decade[i] < 1800) { 
        consolidated.in$century[i] = "18th Century"}   
    else if (consolidated.in$decade[i] >= 1800 & consolidated.in$decade[i] < 1900) {
        consolidated.in$century[i] = "19th Century" }   
    else if (consolidated.in$decade[i] >= 1900 & consolidated.in$decade[i] < 2000) { 
        consolidated.in$century[i] = "20th Century" }  
    else if (consolidated.in$decade[i] >= 2000 & consolidated.in$decade[i] < 2100) { 
        consolidated.in$century[i] = "21st Century" } 
    else { consolidated.in$century[i] = "Unknown" }   
}

consolidated.in$location <- trim(tolower(consolidated.in$location))

consolidated.in <- arrange(consolidated.in,date)


#str(consolidated.in)

unique.locations <- select(consolidated.in,location) %>%
                    distinct()

unique.locations$fixed <- unique.locations$location
unique.locations$fix.flag <- 1

#fix(unique.locations)
#write.csv(unique.locations,"check_locations.csv", row.names=F)
