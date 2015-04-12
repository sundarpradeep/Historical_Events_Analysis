  
# read input population file , filter worldwide data and format 
#head(population.raw)

population.in <- filter(population.raw , Year == "World")
population.in[1,] <- gsub(" ", "", population.in[1,])
population.in <- select(population.in,-c(2,3))
population.in <- gather(population.in,date,total ,X1500:X1998)
names(population.in)[1] <-  "Geo"
population.in$date <- gsub("X", "", population.in$date)

# Add most recent population data 

population.in <- rbind(population.in,c("World","2015","7304813"))

population.in$total <- as.numeric(population.in$total)
population.in$decade <- round(as.numeric(population.in$date),-1)
population.in$date <- as.Date(population.in$date,"%Y")

#qplot(date,total,data=population.in,aes(color="red"))

# create a new file at "decade" level - will be used for projection 

decades <- seq(1500,2020,10)
dim(decades) <- c(length(decades),1)
decades <- as.data.frame(decades,header=T)
names(decades)  <- "decade"
decades$date <- as.character(decades$decade)
decades$date <- as.Date(decades$date,"%Y")

#str(decades)

