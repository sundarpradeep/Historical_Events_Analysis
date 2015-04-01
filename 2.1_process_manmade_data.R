# Process Man-made files 

# process wars table 

wars.in <- tbl_df(as.data.frame(manmade.raw[[1]]))
#str(wars.in)
wars.in$From <- as.numeric(wars.in$From)
wars.in$To <- as.numeric(wars.in$To)

wars.in <- filter(wars.in,To > 1500 ) 

wars.in <- mutate(wars.in,date = round((From + To)/2),
                  major="man-made",
                  minor="wars"
                  ) 

wars.in <- select(wars.in,c(2,4,5,10:12)) 
names(wars.in) <- c('toll','event','location','date','major','minor')
wars.in$toll <- extract_numeric(wars.in$toll)

wars.in <- filter(wars.in, toll>0.1)

wars.in <- arrange(wars.in,date) 

# process camps table 

camps.in <- tbl_df(as.data.frame(manmade.raw[[2]]))
camps.in$Deaths <- gsub(",", "", camps.in$Deaths)
camps.in <- separate(camps.in,Deaths,c("toll.low","toll.high"),
                     extra="merge", remove=FALSE,convert=TRUE)
camps.in <- separate(camps.in,Date,c("start.date","end.date"),
                     extra="merge", remove=FALSE,convert=TRUE)

for (i in 1:nrow(camps.in)){    
    camps.in$toll.high[i] <- ifelse(is.na(camps.in$toll.high[i]),
           camps.in$toll.low[i], camps.in$toll.high[i])
    camps.in$end.date[i]<- ifelse(is.na(camps.in$end.date[i]),
           camps.in$start.date[i], camps.in$end.date[i])    
}
camps.in <- mutate(camps.in,
                   toll = ((toll.low + toll.high) / 2)/1000000, 
                   date = (start.date + end.date) / 2,
                   major = "man-made",
                   minor="camps"
)

camps.in <- select(camps.in,c(4,6,11:14))
camps.in <- arrange(camps.in,date)

names(camps.in) <- c('event','location','toll','date','major','minor')


# process famine table 

famine.in <- tbl_df(as.data.frame(manmade.raw[[3]]))
names(famine.in) <- c('toll.low','toll.high','event','location','start.date','end.date','notes')

famine.in$toll.low <- substring(famine.in$toll.low, 20, nchar(famine.in$toll.low))
famine.in$toll.high <- substring(famine.in$toll.high, 20, nchar(famine.in$toll.high))

famine.in$toll.low <- gsub(",", "", famine.in$toll.low)
famine.in$toll.high <- gsub(",", "", famine.in$toll.high)

famine.in <- separate(famine.in,toll.low,c("toll.low.keep","toll.low.drop"),
                     extra="merge", remove=FALSE,convert=TRUE)
famine.in <- separate(famine.in,toll.high,c("toll.high.keep","toll.high.drop"),
                     extra="merge", remove=FALSE,convert=TRUE)

famine.in <- mutate(famine.in,
                    toll = ((toll.low.keep + toll.high.keep) / 2)/1000000 , 
                    date = (as.numeric(start.date) + as.numeric(end.date)) / 2,
                    major = "man-made",
                    minor = "famine"
    )

famine.in <- select(famine.in,c(7,8,12:15))


# process floods table 

floods.in <- tbl_df(as.data.frame(manmade.raw[[4]]))
names(floods.in)[2] <- "toll"
floods.in$toll <- gsub(",", "", floods.in$toll)

floods.in$Date <- ifelse(nchar(floods.in$Date) > 4,
                          substring(floods.in$Date, (nchar(floods.in$Date)-3), nchar(floods.in$Date)),
                         floods.in$Date) 

floods.in$Date <- ifelse(nchar(floods.in$Date) > 4,
                         substring(floods.in$Date, (nchar(floods.in$Date)-3), nchar(floods.in$Date)),
                         floods.in$Date) 

floods.in$toll <- gsub("more than ", "", floods.in$toll)
floods.in$toll <- gsub("]", "", floods.in$toll)

floods.in <- separate(floods.in,toll,c("toll.low","toll.high","drop"),
                      extra="drop", remove=FALSE,convert=TRUE)

for (i in 1:nrow(floods.in)){
    floods.in$toll.high[i] <- ifelse(is.na(floods.in$toll.high[i]),
                               floods.in$toll.low[i], floods.in$toll.high[i])
    floods.in$toll.high[i] <- ifelse(as.numeric(floods.in$toll.high[i]) < 
                                         as.numeric(floods.in$toll.low[i]),
                                     floods.in$drop[i], floods.in$toll.high[i])
    
}

floods.in <- mutate(floods.in,
                     toll = ((toll.low + toll.high) / 2)/1000000 , 
                     major = "man-made",
                     minor = "floods"
)

floods.in <- select(floods.in,c(2,6:10))

names(floods.in)[2:4] <- c("event","location","date") 



# process rituals table 

rituals.in <- tbl_df(as.data.frame(manmade.raw[[5]]))
#str(rituals.in)

names(rituals.in)[1:2] <- c("toll.low","toll.high")
rituals.in$toll.low <- ifelse(nchar(rituals.in$toll.low) > 20,
                   substring(rituals.in$toll.low, 20, nchar(rituals.in$toll.low)),
                             rituals.in$toll.low) 
rituals.in$toll.high <- ifelse(nchar(rituals.in$toll.high) > 20,
                              substring(rituals.in$toll.high, 20, nchar(rituals.in$toll.high)),
                              rituals.in$toll.high) 

rituals.in$toll.low <- gsub(",", "", rituals.in$toll.low)
rituals.in$toll.high <- gsub(",", "", rituals.in$toll.high)

rituals.in <- separate(rituals.in,toll.low,c("toll.low.keep","toll.low.drop"),
                      extra="merge", remove=FALSE,convert=TRUE)
rituals.in <- separate(rituals.in,toll.high,c("toll.high.keep","toll.high.drop"),
                      extra="merge", remove=FALSE,convert=TRUE)

rituals.in$From <- gsub("BC", "-", rituals.in$From)
rituals.in$To <- gsub("BC", "-", rituals.in$To)

rituals.in$From <- gsub("century", "50", rituals.in$From)
rituals.in$From <- extract_numeric(rituals.in$From)
rituals.in$To <- extract_numeric(rituals.in$To)

for (i in 1:nrow(rituals.in)){
rituals.in$To[i] <- ifelse(is.na(rituals.in$To[i]),
                           rituals.in$From[i], rituals.in$To[i])
}

rituals.in <- filter(rituals.in,(as.numeric(rituals.in$To) > 1500))

rituals.in$From <- ifelse(nchar(rituals.in$From) > 4,
                       substring(rituals.in$From, (nchar(rituals.in$From)-3), nchar(rituals.in$From)),
                       rituals.in$From) 

rituals.in$To <- ifelse(nchar(rituals.in$To) > 4,
                          substring(rituals.in$To, (nchar(rituals.in$To)-3), nchar(rituals.in$To)),
                          rituals.in$To) 

rituals.in <- mutate(rituals.in,
                    toll = ((toll.low.keep + toll.high.keep) / 2)/1000000 , 
                    date = (as.numeric(From) + as.numeric(To)) / 2, 
                    major = "man-made",
                    minor = "rituals"
)

rituals.in <- select(rituals.in,c(8,9,13:16))

names(rituals.in)[1:2] <- c("event","location")

# process others table 

others.in <- tbl_df(as.data.frame(manmade.raw[[6]]))
names(others.in)[1:2] <- c("toll.low","toll.high")

others.in$toll.low <- substring(others.in$toll.low, 20, nchar(others.in$toll.low))
others.in$toll.high <- substring(others.in$toll.high, 20, nchar(others.in$toll.high))

others.in$start.date <- substring(others.in$From, (nchar(others.in$From)-4), nchar(others.in$From))
others.in$end.date <- substring(others.in$To, (nchar(others.in$To)-4), nchar(others.in$To))

others.in$toll.low <- gsub(",", "", others.in$toll.low)
others.in$toll.high <- gsub(",", "", others.in$toll.high)

others.in <- separate(others.in,toll.low,c("toll.low.keep","toll.low.drop"),
                      extra="merge", remove=FALSE,convert=TRUE)
others.in <- separate(others.in,toll.high,c("toll.high.keep","toll.high.drop"),
                      extra="merge", remove=FALSE,convert=TRUE)

others.in <- mutate(others.in,
                    toll = ((toll.low.keep + toll.high.keep) / 2)/1000000 , 
                    date = (extract_numeric(start.date) + extract_numeric(end.date))/2,
                    major = "man-made",
                    minor = "others"
)

others.in <- select(others.in,c(7,8,14:17))

names(others.in)[1:2] <- c("event","location")

# merge man made files. 

manmade.in <- rbind(wars.in,
                      camps.in,
                      famine.in,
                      floods.in,
                      rituals.in,
                      others.in
                      )

manmade.in <- filter(manmade.in,as.numeric(date) > 1500 & toll >= 0.1)

manmade.in <- arrange(manmade.in,date)


