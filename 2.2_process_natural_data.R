
# process natural.list 


natural.in <- natural.list  
natural.in$toll <- gsub(",", "", natural.in$toll)
natural.in <- separate(natural.in, toll, c("toll.low","check"), sep = "\\[" , extra="merge")
natural.in <- separate(natural.in, toll.low, c("toll.low.keep","drop"), sep = "\\(" , extra="merge")

natural.in$toll.low.keep <- gsub("more than ", "", natural.in$toll)
natural.in$toll.low.keep <- gsub("up to ", "", natural.in$toll)


natural.in$toll.low.keep <- ifelse(nchar(natural.in$toll.low.keep) > 20,
                            substring(natural.in$toll.low.keep, 21, nchar(natural.in$toll.low.keep)),
                            natural.in$toll.low.keep) 

natural.in <- separate(natural.in, toll.low.keep, 
                       c("toll.low","toll.high"), extra="merge", remove = FALSE)

for (i in 1:nrow(natural.in)){    
    natural.in$toll.high[i] <- ifelse((is.na(natural.in$toll.high[i]) | (natural.in$toll.high[i] < natural.in$toll.low[i])),
                                      natural.in$toll.low[i], natural.in$toll.high[i])
}

natural.in <- mutate(natural.in,
                   toll = ((as.numeric(toll.low) + as.numeric(toll.high)) / 2)/1000000
)

natural.in <- filter(natural.in,toll >= 0.1)

natural.in <- separate(natural.in, date, c("start.date","check.date"), , extra="merge")

for (i in 1:nrow(natural.in)){    
    natural.in$check.date[i] <- ifelse(is.na(natural.in$check.date[i]),
                                      natural.in$start.date[i], natural.in$check.date[i])
}

natural.in$check.date <- gsub("BC", "0000", natural.in$check.date)
natural.in$check.date <- gsub(" A.D.", "", natural.in$check.date)

natural.in$check.date <- ifelse(nchar(natural.in$check.date) > 4,
                                   substring(natural.in$check.date, (nchar(natural.in$check.date)-3), nchar(natural.in$check.date)),
                                   natural.in$check.date) 
 
natural.in$start.date <- extract_numeric(natural.in$start.date)
natural.in$check.date <- extract_numeric(natural.in$check.date)

for (i in 1:nrow(natural.in)){    
    natural.in$check.date[i] <- ifelse(natural.in$check.date[i] < natural.in$start.date[i],
                                      natural.in$start.date[i], natural.in$check.date[i])
}

natural.in <- mutate(natural.in,date = round((start.date + check.date)/2)) 

natural.in <- filter(natural.in,date > 1500) 

#names(natural.in)

natural.in <- select(natural.in,c(6,7,10:13)) 
natural.in <- arrange(natural.in,date)
