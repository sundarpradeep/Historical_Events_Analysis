
# process epidemic.file 

epidemics.in <- tbl_df(epidemics.list)

epidemics.in$date <- gsub("present", "2015", epidemics.in$date)
epidemics.in <- separate(epidemics.in, date, c("start.date","end.date"),extra="drop", remove=F)

for (i in 1:nrow(epidemics.in)){    
    epidemics.in$end.date[i] <- ifelse(is.na(epidemics.in$end.date[i]),
                                       epidemics.in$start.date[i], epidemics.in$end.date[i])
}

epidemics.in <- mutate(epidemics.in,
                     date = (extract_numeric(start.date) + extract_numeric(end.date)) / 2)

epidemics.in <- filter(epidemics.in,date >= 1500 & nchar(toll) > 0)

epidemics.in$toll <- gsub(",", "", epidemics.in$toll)
epidemics.in$toll <- gsub(">", "", epidemics.in$toll)
epidemics.in$toll <- gsub("+", "", epidemics.in$toll)

epidemics.in <- separate(epidemics.in, toll, c("toll.low","drop"),extra="merge",remove=F)

epidemics.in$toll.low[1] = 12000 # http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2957993/

epidemics.in <- mutate(epidemics.in, 
                       toll = as.numeric(toll.low)/1000000)  

epidemics.in <- filter(epidemics.in,toll >= 0.1)

epidemics.in <- select(epidemics.in, c(1,4,5,8:10))

