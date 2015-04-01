
setwd("~/dev/Projects/2_timeseries_visualization/")


epidemicsUrl = 'http://en.wikipedia.org/wiki/List_of_epidemics'
manmadeUrl = 'http://en.wikipedia.org/wiki/List_of_wars_and_anthropogenic_disasters_by_death_toll'
naturalUrl ='http://en.wikipedia.org/wiki/List_of_natural_disasters_by_death_toll' 

epidemics.raw = readHTMLTable(epidemicsUrl, stringsAsFactors = FALSE)
manmade.raw = readHTMLTable(manmadeUrl, stringsAsFactors = FALSE)
natural.raw = readHTMLTable(naturalUrl, stringsAsFactors = FALSE)

# Epidemics file
epidemics.list <- tbl_df(as.data.frame(epidemics.raw[[1]]))
epidemics.list <- mutate(epidemics.list,
                         major="epidemics",
                         minor="epidemics"
                         )

epidemics.list <- select(epidemics.list,-c(4,6) )
names(epidemics.list) <- c('toll','location','date','event','major','minor')

# Natural file

# check if tables are of same names and dimensions
#for (i in 3:15) { 
#    print(names(natural.raw[[i]]))
#    print(dim(natural.raw[[i]]))
#}    

# Names are different  , list element 8 is null 
# copy raw file before changes to original

natural.in <- natural.raw 
natural.in[[8]] <- NULL  # remove null table

# read tables , rename column names for consistency and add type field

natural.list <- NULL
natural.list <- data.frame()

for (i in 3:15) { 
    names(natural.in[[i]]) <- c('rank','toll','event','location','date')
    natural.in[[i]]$major <- "natural"    
    natural.in[[i]]$minor <- i    
    natural.list <- rbind(natural.list,natural.in[[i]])
}

for (i in 1:nrow(natural.list)) {
    if ( natural.list$minor[i] == "3" ) {
        natural.list$minor[i] = "avalanches"
    }
    else if ( natural.list$minor[i] == "4" ) {
        natural.list$minor[i] = "blizzards"
    }
    else if ( natural.list$minor[i] == "5" ) {
        natural.list$minor[i] = "cyclones"
    }
    else if ( natural.list$minor[i] == "6" ) {
        natural.list$minor[i] = "earth_quakes"
    }
    else if ( natural.list$minor[i] == "7" ) {
        natural.list$minor[i] = "floods"
    }
    else if ( natural.list$minor[i] == "8" ) {
        natural.list$minor[i] = "heat_waves"
    }
    else if ( natural.list$minor[i] == "9" ) {
        natural.list$minor[i] = "lightning_strikes"
    }
    else if ( natural.list$minor[i] == "10" ) {
        natural.list$minor[i] = "limnic_eruptions"
    }
    else if ( natural.list$minor[i] == "11" ) {
        natural.list$minor[i] = "storms"
    }
    else if ( natural.list$minor[i] == "12" ) {
        natural.list$minor[i] = "tornadoes"
    }
    else if ( natural.list$minor[i] == "13" ) {
        natural.list$minor[i] = "tsunamis"
    }
    else if ( natural.list$minor[i] == "14" ) {
        natural.list$minor[i] = "volcanic_eruptions"
    }
    else if ( natural.list$minor[i] == "15" ) {
        natural.list$minor[i] = "wildfires"
    }
    else  {
        print("Error: check values")
    }
}

natural.list <- select(natural.list,-1)

# Manmade file

manmade.in <- manmade.raw

# check elements for format 

#for (i in 1:6) {
#    print(i)
#    print(names(manmade.raw[[i]]))
#    print(dim(manmade.raw[[i]]))
#}    
    


# tables are in very different format , to be processed individually

wars.list <- tbl_df(as.data.frame(manmade.raw[[1]]))
camps.list <- tbl_df(as.data.frame(manmade.raw[[2]]))
famine.list <- tbl_df(as.data.frame(manmade.raw[[3]]))
floods.list <- tbl_df(as.data.frame(manmade.raw[[4]]))
rituals.list <- tbl_df(as.data.frame(manmade.raw[[5]]))
other.list <- tbl_df(as.data.frame(manmade.raw[[6]]))


# read population table 

population.raw <- tbl_df(read.xlsx("data/world_population_over_centuries.xlsx",
                            1, header=TRUE, startRow=4,, stringsAsFactors=FALSE)) 


