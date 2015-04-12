# review and fix locations that cant be fixed programmatically 

use_fixed <- 1 # (switch use_fixed to 1 to get fixed locations)

if (use_fixed == 1) { 
    
    cleaned.location <- tbl_df(read.csv("check_locations.csv",
                                        header=T,stringsAsFactors=FALSE)) %>% 
        filter(location != fixed)
    consolidated.in <- left_join(consolidated.in,cleaned.location,by="location")
    
    consolidated.in$fix.flag <- as.character(consolidated.in$fix.flag)
    consolidated.in$fix.flag <- as.numeric(consolidated.in$fix.flag)
    consolidated.in$fix.flag[is.na(consolidated.in$fix.flag)] == 0

    for (i in 1:nrow(consolidated.in)) {
        consolidated.in$fix.flag[i] <- ifelse(is.na(consolidated.in$fix.flag[i]), 
                                              0,
                                              1) 
    } 
        
    for (i in 1:nrow(consolidated.in)) {
        consolidated.in$location[i] <- ifelse(consolidated.in$fix.flag[i] == 1 , 
                                              consolidated.in$fixed[i],
                                              consolidated.in$location[i]) 
    } 

    consolidated.in <- select(consolidated.in,
                              -fixed,-fix.flag)
}

#fix(consolidated.in)