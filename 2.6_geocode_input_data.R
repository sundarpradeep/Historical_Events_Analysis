
library(taRifx.geo)
# geo-code

options(BingMapsKey='AgyqIM0pl1I8dJr8Tfb7efnQAChYXjdaSi73yGP1XVDITzldbFAaJKzTfZR3QzJX')


coordinates <- sapply(consolidated.in$location,
                    function(x) taRifx.geo::geocode(x,service='bing',returntype='coordinates'))
#                        function(x) geocode(x,returntype='coordinates'))

coordinates.in <- as.data.frame(t(as.data.frame(coordinates)))

consolidated.final <- cbind(consolidated.in,coordinates.in)
names(consolidated.final)[9:10] <- c("lat","lon")


