
library( "macroutils" )

timeseries <- seq.POSIXt(
    from = as.POSIXct( "2015-01-01 12:00", tz = "GMT" ), 
    to   = as.POSIXct( "2015-12-31 12:00", tz = "GMT" ), 
    by   = "day"
)   

isValidTimeSeries( x = timeseries ) 
# [1] TRUE



warning2 <- function(w){message("",w)}

#   The time series is not increasing
tryCatch( isValidTimeSeries( x = rev( timeseries ) ), 
    warning = warning2 )

#   The time series is not increasing homogeneously
tryCatch( isValidTimeSeries( x = timeseries[ -2 ] ), 
    warning = warning2 )

#   The time series has duplicated values
timeseries2 <- rep( timeseries, each = 2 ) 
tryCatch( isValidTimeSeries( x = timeseries2 ), 
    warning = warning2 )



#   See also option 'timeSeriesValid' in ?muPar
