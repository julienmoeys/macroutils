library( "macroutils" )



# ====== Read a bin file ======

#   Format the path to a test binary file
( filenm <- system.file( "bintest/METFILE.BIN", 
    package = "macroutils", mustWork = TRUE ) ) 

#   Read the binary file
tmp1 <- macroReadBin( file = filenm ) 

#   Inspect the table
colnames( tmp1 ) 

dim( tmp1 ) 



# ====== Aggregate the results =====

#   Sum by year and month:
( r1 <- macroAggregateBin( x = tmp1, by = "%Y-%m", FUN = sum ) )

#   Sum by month too, but on one column only:
( r2 <- macroAggregateBin( 
    x       = tmp1, 
    columns = "Wind_speed_m_s_1_Sutton_Bonington_TRS1", 
    by      = "%Y-%m", 
    FUN     = sum ) ) 


#   Sum by week of year (00 -> 53):
r3 <- macroAggregateBin( x = tmp1, by = "%Y-%W", FUN = sum ) 

#   Inspect the results
head( r3 ) 

tail( r3 ) 

#   Notice the new format of the column 'Date' ("character")
class( r1[,"Date"] )

#   Trick to convert r1$Date to POSIXct date again, by adding a virtual day:
r1[,"Date"] <- as.POSIXct( paste( sep = "", r1[,"Date"], "-15" ), 
    format = "%Y-%m-%d", tz = getMuPar( "tz" ) ) 
class( r1[,"Date"] )

#   Plot the results
plot( r1[,2] ~ r1[,"Date"], type = "b", col = "red" ) 



# ====== using the original R aggregate() =====

#   More code, but a bit faster
r1b <- aggregate( 
    x   = tmp1[,-1], 
    by  = list( "Date" = format.POSIXct( tmp1[,"Date"], "%Y-%m" ) ), 
    FUN = sum ) 
r1b 

identical( r1[,-1], r1b[,-1] )
