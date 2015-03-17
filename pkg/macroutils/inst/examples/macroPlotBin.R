
require( "macroutils" )



# ====== Example 1: SOIL model input file ======

#   Path to the file to be read
( filenm <- system.file( 
    c( "bintest/METFILE.BIN", "bintest/RAINFALL.BIN" ), 
    package  = "macroutils", 
    mustWork = TRUE 
) )

#   Read these 2 files
met  <- macroReadBin( file = filenm[ 1 ] ) 

rain <- macroReadBin( file = filenm[ 2 ] ) 

#   Inspect the data:
head( met ); dim( met ) 
head( rain ); dim( rain ) 



# ====== Plot the data ======

#   With sub-plots
macroPlot( x = met[, 1:4 ], gui = FALSE ) 

#   In one plot
macroPlot( x = met[, 1:4 ], gui = FALSE, subPlots = FALSE ) 

#   Plot Multiple tables at once 
#   Format a list of tables (as when interactively imported)
metRain        <- list( met, rain ) 
names(metRain) <- c( "METFILE", "RAINFALL" ) 

macroPlot( x = metRain, gui = FALSE, subPlots = TRUE ) 
