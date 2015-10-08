
library( "macroutils" ) 

#   Path to the file to be read
( filenm <- system.file( "bintest/MACRO001_20151005.BIN", 
    package = "macroutils", mustWork = TRUE ) )

res <- macroInFocusGWConc( x = filenm ) 

res 

attr( res, "more" ) 

#   Clean-up
rm( filenm, res )  

