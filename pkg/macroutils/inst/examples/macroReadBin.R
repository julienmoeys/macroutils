require( "macroutils" )


# ====== Example 1: MACRO model weather file ====== 

#   Path to the file to be read
( filenm <- system.file( "bintest/METFILE.BIN", 
    package = "macroutils", mustWork = TRUE ) )

#   Read the file
tmp1 <- macroReadBin( file = filenm ) 

#   Using different settings
muPar( alphaNumOnly = FALSE, removeSpace = FALSE ) 

tmp3 <- macroReadBin( file = filenm ) 

colnames( tmp1 ) 
colnames( tmp3 ) 

dim( tmp1 ) 
dim( tmp3 ) 


#   Reset settings
muPar( reset = TRUE ) 


# ====== Example 2: SOIL model input file ====== 

#   Path to the file to be read
( filenm <- system.file( "bintest/soiln001.BIN", 
    package = "macroutils", mustWork = TRUE ) ) 

#   Read the file
tmp1 <- macroReadBin( file = filenm ) 

#   Using different settings
muPar( alphaNumOnly = FALSE, removeSpace = FALSE ) 

tmp3 <- macroReadBin( file = filenm ) 

colnames( tmp1 ) 
colnames( tmp3 ) 

dim( tmp1 ) 
dim( tmp3 ) 


# Reset settings
muPar( reset = TRUE ) 
