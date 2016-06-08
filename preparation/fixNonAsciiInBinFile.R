
library( "macroutils" )

packDir <- file.path( sprintf( 
    "D:/Users/%s/Documents/_WORKS/_PROJECTS/r_packages/macro-se/", 
    Sys.info()[[ "user" ]] 
    ), "macroutils", "pkg", "macroutils" )

#   Path to the file to be read
( filenm <- file.path( packDir, "inst", "bintest", "soiln001.BIN" ) ) 

#   Using different settings
muPar( alphaNumOnly = FALSE, removeSpace = FALSE ) 

tmp3 <- macroReadBin( file = filenm ) 

colnames( tmp3 ) 

tools::showNonASCII(colnames(tmp3))

colnames(tmp3) <- gsub(x=colnames(tmp3),pattern="²", 
    replacement = "2",fixed=TRUE) 

funnyCharacter <- substr( colnames( tmp3 ), 27, 27 )[48]

funnyCharacter == " "
# [1] FALSE

colnames(tmp3) <- gsub(x=colnames(tmp3),pattern=funnyCharacter, 
    replacement = " ",fixed=TRUE) 

colnames(tmp3)

substr( colnames( tmp3 ), 27, 27 )[48] == " "

macroWriteBin( x = tmp3, file = filenm ) 

#   Also fix the reference RDS file
( rdsnm <- file.path( packDir, "inst", "bintest", "macro52convertedBin", 
    "soiln001.rds" ) ) 

rds <- readRDS( file = rdsnm )

colnames( rds ) 

colnames(rds) <- gsub(x=colnames(rds),pattern="²", replacement = "2",fixed=TRUE) 
colnames(rds) <- gsub(x=colnames(rds),pattern=funnyCharacter, 
    replacement = " ",fixed=TRUE) 

colnames(rds)

saveRDS( object = rds, file = rdsnm )
