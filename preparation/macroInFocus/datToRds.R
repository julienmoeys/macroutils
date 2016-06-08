
#   This script is used to convert text tabular files 
#   (themselves exported by MACROInFOCUS), into more compact 
#   RDS files, that can then be used to check that the 
#   differences between macroInFocusGWConc() and MACROInFOCUS 
#   are very small (when calculating processing results)

setwd( wd <- sprintf( 
    "%s/macro-se/macroutils/preparation/macroInFocus", 
    Sys.getenv("rPackagesDir") 
) ) 

outDir <- sprintf( 
    "%s/macro-se/macroutils/pkg/macroutils/inst/bintest/macroInFocus", 
    Sys.getenv("rPackagesDir") 
)   

allDat <- list.files( wd, pattern = ".dat", ignore.case = TRUE )

for( i in 1:length( allDat ) ){
    #   i <- 1L
    
    #   Format the name of the output RDS file
    rdsName <- allDat[ i ]
    rdsName <- gsub( x = rdsName, pattern = ".dat", 
        replacement = ".rds", ignore.case = TRUE )
    
    #   Import the txt file (of the converted bin files)
    inTbl <- read.table(
        file    = allDat[ i ], 
        header  = TRUE, 
        sep     = "", 
        dec     = ".", 
        strip.white = TRUE
    )   
    
    #   Re-export it as RDS file
    saveRDS( object = inTbl, file = file.path( outDir, rdsName ) )
    
    rm( inTbl, rdsName )
}   
