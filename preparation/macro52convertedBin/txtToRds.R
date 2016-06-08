
#   This script is used to convert text tabular files 
#   (themselves converted from MACRO or SOILN bib files 
#   using MACRO 5.2 GUI), into more compact RDS files, 
#   that can then be used to check that the differences 
#   between macroReadBin() and MACRO are very small (when 
#   converting bin files)

setwd( wd <- sprintf( 
    "%s/macro-se/macroutils/preparation/macro52convertedBin", 
    Sys.getenv("rPackagesDir") 
) ) 

outDir <- sprintf( 
    "%s/macro-se/macroutils/pkg/macroutils/inst/bintest/macro52convertedBin", 
    Sys.getenv("rPackagesDir") 
)   

allTxt <- list.files( wd, pattern = ".txt", ignore.case = TRUE )

for( i in 1:length( allTxt ) ){
    #   i <- 1L
    
    #   Format the name of the output RDS file
    rdsName <- allTxt[ i ]
    rdsName <- gsub( x = rdsName, pattern = ".txt", 
        replacement = ".rds", ignore.case = TRUE )
    
    #   Import the txt file (of the converted bin files)
    inTbl <- read.table(
        file    = allTxt[ i ], 
        header  = TRUE, 
        sep     = "\t", 
        dec     = ".", 
        strip.white = TRUE
    )   
    
    #   Re-export it as RDS file
    saveRDS( object = inTbl, file = file.path( outDir, rdsName ) )
    
    rm( inTbl, rdsName )
}   
