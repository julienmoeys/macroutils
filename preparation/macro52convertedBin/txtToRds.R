
#   This script is used to convert text tabular files 
#   (themselves converted from MACRO or SOILN bib files 
#   using MACRO 5.2 GUI), into more compact RDS files, 
#   that can then be used to check that the differences 
#   between macroReadBin() and MACRO are very small (when 
#   converting bin files)

setwd( wd <- "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/macro-sp/macroutils/preparation/macro52convertedBin" )

outDir <- "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/macro-sp/macroutils/pkg/macroutils/inst/bintest/macro52convertedBin"

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
