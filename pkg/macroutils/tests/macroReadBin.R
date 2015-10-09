
library( "macroutils" )

#   Maximum acceptable relative difference (relative to the 
#   mean)
maxRelDiff <- 0.0015 # 0.15%

#   Folder in which the example bin files are stored
binFolder <- system.file( "bintest", package = "macroutils", 
    mustWork = TRUE ) 

#   List all the files in binFolder
f <- list.files( path = binFolder, full.names = FALSE ) 

#   Select only bin files (notice that pattern argument 
#   in list.files() also selects a folder, so we 
#   can't use it here, and use grepl instead)
f <- f[ grepl( x = tolower( f ), pattern = ".bin", 
    fixed = TRUE ) ]

#   Excluding a file that raise a warning 
#   (irregular time series)
f <- f[ f != "Fert.bin" ]

#   Folder containing all the RDS-files that contain the 
#   bin files as converted with MACRO 5.2 GUI
rdsFolder <- system.file( "bintest/macro52convertedBin", 
    package = "macroutils", mustWork = TRUE ) 

#   Format the name of the expected RDS files
fRds <- gsub( x = f, pattern = ".bin", replacement = ".rds", 
    ignore.case = TRUE )

#   Test that these files exists
testFRds <- file.exists( x = file.path( rdsFolder, fRds ) )

if( !all( testFRds ) ){
    stop( sprintf( 
        "Some expected files are missing (%s) in folder %s", 
        paste( fRds[ !testFRds ], collapse = "; " ), 
        rdsFolder 
    ) ) 
}   

#   Compare files one by one
for( i in 1:length( f ) ){
    #   i <- 2L
    
    #   Import the bin files
    binImport <- macroReadBin( file = file.path( binFolder, f[ i ] ) ) 
    
    #   Read the corresponding RDS file
    rdsImport <- readRDS( file = file.path( rdsFolder, fRds[ i ] ) )
    
    #   Remove possible spurious columns
    if( "X" %in% colnames( rdsImport ) ){
        rdsImport <- rdsImport[, colnames( rdsImport ) != "X" ]
    }   
    
    #   Convert the Date column
    rdsImport[, "Date" ] <- as.character( rdsImport[, "Date" ] ) 
    rdsImport[, "Date" ] <- as.POSIXct( x = rdsImport[, "Date" ], 
        format = "%Y%m%d%H%M", tz = "GMT" ) 
    
    dim( binImport ) 
    dim( rdsImport ) 
    
    #   Calculate the difference between the two tables
    diffTable <- abs( binImport[, -1L, drop = FALSE ] - rdsImport[, -1L, drop = FALSE ] )
    
    #   Summary by columns (maximum difference)
    diffTable <- apply( X = diffTable, MARGIN = 2, FUN = max ) 
    
    #   Find out the mean value of each variable (after MACRO 
    #   5.2) (min would cause problem, max would be not so worst 
    #   case)
    meanTable <- apply( X = rdsImport[, -1L, drop = FALSE ], MARGIN = 2, FUN = mean ) 
    
    #   Find out the relative difference, excepts for 
    #   variables where the mean value is 0
    relDiffTable <- diffTable 
    relDiffTable[ meanTable != 0 ] <- 
        relDiffTable[ meanTable != 0 ] / meanTable[ meanTable != 0 ]
    
    #   Test that the differences are not too big
    if( any( relDiffTable > maxRelDiff ) ){
        stop( sprintf( 
            "Some differences bigger than the maximum acceptable difference: %s > %s (file: %s)", 
            max( relDiffTable ), maxRelDiff, f[ i ] 
        ) ) 
    }   
    
}   
