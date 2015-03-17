
### Converts MACRO internal dates into R POSIXct date-time format
.macroDate2POSIXct <- function( x, tz = "GMT" ){ 
    x <- as.integer( x ) 
    
    date.offsetX <- -48/24 
    
    x <- as.POSIXct( "0001/01/01 00:00:00", 
        tz = tz) + x * 60 + date.offsetX * 
        24 * 60 * 60
    
    x <- as.POSIXct( format( x = x, format = "%Y-%m-%d %H:%M:%S", 
        tz = tz), format = "%Y-%m-%d %H:%M:%S", tz = tz )
    
    return( x ) 
}   
#   .macroDate2POSIXct( c("1035596160","1049270399") )



# macroReadIndump ===============================================

#' INTERNAL. Import a MACRO indump.tmp file and output it in a human readable format.
#'
#' INTERNAL. Import a MACRO indump.tmp file and output it in a 
#'  human readable format. It reads layered parameters, options, 
#'  crop parameters and irrigation parameters, but not yet output 
#'  parameters. EXPERIMENTAL. USE AT YOUR OWN RISKS.
#'
#'@param f 
#'  Single character string. Name (and if needed, path) of the 
#'  indump.tmp file to be read
#'
#'@param layerLoc
#'  Single integer. Line where the number of numerical layers is 
#'  written
#'
#'@param exportTrash
#'  Single logical value. If TRUE, 'filling' parameter values (i.e. 
#'  values written but not used) are also exported.
#'
#'
#'@return 
#'  Returns a list of \code{\link[base]{data.frame}}s with different 
#'  MACRO parameters
#'
#'
#'@export
#'
#'@keywords internal
#'
macroReadIndump <- function(
 f, 
 layerLoc = 7, 
 exportTrash = FALSE
){   
    indump <- readLines( con = f ) 
    
    nlayer  <- as.integer( scan( text = indump[ layerLoc ], quiet = TRUE ) ) 
    
    # Find the beginning and end of the 1st variables array
    varLoc1 <- which( substr( indump, 1, 1 ) == "4" ) 
    varLoc1 <- varLoc1[ which( varLoc1 > layerLoc ) ][1]
    varLoc2 <- strsplit( x = indump[ varLoc1 ], split = " " )[[ 1 ]] 
    varLoc2 <- as.integer( varLoc2[ length( varLoc2 ) ] ) 
    varLoc <- (varLoc1+1):(ceiling( varLoc2 / 6 ) + varLoc1)
    rm( varLoc1, varLoc2 ) 
    
    # Read the 1st variable array
    val <- as.numeric(unlist( lapply( 
        X   = indump[ varLoc ], 
        FUN = function(X){ scan( text = X, what = "character", quiet = TRUE ) } ) ) ) 
    
    # Find the beginning and end of the 1st variables index vector
    indexLoc1 <- which( substr( indump, 1, 1 ) == "8" ) 
    indexLoc1 <- indexLoc1[ which( indexLoc1 > max(varLoc) )[1] ] 
    indexLoc2 <- strsplit( x = indump[ indexLoc1 ], split = " " )[[ 1 ]] 
    indexLoc2 <- as.integer( indexLoc2[ length( indexLoc2 ) ] ) 
    indexLoc <- (indexLoc1+1):(ceiling( indexLoc2 / 8 ) + indexLoc1)
    rm( indexLoc1, indexLoc2 ) 
    
    # Read the 1st variables index-array
    ind <- as.integer( unlist( lapply( 
        X   = indump[ indexLoc ], 
        FUN = function(X){ scan( text = X, what = "character", quiet = TRUE ) } ) ) ) 
    
    # Find the beginning and end of the 1st variables column vector
    colLoc1 <- which( substr( indump, 1, 1 ) == "7" ) 
    colLoc1 <- colLoc1[ which( colLoc1 > max(indexLoc) )[1] ] 
    colLoc2 <- strsplit( x = indump[ colLoc1 ], split = " " )[[ 1 ]] 
    colLoc2 <- as.integer( colLoc2[ length( colLoc2 ) ] ) 
    colLoc <- (colLoc1+1):(ceiling( colLoc2 / 8 ) + colLoc1)
    rm( colLoc1, colLoc2 ) 
    
    # Read the 1st variables index-array
    # colz <- unlist( lapply( 
        # X   = indump[ colLoc ], 
        # FUN = function(X){ scan( text = X, what = "character", quiet = TRUE ) } ) ) 
    tmp  <- tempfile() 
    writeLines( text = paste( indump[ colLoc ] ), con = tmp ) 
    library( "utils" )
    colz <- read.fwf( file = tmp, widths = rep(9,8), stringsAsFactors = FALSE ) 
    unlink( tmp ); rm( tmp ) 
    colz <- unlist( lapply( 
        X   = 1:nrow(colz),
        FUN = function(X){
            scan( 
                text  = paste( colz[X,], collapse = " " ),
                what  = "character", 
                quiet = TRUE 
            )
        }
    ) ) 
    colz <- colz[ !is.na( colz ) ] 
    
    # Bind the columns and their indexes
    colz <- data.frame( 
        "name"   = colz, 
        "start"  = ind[ -length( ind ) ], 
        "stop"   = ind[ -length( ind ) ] + diff( ind ) - 1, 
        "length" = diff( ind ), 
        stringsAsFactors = FALSE )
    
    # Empty variable matrix
    mat   <- matrix( data = length(0), nrow = nlayer, ncol = 0 )
    vr    <- numeric(0) 
    trash <- list() 
    
    for( r in 1:nrow( colz ) ){ 
        #   Locate the values 
        i <- colz[ r, "start" ] 
        j <- colz[ r, "stop" ]  
        
        #   Case: layered variable:
        if( colz[ r, "length" ] == nlayer ){ 
            #   Read the values into a matrix
            matTmp <- matrix( data = val[ i:j ], nrow = nlayer, 
                ncol = 1, byrow = FALSE ) 
            
            #   Name the column
            colnames( matTmp ) <- colz[ r, "name" ] 
            
            #   Bind to the existing data
            mat <- cbind( mat, matTmp ); rm( matTmp ) 
            
        }else{ 
        #   Case: non-layered variable
            varTmp <- val[ i:j ] 
            
            #   Case: not a single value (strange stuff)
            if( colz[ r, "length" ] != 1 ){ 
                #   Only keep the last value
                trash[[ colz[ r, "name" ] ]] <- varTmp[ -1 ] # -length( varTmp )
                varTmp <- varTmp[ 1 ] # length( varTmp )
            }   
            
            names( varTmp ) <- colz[ r, "name" ] 
            vr <- c( vr, varTmp ); rm( varTmp ) 
        }   
    }   
    
    
    
    # === === read the "options" === === 
    
    # Find the beginning and end of the 1st variables array
    varLoc <- which( substr( indump, 1, 17 ) ==  " 5             35" )[1] 
    if( length( varLoc ) == 0 ){ 
        stop( "Could not find the 'options' variables" )
    }   
    varLoc <- varLoc+1
    
    # Read the 1st variable array
    valO <- as.numeric(unlist( lapply( 
        X   = indump[ varLoc ], 
        FUN = function(X){ scan( text = X, what = "character", quiet = TRUE ) } ) ) ) 
    
    # Find the beginning and end of the 1st variables column vector
    colLoc1 <- which( substr( indump, 1, 17 ) ==  " 6             35" )[1] 
    if( length( colLoc1 ) == 0 ){ 
        stop( "Could not find the 'options' header" )
    }   
    colLoc2 <- strsplit( x = indump[ colLoc1 ], split = " " )[[ 1 ]] 
    colLoc2 <- as.integer( colLoc2[ length( colLoc2 ) ] ) 
    colLoc <- (colLoc1+1):(ceiling( colLoc2 / 8 ) + colLoc1)
    rm( colLoc1, colLoc2 ) 
    
    # Read the 1st variables index-array
    tmp  <- tempfile() 
    writeLines( text = paste( indump[ colLoc ] ), con = tmp ) 
    colO <- read.fwf( file = tmp, widths = rep(9,8), stringsAsFactors = FALSE ) 
    unlink( tmp ); rm( tmp ) 
    colO <- unlist( lapply( 
        X   = 1:nrow(colO),
        FUN = function(X){
            scan( 
                text  = paste( colO[X,], collapse = " " ),
                what  = "character", 
                quiet = TRUE 
            )
        }
    ) ) 
    colO <- colO[ !is.na( colO ) ] 
    
    names( valO ) <- colO 
    
    
    
    # === === read the start / stop dates === === 
    
    varLoc <- which( substr( indump, 1, 7 ) ==  "25    0" )[1] 
    if( length( varLoc ) == 0 ){ 
        stop( "Could not find the 'start / stop dates' variables" )
    }   
    varLoc <- varLoc+1
    
    # Read the 1st variable array
    valDates <- as.integer( unlist( lapply( 
        X   = indump[ varLoc ], 
        FUN = function(X){ scan( text = X, what = "character", quiet = TRUE ) } ) ) ) 
    
    valDates <- .macroDate2POSIXct( valDates[1:2] )
    
    
    
    # === === Find time-variable parameters === ===
    
    timePar <- list() 
    
    dateLoc  <- which( substr( indump, 1, 3 ) ==  "23 " ) 
    dateLoc2 <- c( dateLoc, length( indump ) + 1 ) 
    
    if( length( dateLoc ) == 0 ){ 
        warning( "Could not find time-variable parameters" )
    }else{ 
        varLoc2a <- which( substr( indump, 1, 4 ) ==  "101 " ) 
        varLoc2b <- which( substr( indump, 1, 4 ) ==  "102 " ) 
            
        for( timeI in 1:length( dateLoc ) ){ 
            #   Find and convert the date-time
            dateTime <- as.integer( scan( text = indump[ dateLoc[ timeI ]+1 ], 
                what = "character", quiet = TRUE ) ) 
            dateTime <- .macroDate2POSIXct( dateTime )
            
            # #   Locate and read the index and values array
            # if( timeI != length( dateLoc ) ){ 
                # timeIPlusOne <- timeI+1 
            # }else{ 
                # timeIPlusOne <- length( indump ) + 1 
            # }   
            
            testTime <- (varLoc2a > dateLoc[ timeI ]) & 
                (varLoc2a < dateLoc2[ timeI+1 ])
            indexLoc1 <- varLoc2a[ which( testTime )[ 1 ] ]
            indexLoc2 <- strsplit( x = indump[ indexLoc1 ], split = " " )[[ 1 ]] 
            indexLoc2 <- as.integer( indexLoc2[ length( indexLoc2 ) ] ) 
            indexLoc <- (indexLoc1+1):(ceiling( indexLoc2 / 10 ) + indexLoc1)
            rm( indexLoc1, indexLoc2 ) 
            
            # Read the index-array
            ind <- as.integer( unlist( lapply( 
                X   = indump[ indexLoc ], 
                FUN = function(X){ scan( text = X, what = "character", quiet = TRUE ) } ) ) ) 
            
            testTime <- (varLoc2b > dateLoc[ timeI ]) & 
                (varLoc2b < dateLoc2[ timeI+1 ])
            varLoc1   <- varLoc2b[ which( testTime )[ 1 ] ] 
            varLoc3 <- strsplit( x = indump[ varLoc1 ], split = " " )[[ 1 ]] 
            varLoc3 <- as.integer( varLoc3[ length( varLoc3 ) ] ) 
            varLoc <- (varLoc1+1):(ceiling( varLoc3 / 10 ) + varLoc1)
            rm( varLoc1, varLoc3 ) 
            
            # Read the variable array
            valDate <- as.numeric( unlist( lapply( 
                X   = indump[ varLoc ], 
                FUN = function(X){ scan( text = X, what = "character", quiet = TRUE ) } ) ) ) 
            
            
            colz2 <- lapply( 
                X   = ind, 
                FUN = function(X){ 
                    testCol <- (colz[,"start"] <= X) & 
                        (colz[,"stop" ] >= X) 
                    testCol <- which( testCol ) 
                    
                    if( length(testCol) == 0 ){ 
                        stop( "Can't find index of time-variable parameter" )
                    }   
                    
                    return( colz[ testCol[1], ] ) 
                }   
            )   
            colz2 <- unique( do.call( "rbind", colz2 ) ) 
            
            # testCol <- which( colz[,"start"] %in% ind )
            # colz2 <- data.frame( 
                # "name"   = colz[ testCol, "name" ], 
                # "start"  = ind, 
                # "length" = colz[ testCol, "length" ], 
                # stringsAsFactors = FALSE 
            # )   
            
            #   Prepare reading the new values:
            mat2   <- matrix( data = length(0), nrow = nlayer, ncol = 0 )
            vr2    <- numeric(0) 
            trash2 <- list() 
            j <- 0 
            
            # if( timeI == 2 ){ browser() }
            
            #   Read the new values
            for( r in 1:nrow( colz2 ) ){ 
                # Locate the values 
                # i <- colz2[ r, "start" ] 
                i <- j+1
                j <- i + colz2[ r, "length" ] - 1 
                
                #   Case: layered variable:
                if( colz2[ r, "length" ] == nlayer ){ 
                    #   Read the values into a matrix
                    matTmp <- matrix( data = valDate[ i:j ], nrow = nlayer, 
                        ncol = 1, byrow = FALSE ) 
                    
                    #   Name the column
                    colnames( matTmp ) <- colz2[ r, "name" ] 
                    
                    #   Bind to the existing data
                    mat2 <- cbind( mat2, matTmp ); rm( matTmp ) 
                    
                }else{ 
                #   Case: non-layered variable
                    varTmp <- valDate[ i:j ] 
                    
                    #   Case: not a single value (strange stuff)
                    if( colz2[ r, "length" ] != 1 ){ 
                        #   Only keep the last value
                        trash2[[ colz2[ r, "name" ] ]] <- varTmp[ -1 ] 
                        varTmp <- varTmp[ 1 ] 
                    }   
                    
                    names( varTmp ) <- colz2[ r, "name" ] 
                    vr2 <- c( vr2, varTmp ); rm( varTmp ) 
                }   
            }   
            
            if( ncol(mat2) == 0 ){ mat2 <- NULL } 
            
            if( !exportTrash ){ trash2 <- list() } 
            
            timePar[[ length(timePar)+1 ]] <- list( 
                "date"    = dateTime, 
                "trash"   = trash2, 
                "mat"     = mat2, 
                "var"     = vr2 
            )   
            
        }   
        
    }   
    
    
    
    # === === Tag time variable parameters === === 
    
    cropCol <- c( "ROOTINIT", "ROOTMAX", "ROOTDEP", "CFORM", 
        "RPIN", "WATEN", "CRITAIR", "BETA", "CANCAP", "ZALP", 
        "IDSTART", "IDMAX", "IHARV", "ZHMIN", "LAIMIN", "LAIMAX", 
        "ZDATEMIN", "DFORM", "LAIHAR", "HMAX", "RSMIN", "ATTEN" ) 
    
    irrCol <- c("IRRDAY", "AMIR", "IRRSTART", "IRREND", "ZFINT", 
        "CONCI", "NIRR" )
    
    type <- unlist( lapply( 
        X   = timePar, 
        FUN = function(X){ 
            type <- "other" 
            nm   <- names( X[["var"]] ) 
            
            #   Test if crops
            selCol <- cropCol[ cropCol %in% nm ]
            
            if( length(selCol) != 0 ){ 
                typeCrop <- TRUE 
            }else{ typeCrop <- FALSE }
            
            #   Test if irrigation
            selCol <- irrCol[ irrCol %in% nm ]
            
            if( length(selCol) != 0 ){ 
                typeIrr <- TRUE 
            }else{ typeIrr <- FALSE }
            
            if( typeCrop & typeIrr ){ 
                warning( "Both irrigation and crop parameters mixed in time-variable parameters" ) }
            
            if( typeCrop ){ type <- "crop" } 
            if( typeIrr){ type <- "irr" } 
            
            return( type ) 
        }   
    ) ) 
    
    
    # === === Prepare crop parameters === === 
    
    #   Separate the crop parameters from the rest
    
    crop    <- vr[ cropCol ] 
    crop    <- t( as.matrix( crop ) ) 
    colnames(crop) <- cropCol  
    
    vr      <- vr[ !(names(vr) %in% cropCol) ] 
    
    #   Add a date column
    crop    <- data.frame( 
        "DATE"  = valDates[1], 
        "DOY"   = as.integer( format( valDates[1], format = "%j" ) ), 
        crop 
    )   
    
    cropLater <- matrix( data = NA_real_, nrow = 1, 
        ncol = length(cropCol) ) 
    colnames( cropLater ) <- cropCol 
    cropLater <- data.frame( 
        "DATE"  = as.POSIXct( NA ), 
        "DOY"   = as.integer( NA ), 
        cropLater 
    )   
    
    testTimeCrop <- which( type == "crop" ) 
    
    if( length(testTimeCrop) != 0 ){ 
        cropLater <- lapply( 
            X   = timePar[ testTimeCrop ], 
            FUN = function(X){ 
                selCol <- cropCol[ cropCol %in% names( X[["var"]] ) ]
                
                if( length(selCol) != 0 ){ 
                    cropLater[ 1, selCol ] <- X[["var"]][ selCol ] 
                    
                    if( length(selCol) != length( cropCol ) ){ 
                        warning( "Some time variable crop parameters not found in initial parameters" ) 
                    }   
                    
                    cropLater[, "DATE" ] <- X[["date"]] 
                    cropLater[, "DOY" ]  <- as.integer( format( X[["date"]], format = "%j" ) ) 
                }else{ 
                    #   Return an empty data.frame
                    cropLater <- cropLater[ logical(0), ] 
                }   
                
                return( cropLater ) 
            }   
        )   
        cropLater <- do.call( "rbind", cropLater )
        
        crop <- rbind( crop, cropLater ); rm( cropLater ) 
    }   
    
    # === === Prepare Irrigation parameters === === 
    
    #   Separate the irrigation parameters from the rest
    
    irr    <- vr[ irrCol ] 
    irr    <- t( as.matrix( irr ) ) 
    colnames(irr) <- irrCol  
    
    vr      <- vr[ !(names(vr) %in% irrCol) ] 
    
    #   Add a date column
    irr    <- data.frame( 
        "DATE"  = valDates[1], 
        "DOY"   = as.integer( format( valDates[1], format = "%j" ) ), 
        irr 
    )   
    
    irrLater <- matrix( data = NA_real_, nrow = 1, 
        ncol = length(irrCol) ) 
    colnames( irrLater ) <- irrCol 
    irrLater <- data.frame( 
        "DATE"  = as.POSIXct( NA ), 
        "DOY"   = as.integer( NA ), 
        irrLater 
    )   
    
    testTimeIrr <- which( type == "irr" ) 
    
    if( length(testTimeIrr) != 0 ){ 
        irrLater <- lapply( 
            X   = timePar[ testTimeIrr ], 
            FUN = function(X){ 
                selCol <- irrCol[ irrCol %in% names( X[["var"]] ) ]
                
                if( length(selCol) != 0 ){ 
                    irrLater[ 1, selCol ] <- X[["var"]][ selCol ] 
                    
                    if( length(selCol) != length( irrCol ) ){ 
                        warning( "Some time variable irrigation parameters not found in initial parameters" ) 
                    }   
                    
                    irrLater[, "DATE" ] <- X[["date"]] 
                    irrLater[, "DOY" ]  <- as.integer( format( X[["date"]], format = "%j" ) ) 
                }else{ 
                    #   Return an empty data.frame
                    irrLater <- irrLater[ logical(0), ] 
                }   
                
                return( irrLater ) 
            }   
        )   
        irrLater <- do.call( "rbind", irrLater )
        
        irr <- rbind( irr, irrLater ); rm( irrLater ) 
    }   
    
    
    # === === Final export === === 
    
    #   Keep only non crop and non irrigation parameters in 
    #   list of time variable parameters
    timePar <- timePar[ which( !(type %in% c("crop","irr")) ) ] 
    
    if( !exportTrash ){ trash <- list() } 
    
    out <- list( 
        "trash"     = trash, 
        "mat"       = mat, 
        "var"       = vr, 
        "options"   = valO, 
        "crop"      = crop, 
        "irrig"     = irr, 
        "dateRange" = valDates, 
        "timePar"   = timePar ) 
    
    return( out )
}   

