
# +-------------------------------------------------------------+ 
# | Title:      Utilities to read, write and plot MACRO bin     |
# |             files                                           | 
# | Author:     Julien MOEYS -- SLU/CKB                         | 
# | Language:   R                                               | 
# | Contact:    Julien.Moeys@slu.se                             | 
# | License:    AGPL3 (GNU AFFERO GENERAL PUBLIC LICENSE v 3)   | 
# +-------------------------------------------------------------+ 



# Template doc

# ==================== .functionName ====================

## # Title
## #
## # Description
## #
## #
## #@param 
## #    
## # 
## #@param 
## #    
## # 
## #@return
## #    Bla bla
## #
## #



# ==================== isValidTimeSeries ===================

#' Test that Date or POSIXct date-time are unique, sorted and regular.
#'
#'@description
#'  Test that Date or POSIXct date-time are unique, sorted and 
#'  regular.
#'
#'
#'@param x
#'  A vector of \code{\link{Date}}, or of \code{\link{POSIXct}} 
#'  date-times
#'
#'@param units
#'  Passed to \code{\link{as.numeric}}-\code{difftime}. Only 
#'  used in case irregularities in the time series are 
#'  detected.
#'
#'@param onError
#'  A valid R function, such as \code{warning} or \code{stop}, 
#'  or \code{message}. Function that will be used to output 
#'  an error message if the time series is not unique, sorted 
#'  and regular.
#'
#'
#'@return 
#'  Returns \code{FALSE} if a problem was detected, and 
#'  \code{TRUE} otherwise (unless \code{onError} is \code{stop}, 
#'  in which case an error is send and the function stops).
#'
#'
#'@example inst/examples/isValidTimeSeries-examples.R
#'
#'@export
#'
isValidTimeSeries <- function( 
    x,      # Date-format or POSIXct-format
    units   = "hours", 
    onError = warning 
){  
    isValid <- TRUE
    
    #   Find if all dates are unique
    if( any( dup <- duplicated( x ) ) ){
        onError( sprintf(
            "Some climate date(s)-time(s) are duplicated. First case: %s. Please check. See also option 'timeSeriesValid' in muPar()", 
            x[ dup ][ 1L ]
        ) ) 
        
        isValid <- FALSE
    };  rm( dup )
    
    #   Find if all dates are sorted
    if( any( sort( x ) != x ) ){
        onError( "Some date(s)-time(s) seems to be unsorted. Please check. See also option 'timeSeriesValid' in muPar()" ) 
        
        isValid <- FALSE
    }   
    
    #   Find if time increment is homogeneous
    # udiff <- unique( diff( x ) )
    udiff <- unique( difftime( x[ -length(x) ], x[ -1 ], units = units ) )
    
    if( length( udiff ) > 1L ){
        udiff <- as.numeric( udiff, units = units ) 
        
        u <- substr( units, 1, 1 )
        
        onError( sprintf( 
            "The time interval between date(s)-time(s) vary. First two time differences: %s %s, %s %s. Please check. See also option 'timeSeriesValid' in muPar()", 
            udiff[ 1L ], u, udiff[ 2L ], u
        ) )  
        
        isValid <- FALSE
    }   
    
    return( isValid ) 
}   



# ==================== .macroReadBin ====================

## # Read bin file from the Soil and MACRO models.
## #
## # Read bin file from the Soil and MACRO models. Adapted from 
## #  an "anonymous" SLU origial code by Kristian Persson. R code 
## #  vectorisation by Julien Moeys.
## #  
## #  Many global arguments can be set-up and retrieved via 
## #  \code{\link{muPar}} and \code{\link{getMuPar}}. 
## #  Please check the help page of these functions if you need to 
## #  tune \code{macroReadBin}.
## #
## #
## #@seealso \code{\link[base]{readBin}}.
## #
## #
## #@param file 
## #  Single character string or connection to a binary file. If 
## #  a character string, it should be the name of the binary file 
## #  which the data are to be read from. The path of the file may 
## #  be provided as well, if file is not in the working directory.
## #
## #@param \dots 
## #  Additional options passed to \code{\link[base]{readBin}}.
## #
## #
## #@return 
## #  Returns a data.frame with the content of the bin file. Columns 
## #  names found in the bin file are returned as well. The "Date" 
## #  column in the bin file is converted from "Julian Date" into 
## #  POSIXct date format.
## #
## #
.macroReadBin <- function(
 file,
 ...
){  # Reads an integer (4 byte) containting the numner of records 
    # and the length of each record
    record.number <- readBin( 
        con  = file, 
        what = "int",
        n    = 2, 
        size = 4, 
        ... 
    )   
    
    record.length <- record.number[ 2 ] # Width of a row in bytes
    record.number <- record.number[ 1 ] # Number of rows
    
    # Number of variables in the file
    variables.number <- record.length / 4 # - 1
    
    if( getMuPar( "header" ) ){ 
        colLength <- (variables.number-1) * ( 52 + 4 + 4 ) 
    }else{ 
        colLength <- 0 
    }   
    
    # Read all the bin file at once:
    # - Calculate its total length 
    total.length <- 
        record.length +                       # File 'info' record
        record.number * record.length +       # Table of data 
        colLength                             # Column names (excl. date)
    
    # - Read the file
    binData <- readBin( 
        con  = file, 
        what = "raw",
        n    = total.length, 
        #size=NA, 
        ... 
    )   
    
    
    # Create a matrix to store the data
    data.values <- matrix( 
        nrow = record.number, 
        ncol = variables.number
    )   #
    
    sel.vec.lst <- mapply( 
        FUN  = seq, 
        from = record.length * (1:record.number) + 1, 
        to   = record.length * (2:(record.number+1)), 
        SIMPLIFY = FALSE 
    )   
    
    data.values <- do.call( 
        "what" = rbind, 
        args   = lapply( 
            X   = sel.vec.lst, # X is row.nr 
            FUN = function(X){ 
                c( 
                    as.double( 
                        readBin( 
                            con  = binData[ X[1:4] ], 
                            what = "int", 
                            size = 4, 
                            n    = 1 
                        )   
                    ),  
                    readBin( 
                        con  = binData[ X[5:length(X)] ], 
                        what = "double", 
                        size = 4, 
                        n    = variables.number - 1 
                    )   
                )   
            }   
        )   
    )   
    
    # Read in column names
    if( getMuPar( "header" ) ){ 
        col.Names <- rep( x = as.character(NA), times = variables.number ) 
        
        col.Names[1] <- "Date" 
        
        sel.vec.lst2 <- mapply( 
            FUN  = seq, 
            from = record.length * (record.number+1) + 1 + (0:(variables.number-2))*60, 
            to   = record.length * (record.number+1) + (1:(variables.number-1))*60, 
            SIMPLIFY = FALSE 
        )   #
        
        col.Names[ 2:variables.number ] <- unlist( lapply( 
            X   = 1:length(sel.vec.lst2), 
            FUN = function(X){ 
                readChar( 
                    con    = binData[ sel.vec.lst2[[ X ]] ], 
                    nchars = 52  
                )   
            }   
        ) ) 
        
        
        # Remove trailing blanks
        col.Names <- sub( 
            pattern     = "[ ]*$", 
            replacement = "", 
            x           = col.Names  
        )   
        
        if( getMuPar( "removeSpace" ) ) # Replacing spaces by underscores
        {   
            col.Names <- gsub( pattern = "-", replacement = " ", x = col.Names ) 
            
            col.Names  <- strsplit(
                x        = col.Names, 
                split    = " ", 
                fixed    = FALSE, 
                perl     = FALSE
            )   
            
            col.Names  <- unlist( lapply( 
                    X   = col.Names, 
                    FUN = function(X){ 
                        paste( X[ X != "" ], collapse = "_" ) 
                    }   
            )   )   
        }   
        
        if( getMuPar( "alphaNumOnly" ) ) 
        {   
            col.Names <- strsplit( x = col.Names, split = "" ) 
            
            col.Names <- unlist( lapply( 
                X   = col.Names, 
                FUN = function(X){ 
                    sel <- X %in% getMuPar( "alphaNum" ) 
                    
                    return( paste( X[ sel ], collapse = "" ) ) 
                }   
            ) ) 
        }   
        
        colnames( data.values ) <- col.Names 
        
        data.values <- as.data.frame( data.values ) 
        
        if( getMuPar( "stripRunID" ) & getMuPar( "removeSpace" ) ){ 
            colnames( data.values ) <- macroStripRunID( 
                x         = colnames( data.values ), 
                splitChar = "_"
            )   
        }   
    }else{ 
        data.values <- as.data.frame( data.values ) 
        
        colnames( data.values )[ 1 ] <- "Date" 
    }   
    
    
    if( getMuPar("dateMethod") == 1 )
    {   
        date.offsetX <- -48/24
        tz           <- getMuPar( "tz" ) 
        
        # Method 1: Add the date converted in seconds + an offset 
        # of -2 days 
        data.values[,"Date"] <- as.POSIXct( 
            "0001/01/01 00:00:00",
            tz = tz 
        ) + data.values[,"Date"]*60 + date.offsetX*24*60*60  # Add the date
        
        data.values[,"Date"] <- as.POSIXct( 
            format( 
                x      = data.values[, "Date" ], 
                format = "%Y-%m-%d %H:%M:%S", 
                tz     = tz 
            ),  #
            format = "%Y-%m-%d %H:%M:%S", 
            tz = tz 
        )   #
        
        # cat( "1. class(Date2): ", class(Date2), "\n" ) 
        
        # data.values[,"Date"] <- Date2 
    }   
    #
    # res  <- as.POSIXct( "0001/01/01 00:00:00", tz=getMuPar( "tz" ) ) + 20000001 
    # res2 <- as.POSIXct( format( 
    #             x      = res, 
    #             format = "%Y-%m-%d %H:%M:%S", 
    #             tz     = "" 
    #         ),  #
    #         format = "%Y-%m-%d %H:%M:%S", 
    #         tz = "" 
    # )   #
    #
    if( getMuPar("dateMethod") == 2 )
    {   #
        data.values[,"Date"] <- unlist( lapply( 
            X   = 1:record.number, 
            FUN = function(X){ 
                jul<-data.values[X,"Date"]    # minutes
                mi <- jul %% (60 * 24)          # residual minutes
                ho <- mi / 60                   # hours 
                mi <- mi - ho * 60  # This bit is weird mi = 0
                jul <- (jul - mi - ho * 60) / 60 / 24 + 1721424
                ja <- jul
                if(jul >= 2299161)
                {
                    jalpha <- ((jul - 1867216) - 0.25) / 36524.25
                    jalpha <- floor(jalpha)
                    ja = jul + 1 + jalpha - floor(jalpha * 0.25)
                }
                jb <- ja + 1524
                jc <- floor(6680.0 + ((jb - 2439870) - 122.1) / 365.25)
                jd <- 365 * jc + floor(0.25 * jc)
                je <- floor((jb - jd) / 30.6001)
                da <- jb - jd - floor(30.6001 * je)
                mon <- je - 1
                if (mon > 12) 
                {
                    mon <- mon - 12
                }
                yea <- jc - 4715
                if (mon > 2)
                {
                    yea = yea - 1
                }
                if (yea <= 0)
                {
                     yea = yea - 1
                }
                dateStr <- paste(yea,"-",mon,"-", da, " ", ho, ":" , mi, sep="") #make a date string
                #
                return( dateStr ) 
            }   #
        ) ) #
         
        #Transform text format date to POSIXct
        data.values[,"Date"] <- as.POSIXct( 
            data.values[,"Date"],      #vektor att transformera
            format = "%Y-%m-%d %H:%M", #Description of current date string to convert
            tz     = getMuPar( "tz" )  
        )   
        
        # cat( "1. class(Date2): ", class(Date2), "\n" )  
        
        # data.values[,"Date"] <- Date2 
    }   
    
    # cat( "2. class(data.values[,'Date']): ", class(data.values[,'Date']), "\n" ) 
    
    #Assembly the data.frame
    #data.complete <- data.frame(data.date, data.values)
    #Set the column name for the date column 
    #colnames(data.complete)[1] = "Date"
    
    
    #   Control that the date-time series is valid
    .isValidTimeSeries <- getMuPar( "timeSeriesValid" ) 
    
    if( !is.null( .isValidTimeSeries ) ){
        .isValidTimeSeries( data.values[,"Date"] ) 
    }   
    
    #Return the data.frame
    return( data.values )
}   




# ==================== .SMU.stripName ====================

# .SMU.stripName <- function(# Strip the name from a file name:

 # file  
# ### Vector of character strings

# ){  ## Split the file path with known file separators
    # file <- gsub( pattern = "//", replacement = "/", x = file, fixed = TRUE ) 
    # file <- gsub( pattern = "\\", replacement = "/", x = file, fixed = TRUE ) 
    
    # file <- strsplit( 
        # x     = file, 
        # split = "/", 
        # fixed = TRUE 
    # )   
    
    # ## Keep the last element of each path
    # file <- unlist( lapply( 
        # X   = file, 
        # FUN = function(X){ 
            # # X <- X[ X != "" ] 
            
            # if( length(X) > 1 ){ 
                # X <- X[ -length( X ) ] 
            # }   
            
            # X <- paste( X, collapse = "/" ) 
            
            # return( X ) 
        # }   
    # ) ) 
    
    # return( file ) 
# }   




# ==================== .chooseBinFiles ====================

#'@importFrom tcltk tk_choose.files

## # Pop-up a menu to choose bin file from the file system.
## # 
## # Pop-up a menu to choose bin file from the file system.
## #
## #
## #@param caption
## #   See \code{\link[utils]{choose.files}} or 
## #   \code{\link[tcltk]{tk_choose.files}}.
## # 
## #@param multi
## #   See \code{\link[utils]{choose.files}} or 
## #   \code{\link[tcltk]{tk_choose.files}}.
## # 
## # 
.chooseBinFiles <- function(
    caption = "Select one or several binary file(s)", 
    multi   = TRUE
){  
    if( !interactive() ){ 
        stop( "'.chooseBinFiles' can only be used in interactive mode" )
    }   
    
    
    ## Set the folder working directory
    lastBinWd <- getMuPar( "lastBinWd" ) 
    
    if( length(lastBinWd) == 0 ){ 
        lastBinWd <- getwd() 
    }else{ 
        if( lastBinWd == "" ){ 
            lastBinWd <- getwd() 
        }else{ 
            lastBinWd <- file.path( lastBinWd, "*.*" )
        }   
    }   
    
    
    ## Create a template of file extension to be read:
    filterz <- matrix( 
        data  = c( 
            "Binary files (*.bin)", "*.bin", 
            "All",                  "*" ), 
        nrow  = 2, 
        ncol  = 2, 
        byrow = TRUE  
    )   
    rownames( filterz ) <- c( "bin", "all" ) 
    
    ## Pop-up a menu to choose the bin file to be 
    ## imported
    if( exists(x = "choose.files", where = "package:utils" ) ){ 
        # fun <- get( "choose.files" ) 
        
        file <- utils::choose.files(
            default = lastBinWd, # , "*.bin"
            caption = caption, 
            multi   = multi, 
            filters = filterz 
        )   
        
    }else{ 
        # library( "tcltk" ) 
        
        # fun <- get( "tk_choose.files" ) 
        
        file <-tcltk::tk_choose.files(
            default = lastBinWd, # , "*.bin"
            caption = caption, 
            multi   = multi, 
            filters = filterz 
        )   
    }   
    
    
    # browser()
    
    
    ## Set the last folder where binary files were found:
    lastBinWd <- .pathNoLastItem( p = file[1] ) 
    
    muPar( "lastBinWd" = lastBinWd ) 
    
    return( file ) 
}   




# ==================== .macroMenu ====================

## # Wrapper around 'menu' with error handling
## #
## # Wrapper around 'menu' with error handling
## #
## #
## #@param title
## #    See \code{\link[utils]{select.list}}
## # 
## #@param choices
## #    See \code{\link[utils]{select.list}}
## # 
## #@param graphics
## #    See \code{\link[utils]{select.list}}
## # 
## #@param preselect
## #    See \code{\link[utils]{select.list}}
## # 
## #@param error
## #    Single character string. Error message to be displayed if 
## #    the user does not chose any item (code 0).
## # 
## #@param multi
## #    Single logical. If \code{TRUE}, then multiple choices are 
## #    allowed.
## # 
## # 
## #@return
## #    The user's choice.
## #
## #
#'@importFrom utils select.list
.macroMenu <- function(
    title = NULL, 
    choices, 
    graphics = FALSE, 
    preselect = NULL, 
    error = "You haven't chosen anything :o(", 
    multi = FALSE
){  ## Ask the user some choice
    # mRes <- menu( 
    #     choices  = choices, 
    #     graphics = graphics, 
    #     title    = title
    # )   
    
    choicesNum <- 1:length(choices) 
    names( choicesNum ) <- choices 
    
    mRes <- utils::select.list( 
        title       = title,
        choices     = choices, 
        preselect   = preselect, 
        multiple    = multi, 
        graphics    = graphics 
    )   
    
    ## Error handling:
    if( length(mRes) == 0 ){ 
        stop( error ) 
    }   
    
    mRes <- choicesNum[ mRes ] 
    names( mRes ) <- NULL 
    
    if( any( is.na( mRes ) ) ){ 
        stop( "Wrong value(s) chosen" )
    }   
    
    return( mRes ) 
}   




# ==================== macroReadBin ====================

#' Read bin file from the Soil and MACRO models.
#'
#' Read bin file from the Soil and MACRO models. Adapted from an "anonymous" SLU
#'  origial code by Kristian Persson. R code vectorisation by Julien Moeys.
#'
#' Many global arguments can be set-up and retrieved via \code{\link{muPar}}
#'  and \code{\link{getMuPar}}.  Please check the help page of these functions
#'  if you need to tune \code{macroReadBin}.
#'
#'@param file 
#'  Vector of character strings or a single \code{\link{connection}}
#'  to a binary file. If a vector character strings, it should be the name(s) of
#'  the binary file(s) which the data are to be read from. The path of the
#'  file(s) may be provided as well, if file(s) is (are) not in the working
#'  directory.
#'
#'@param \dots Additional options passed to specific 
#'  methods and to \code{\link[base]{readBin}}
#'
#'
#'@return 
#'  Returns a \code{data.frame} with the content of the bin file. 
#'  If \code{length(file) > 1}, then a \code{list} of \code{data.frame} 
#'  is returned instead. The \code{Date} column in the bin file is 
#'  converted from "Julian Date" into \code{\link[base]{POSIXct}} 
#'  date format.
#'
#'
#'@seealso \code{\link[base]{readBin}}.
#'
#'
#'@example inst/examples/macroReadBin.R
#'
#'@rdname macroReadBin-methods
#'
#'@export
#'
#'
macroReadBin <- function(
 file, 
 ...
){  
    if( missing( file ) ){ 
        UseMethod( "macroReadBin", object = character(0) )
    }else{ 
        UseMethod( "macroReadBin" )
    }   
}   



#'@rdname macroReadBin-methods
#'
#'@method macroReadBin character
#'@export 
macroReadBin.character <- function(
    file,
    ...
){  ## If no file name is provided
    if( missing( file ) ){ 
        if( interactive() ){ 
            ## Pop-up a menu to choose the bin file to be 
            ## imported
            file <- .chooseBinFiles(
                caption = "Select one or several binary file(s)", 
                multi   = TRUE  
            )   
            
            if( length(file) == 0 ){ 
                stop( "You haven't choosen any binary file to read :o(" )
            }   
            
            file <- sort( file ) 
        }else{ 
            stop( "'file' can not be missing when R is not being used interactively" ) 
        }   
    }   
    
    
    bin <- lapply( 
        X   = 1:length( file ), 
        FUN = function(i){ 
            bin <- .macroReadBin( file = file[ i ], ... ) 
            
            # bin <- cbind( 
                # bin, 
                # "index" = i 
            # )   
            
            class( bin ) <- c( "macroTimeSeries", "data.frame" )
            
            attr( x = bin, which = "file" ) <- file[ i ] 
            
            return( bin ) 
        }   
    )   
    
    
    ## Add the file name to each table:
    if( length( bin ) > 1 ){ 
        # file <- .pathLastItem( p = file, noExt = TRUE ) 
        
        # bin <- do.call( what = "rbind", args = bin )
        
        # names( bin ) <- file 
        
        class( bin ) <- c( "macroTimeSeriesList", "list" ) 
        
        # attr( x = bin, which = "file" ) <- sprintf( "%s(%s)", 
            # file, 1:length(file) ) 
        
        # bin <- lapply( 
        #     X   = 1:length(bin), 
        #     FUN = function(X){ 
        #         binX <- data.frame( 
        #             "fileName" = file[ X ], 
        #             bin[[ X ]], 
        #             stringsAsFactors = FALSE 
        #         )   
        #         
        #         return( binX ) 
        #     }   
        # )   
        
        ## Bind all the tables into a bigger one:
        # bin <- do.call( what = "rbind", args = bin ) 
    }else{ 
        bin <- bin[[ 1 ]] 
    }   
    
    
    file <- .pathLastItem( p = file, noExt = TRUE )
    attr( x = bin, which = "file" ) <- file 
    
    
    return( bin )
}   



# ==================== macroWriteBin ====================

#' Write bin file for the SOIL and MACRO models.
#'
#' Write bin file for the SOIL and MACRO models. Origial code by 
#'  Kristian Persson. R code vectorisation by Julien Moeys.
#'
#'
#'@param x 
#'  A \code{\link[base]{data.frame}}. 
#'  Table of data to be written in \code{file}. The table must contain one column named "Date" containing POSIXct dates, and
#'  thus must have column names. All columns but "Date" must be of type numerical
#'  (integer or double), and will be written as double. The "Date" will be
#'  converted into integers, representing minutes since 2 days before the 1st of
#'  Januray of year 0001 at 00:00. Missing values are not allowed.
#'  
#'@param file 
#'  Single character string or connection to a binary file. If a
#'  character string, it should be the name of the binary file which the data are
#'  to be written from. The path of the file may be provided as well, if file is
#'  not in the working directory.
#'  
#'@param \dots 
#'  Additional options passed to \code{\link[base]{writeBin}}
#'  
#'  
#'@example inst/examples/macroWriteBin.R
#'
#'@rdname macroWriteBin-methods
#'
#'@export
#'
#'
macroWriteBin <- function(
 x, 
 ...
){  
    UseMethod( "macroWriteBin" )
}   



#'@rdname macroWriteBin-methods
#'
#'@method macroWriteBin macroTimeSeries
#'@export 
macroWriteBin.macroTimeSeries <- function(
 x, 
 ...
){ 
    NextMethod( "macroWriteBin" ) 
}   



#'@rdname macroWriteBin-methods
#'
#'@method macroWriteBin macroTimeSeriesList
#'@export 
macroWriteBin.macroTimeSeriesList <- function(
 x, 
 file, 
 ...
){  
    if( is.data.frame( x ) ){ 
        if( !"index" %in% colnames( x ) ){ 
            stop( "If 'x' is a 'macroTimeSeriesList', it must have a column 'index'" ) 
        }   
        
        n <- length( unique( x[, 'index' ] ) ) 
        
        if( n != length( file ) ){ 
            stop( sprintf( 
                "length(unique(x[,'index'])) and length(file) must be identical (now %s and %s)", 
                n, length( file ) 
            ) ) 
        }   
        
        x <- split( x = x, f = x[, 'index' ] ) 
        
    }else if( is.list( x ) ){ 
        n <- length( x ) 
        
        if( n != length( file ) ){ 
            stop( sprintf( 
                "length(x) and length(file) must be identical (now %s and %s)", 
                n, length( file ) 
            ) ) 
        }   
    }else{ 
        stop( "If 'x' is a 'macroTimeSeriesList', it must be a list or a data.frame"  )
    }   
    
    
    out <- lapply( 
        X   = 1:n, 
        FUN = function(i){ 
            macroWriteBin.data.frame( x = x[[ i ]], file = file[ i ], ... )
        }   
    )   
    
    
    return( invisible( out ) ) 
}   



#'@rdname macroWriteBin-methods
#'
#'@method macroWriteBin list
#'@export 
macroWriteBin.list <- function(
 x, 
 ...
){  
    n <- length(x)
    
    if( n != length( file ) ){ 
        stop( sprintf( 
            "length(x) and length(file) must be identical (now %s and %s)", 
            n, length( file ) 
        ) ) 
    }   
    
    isMacroTimeSeries <- unlist( lapply( 
        X   = x, 
        FUN = function(X){ 
            test <- c( "macroTimeSeries", "data.frame" ) %in% 
                class( X ) 
            
            return( any( test ) )
        } 
    ) ) 
    
    if( !all( isMacroTimeSeries ) ){ 
        stop( "Some items in x are not 'macroTimeSeries'-class or 'data.frame'-class" )
    }   
    
    out <- lapply( 
        X   = 1:n, 
        FUN = function(i){ 
            macroWriteBin.data.frame( x = x[[ i ]], file = file[ i ], ... )
        }   
    )   
    
    return( invisible( out ) ) 
}   



#'@rdname macroWriteBin-methods
#'
#'@method macroWriteBin data.frame
#'@export 
macroWriteBin.data.frame <- function(
    x,
    file,
    ...
){  
    if( !("Date" %in% colnames(x)) ){
        stop( "The table 'x' must contain a column 'Date'" ) 
    }   
    
    if( !("POSIXct" %in% class( x[,"Date"] )) ){   
        stop( "The class of column 'Date' in 'x' must be 'POSIXct'" ) 
    }   
    
    test.na <- apply( 
        X      = x, 
        MARGIN = 1, 
        FUN    = function(X){any(is.na(X) | is.nan(X))} 
    )   
    
    if( any( test.na ) ){
        stop( paste( sum( test.na ), " rows in 'x' were found with NA or NaN values." ) )
    }   
    
    
    #   Control that the date-time series is valid
    .isValidTimeSeries <- getMuPar( "timeSeriesValid" ) 
    
    if( !is.null( .isValidTimeSeries ) ){
        .isValidTimeSeries( x[, "Date" ] ) 
    }   
    
    
    #Version 1.0
    #Writes the contest of a dataframe to a bin file
    #
    # opens binary file for writing
    # con = file(file, open="wb")    
    # 
    #get size of data
    record.number   <- nrow(x) 
    variables.number <- ncol(x) 
    PostSize        <- 4
    
    #Size of a record in bytes
    record.length <- variables.number * PostSize
    #
    if( getMuPar( "header" ) ){ 
        colLength <- (variables.number-1) * ( 52 + 4 + 4 ) 
    }else{ 
        colLength <- 0 
    }   #
    #
    # Write all the (empty) bin string at once:
    # - Calculate its total length 
    total.length <- 
        record.length +                       # File 'info' record
        record.number * record.length +       # Table of data 
        colLength                             # Column names (excl. date)
    #
    # - Read the file
    binData <- raw( length = total.length )
    #
    #Number of records
    rec_ant <- record.number
    #
    #Write first record containg the number of records and the size of the records
    binData[ 1:4 ] <- writeBin( 
        object = as.integer( record.number ), 
        con    = raw(), 
        size   = 4
    )   #
    #
    binData[ 5:8 ] <- writeBin( 
        object = as.integer( record.length ), 
        con    = raw(), 
        size   = 4
    )   #
    #
    # writeBin(rec_ant, con, size=4)
    # writeBin(as.integer(record.length), con, size=4)
    #Pad the record with zeroes
    ## Not needed any more, raw data are automatically 0s...
    # for (zero in 1:(variables.number-2))
    # {
    #      writeBin(0, con, size=4)
    # }
    #
    #Save the data in the dataframe
    #
    if( getMuPar("dateMethod") == 1 )
    {   #
        date.offsetX=-48/24
        #
        # Extract the time zone (summer / winter & time zone)
        x.tz <- format.POSIXct( x = x[1,"Date"], format = "-", usetz = T )
        x.tz <- substr( x = x.tz, start = 3, stop = nchar( x.tz ) ) 
        #
        # "Neutralize" the time zone
        x[,"Date"] <- as.POSIXct( 
            format( 
                x      = x[,"Date"], 
                format = "%Y-%m-%d %H:%M:%S", 
                tz     = x.tz 
            ),  #
            format = "%Y-%m-%d %H:%M:%S", 
            tz = getMuPar( "tz" ) 
        )   #
        #
        # Set the origin date
        originDate <- as.POSIXct( 
            x      = "0001-01-01 00:00:00", 
            format = "%Y-%m-%d %H:%M:%S", 
            tz     = getMuPar( "tz" )
        ) + date.offsetX*24*60*60
        #
        x[,"Date"] <- as.integer( 
            difftime( 
                time1 = x[,"Date"], 
                time2 = originDate, 
                units = "mins" 
            )   #
        )   #
    }   #
    #
    if( getMuPar("dateMethod") == 2 )
    {   #
        #Create a vector to hold dates in julian format
        # data.julian <- rep( as.integer(NA), times = record.number ) 
        #Convert date in POSIXct to julian format
        data.julian <- unlist( lapply( 
            X   = 1:record.number, 
            FUN = function(X){ 
                #print (x[row.nr,1])
                da  <- as.POSIXlt(x[ X, 1 ])$mday
                mon <- as.POSIXlt(x[ X, 1 ])$mon + 1
                yea <- as.POSIXlt(x[ X, 1 ])$year + 1900
                #
                ho = 12
                mi = 0
                if( yea < 0 )
                {   #
                    yea <- yea + 1
                }   #
                if( mon > 2 )
                {   #
                    jy <- yea
                    jm <- mon + 1
                }else{
                    jy <- yea - 1
                    jm <- mon + 13
                }   #
                #
                jd <- floor(365.25 * jy) + floor(30.6001 * jm) + da + 1720995
                tl <- da + 31 * (mon + 12 * yea)
                if( tl > 588829 ) 
                {   #
                    ja <- round(0.01 * jy)
                    jd <- jd + 2 - ja + floor(0.25 * ja)
                }   #
                jd <- jd - 1721424
                jd <- jd * 24 * 60 + ho * 60 + mi
                #
                # data.julian[ row.nr ] <- jd
                return( jd ) 
            }   #
        ) ) #
    }   #
    #
    sel.vec <- (record.length + 1):(record.length * (record.number+1)) 
    #
    sel.colz <- colnames(x) != "Date" 
    #
    binData[ sel.vec ] <- unlist( 
        lapply( 
            X   = 1:nrow(x), # X is row.nr 
            FUN = function(X){ 
                x <- x[ X, ] 
                #
                b1 <- writeBin( 
                    con    = raw(), 
                    object = x[, "Date" ], 
                    size   = 4  
                )   #
                #
                b2 <- writeBin( 
                    con    = raw(), 
                    object = as.double( x[, sel.colz ] ), 
                    size   = 4  
                )   #
                #
                return( c(b1,b2) ) 
            }   #
        )   #
    )   #
    #
    #
    # # A double for loop to write the data
    # for(row.nr in 1:record.number)
    # {
    # 	writeBin(as.integer(data.julian[row.nr]), con, size=4) #date field, julian
    # 	for(col.nr in 1:(variables.number-1))#value fields
    # 	{
    # 		writeBin(x[row.nr, col.nr+1], con, size=4)
    # 	}
    # }
    #
	# Write column names
    # sel.vec.lst2 <- mapply( 
    #     FUN  = seq, 
    #     from = record.length * (record.number+1) + 1 + (0:(variables.number-2))*60, 
    #     to   = record.length * (record.number+1) + (1:(variables.number-1))*60, 
    #     SIMPLIFY = FALSE 
    # )   #
    #
    if( getMuPar( "header" ) ){ 
        sel.vec2 <- (record.length * (record.number+1) + 1):(record.length * (record.number+1) + colLength)
        # 
        sel.colz2 <- colnames(x)[ sel.colz ] 
        # 
        sel.colz3 <- substr( x = sel.colz2, start = 1, stop = 52 ) 
        sel.colz3 <- sprintf( fmt = "%-52s", sel.colz3 ) 
        # 
        # cat( "length( binData[ sel.vec2 ] ): ", binData[ sel.vec2 ], "\n" ) 
        # 
        binData[ sel.vec2 ] <- unlist( 
            lapply( 
                X   = 1:length(sel.colz3), # X is row.nr 
                FUN = function(X){ 
                    nm <- writeChar( 
                        con    = raw(), 
                        object = sel.colz3[ X ], 
                        eos    = NULL 
                    )   #
                    #
                    # cat( "length(nm): ", length(nm), "\n" ) 
                    #
                    minMax <- writeBin( 
                        con    = raw(), 
                        object = as.double( range( x[, sel.colz2[ X ] ] ) ), 
                        size   = 4  
                    )   #
                    #
                    # res <- c(nm,minMax) 
                    #
                    # cat( "length(res): ", length(res), "\n" ) 
                    # 
                    return( c(nm,minMax) ) 
                }   #
            )   #
        )   #
    }   #
    #
    # cat( "length(tmp): ", length(tmp), "\n" ) 
    # 
    # binData[ sel.vec2 ] <- tmp 
    # 
    # cat( "length(sel.vec.lst2): ", length(sel.vec.lst2), "\n" ) 
    #
    # Write column names
    # col.Names <- colnames(x) 
    # print (col.Names)
    # for(col.nr in 2:variables.number) #First column is date time and has no columnname
    # {	
    # 	print (col.Names[col.nr])
    #     # writeChar(col.Names[col.nr], con, eos=NULL) 	#Write column names, padded to 52 characters
    # 	writeChar(sprintf("%-52s", col.Names[col.nr]), con, eos=NULL)
    #
    # 	 writeBin(0, con, size=4)	#4 bytes of nonessential data
    # 	 writeBin(0, con, size=4)   #4 bytes of nonessential data
    # }
    # #Close file
    # close(con) 
    #
    writeBin( 
        con    = file, 
        object = binData,
        size   = NA, 
        ... 
    )   #
}   #




# macroPlot =====================================================

#' Plot time series from SOIL or MACRO simulation data (input or output).
#'
#' Plot time series from SOIL or MACRO simulation data (input or output). When
#'  \code{x} is missing and/or \code{gui} is \code{FALSE}, the function pops-up
#'  menu asking the user which file(s) and which variable(s) to plot, and how.
#'
#'
#'@param x 
#'  A single \code{\link[base]{data.frame}}, or a
#'  \code{\link[base]{list}} of \code{data.frame} containing the data to be
#'  plotted. Each \code{data.frame} must have at least two columns: one column
#'  \code{Date} containing dates in \code{POSIXct} format (see
#'  \code{\link[base]{DateTimeClasses}}), and one or more named columns of data
#'  in some numerical formats. Such \code{data.frame} will presumably be
#'  imported from \code{bin} files, with \code{\link{macroReadBin}}. If missing,
#'  a pop-up menu will ask you the binary files to be read and that contains the
#'  variables to be plotted.
#'
#'@param gui 
#'  Single logical. Set to \code{TRUE} if you want to choose only some
#'  of the columns in the table passed to \code{x}. Will be ignored if
#'  \code{\link[base]{interactive}} is \code{FALSE} (i.e.  if ran outside R GUI
#'  for Windows).
#'
#'@param z 
#'  Vector of character strings. Name of the variables to include 
#'  in the graph. If \code{NULL}, all variables in 'x' are included, 
#'  and if \code{gui} is \code{TRUE}, the user is asked with variable 
#'  should be included.
#'
#'@param subPlots 
#'  Single logical. If \code{TRUE} (default), all the variables
#'  in \code{x} will be plotted in separated sub-plots, with sub-plots on top of
#'  each others. If \code{FALSE}, all the variables in \code{x} will be plotted
#'  in the same plot, on top of each other, with the same Y axis. If \code{gui}
#'  is \code{TRUE}, \code{subPlots} is ignored, and a menu will ask you what to
#'  do.
#'
#'@param verbose 
#'  Single logical. If \code{TRUE}, some text message will be
#'  displayed on the console to explain what is going on.
#'
#'@param xlab 
#'  See \code{\link[graphics]{plot.default}}. A single character
#'  string.  Label of the 'x' axis.
#'
#'@param ylab 
#'  See \code{\link[graphics]{plot.default}}. A vector of character
#'  strings of length one or of the same length as the variables in (or chosen
#'  from) \code{x}.
#'
#'@param ylim 
#'  See \code{\link[graphics]{plot.default}}.
#'
#'@param xlim 
#'  See \code{\link[graphics]{plot.default}}.
#'
#'@param col 
#'  See \code{\link[graphics]{plot.default}} or
#'  \code{\link[graphics]{lines}}. Vector of character strings, line colors.
#'
#'@param sub 
#'  See \code{\link[graphics]{plot}} or \code{\link[graphics]{title}}.
#'  Vector of character strings, sub-titles of each plot.
#'
#'@param lwd 
#'  See \code{\link[graphics]{plot.default}} or
#'  \code{\link[graphics]{lines}}. Vector of integers, line widths (thicknesses).
#'
#'@param lty 
#'  See \code{\link[graphics]{plot.default}}. a vector of line types.
#'
#'@param main 
#'  See \code{\link[graphics]{plot.default}}. Plot title(s).
#'
#'@param cex.main 
#'  See \code{\link[graphics]{par}}. Title(s) expansion factor.
#'
#'@param panel.first 
#'  See \code{\link[graphics]{plot.default}}.
#'
#'@param dLegend 
#'  Single logical value. If \code{TRUE} and \code{subPlots=FALSE}
#'  and more than one variable is plotted, a legend is drawn above the plot (with
#'  distinct colors for each variables).
#'
#'@param las 
#'  See \code{\link[graphics]{par}}.
#'
#'@param bty 
#'  See \code{\link[graphics]{par}}.
#'
#'@param \dots 
#'  Additional arguments passed to \code{\link[graphics]{plot}} and
#'  to \code{\link[graphics]{lines}} (when \code{subPlots} is \code{FALSE}).  See
#'  also \code{\link[graphics]{plot.default}}.
#'
#'@return 
#'  Invisibly returns 'x', or the content of the files selected.
#'
#'
#'@example inst/examples/macroPlotBin.R
#'
#'@rdname macroPlot-methods
#'
#'@export
#'
#'
macroPlot <- function(
 x, 
 ...
){  
    if( missing( x ) & interactive() ){ 
        UseMethod( "macroPlot", object = data.frame() ) 
    }else{ 
        UseMethod( "macroPlot" ) 
    }   
}   



#'@rdname macroPlot-methods
#'
#'@export
#'
macroPlotBin <- function(
 x, 
 ...
){  
    message( "macroPlotBin() is now superseeded by macroPlot() (new generic method)" )
    message( "Use macroPlot() instead, with the same arguments as for macroPlotBin()" )
    
    macroPlot.default( x = x, ... ) 
}   



#'@rdname macroPlot-methods
#'
#'@method macroPlot macroTimeSeries
#'@export 
macroPlot.macroTimeSeries <- function(
 x, 
 ... 
){ 
    macroPlot.default( x = x, ... ) 
}   



#'@rdname macroPlot-methods
#'
#'@method macroPlot macroTimeSeriesList
#'@export 
macroPlot.macroTimeSeriesList <- function(
 x, 
 ... 
){ 
    macroPlot.default( x = x, ... ) 
}   



#'@rdname macroPlot-methods
#'
#'@method macroPlot data.frame
#'@export 
macroPlot.data.frame <- function(
 x, 
 ...
){ 
    macroPlot.default( x = x, ... ) 
}   


#'importFrom grDevices gray
#'importFrom graphics par
#'importFrom graphics rect
#'importFrom graphics axis.POSIXct
#'importFrom graphics axTicks
#'importFrom graphics abline
#'importFrom graphics axis
NULL 

.paf <- function( 
 bg        = gray( .95 ), 
 col       = "white", 
 col0      = gray( .80 ), 
 col.ticks = gray( .50 ), 
 border    = NA, 
 axes      = TRUE, 
 ... 
){  
    #   Fetch plot boundaries
    usr <- graphics::par( "usr" ) 
    
    
    #   Background color
    graphics::rect( xleft = usr[1], ybottom = usr[3], xright = usr[2], 
        ytop = usr[4], col = bg, border = border, ... ) 
    
    
    #   Compute grid positions (x-axis being a POSIXct time)
    usrPOSIXct <- as.POSIXct( usr[1:2], origin = "1970-01-01 00:00:00", 
        tz = "GMT" ) 
    
    
    #   At-points for big and small ticks
    xAt  <- graphics::axis.POSIXct( side = 1, x = usrPOSIXct, labels = FALSE, 
        col = NA ) 
    if( length( xAt ) == 1 ){ 
        dxAt <- max( diff( c( usrPOSIXct[1], xAt, usrPOSIXct[2] ) ) )/2 
    }else{ 
        dxAt <- max( diff(xAt) )/2 
    }   
    xAt2 <- c( xAt[1] - dxAt, xAt + dxAt ); rm( dxAt ) 
    
    yAt  <- graphics::axTicks( side = 2 ) 
    
    if( length( yAt ) == 1 ){ 
        dyAt <- max( diff( c( usr[3], yAt, usr[4] ) ) )/2 
    }else{ 
        dyAt <- max( diff(yAt) )/2 
    }   
    yAt2 <- c( yAt[1] - dyAt, yAt + dyAt ); rm( dyAt ) 
    
    
    #   Get the "official" line width
    lwd <- graphics::par( "lwd" ) 
    
    
    #   Plot the grid
    graphics::abline( h = yAt,  col = col, lwd = lwd )
    graphics::abline( h = yAt2, col = col, lwd = lwd/2 )
    graphics::abline( v = xAt,  col = col, lwd = lwd )
    graphics::abline( v = xAt2, col = col, lwd = lwd/2 )
    
    #   Special line for the Y0
    if( usr[3] <= 0 & usr[4] >= 0 ){ 
        graphics::abline( h = 0,  col = col0, lwd = lwd ) 
    }   
    
    if( axes ){ 
        #   Y and right axes 
        for( i in c(2,4) ){ 
            
            
            graphics::axis( side = i, labels = ifelse( i == 2, TRUE, FALSE ), 
                lwd = 0, lwd.ticks = lwd, col.ticks = col.ticks ) 
            
            graphics::axis( side = i, at = yAt2, 
                labels = FALSE, tcl = -.25, lwd = 0, 
                lwd.ticks = lwd/2, col.ticks = col.ticks )
        }   
        
        #   X and top axes
        for( i in c(1,3) ){             
            
            
            #   X axis labels
            if( i == 1 ){ 
               graphics::axis.POSIXct( side = i, x = usrPOSIXct, at = xAt, 
                    labels = TRUE, col = NA ) 
            }   
            
            graphics::axis( side = i, at = xAt, labels = FALSE, lwd = 0, 
                lwd.ticks = lwd, col.ticks = col.ticks ) 
            
            graphics::axis( side = i, at = xAt2, labels = FALSE, tcl = -.25, 
                lwd = 0, lwd.ticks = lwd/2, col.ticks = col.ticks )
        }   
    }   
}   

    # x <- as.POSIXct( as.Date( 0:10, origin = "1999-01-01" ) )
    # y <- rnorm( length( y ) ) 

    # # par( "las" = 1 )
    # plot( x = x, y = y, axes = FALSE, panel.first = .paf(), 
        # las = 1 ) 



#'@rdname macroPlot-methods
#'
#'@method macroPlot default
#'@export 
#'
#'@importFrom utils flush.console
#'@importFrom graphics locator
#'@importFrom graphics abline
#'@importFrom graphics par 
#'@importFrom graphics layout 
#'@importFrom graphics plot 
#'@importFrom graphics rect 
#'@importFrom graphics legend 
#'@importFrom graphics lines  
#'@importFrom grDevices hcl
#'@importFrom grDevices gray 
macroPlot.default <- function(
    x, 
    gui         = TRUE, 
    z           = NULL, 
    subPlots    = TRUE, 
    verbose     = TRUE, 
    xlab        = "Date", 
    ylab        = NULL, 
    ylim        = NULL, 
    xlim        = NULL, 
    col         = NULL,  
    sub         = NULL, 
    lwd         = 2L, 
    lty         = NULL, 
    main        = NULL, 
    cex.main    = NULL, 
    panel.first = .paf(), 
    dLegend     = TRUE, 
    las         = 1L, 
    bty         = "n", 
    ...
){  
    panel.first <- substitute( panel.first )
    
    xDep <- deparse( substitute( x ) ) 
    
    ## Check that the class of x is (a list of) data.frame
    if( missing( x ) ){     # ifelse( is.data.frame( x ), nrow(x) == 0, FALSE )
        if( interactive() ){ 
            # Pop-up a menu to choose the bin file to be 
            #   imported
            if( verbose ){ message( 
                "'x' is missing. You will be asked which binary files you want to plot (pop-up menu)\n" 
            ) }    
            
            file <- .chooseBinFiles(
                caption = "Select one or several binary file(s) to plot", 
                multi   = TRUE  
            )   
            
            if( length(file) == 0 ){ 
                stop( "You haven't chosen any binary file to read :o(" )
            }   
            
            file <- sort( file ) 
            
            if( verbose ){ message( 
                sprintf( "Now importing files: %s\n", paste( file, collapse = ", " ) )    
            ) }    
            
            # Import the files
            x <- macroReadBin( file = file ) 
            
            if( length( file ) == 1 ){ 
                x <- list( x ) 
                
                attr( x = x, which = "file" ) <- .pathLastItem( p = file, noExt = TRUE ) 
                
                names( x ) <- .pathLastItem( p = file, noExt = TRUE ) 
            }   
            
            # if( length( file ) > 1 ){ 
                # tmp <- attr( x = x, which = "file" ) 
                
                # x <- split( x = x, f = x[, "index" ] ) 
                
                # attr( x = x, which = "file" ) <- tmp; rm( tmp )
            # }   
            
            # names( x ) <- .pathLastItem( p = file ) 
            
            test.class <- TRUE 
        }else{ 
            stop( "'x' can not be missing when R running in a non-interactive mode" )
        }   
    }else if( is.data.frame( x ) ){ 
        
        # test.class <- any( c("data.frame","macroTimeSeries","macroTimeSeriesList") %in% class( x ) ) 
        
        if( ("index" %in% colnames( x )) & is.null( attr( x = x, which = "file" ) ) ){ 
            tmp <- sprintf( "index(%s)", unique( x[, "index" ] ) )
            
            x <- split( x = x, f = x[, "index" ] ) 
            
            attr( x = x, which = "file" ) <- tmp; rm( tmp )
        }else{ 
            if( is.null( attr( x = x, which = "file" ) ) ){ 
                attr( x = x, which = "file" ) <- xDep
            }   
            
            tmp <- attr( x = x, which = "file" )
            x   <- list( x ) 
            attr( x = x, which = "file" ) <- tmp; rm( tmp )
        }   
        
        test.class <- TRUE 
        
    }else if( ("macroTimeSeriesList" %in% class( x )) | is.list( x ) ){ 
        
        if( is.list( x ) ){ 
            test.class <- unlist( lapply( 
                X   = x, 
                FUN = function(X){ 
                    return( "data.frame" %in% class( X ) ) 
                }   
            ) ) 
            
            if( is.null( attr( x = x, which = "file" ) ) ){ 
                if( !is.null( names( x ) ) ){ 
                    attr( x = x, which = "file" ) <- names( x ) 
                    
                }else{ 
                    attr( x = x, which = "file" ) <- 
                        sprintf( "item(%s)", 1:length(x) )
                    
                }   
            }   
            
            # if( all( test.class ) ){
                # x <- do.call( what = "rbind", args = x )
            # }   
            
            # if( !"index" %in% colnames( x ) ){ 
                # stop( "If 'x' is a 'macroTimeSeriesList' and list its data.frame must contain a column 'index'" )
            # }   
            
        }else if( is.data.frame( x ) ){ 
            test.class <- TRUE 
            
            if( "index" %in% colnames( x ) ){ 
                tmp <- sprintf( "index(%s)", unique( x[, "index" ] ) )
                
                x <- split( x = x, f = x[, "index" ] ) 
                
                attr( x = x, which = "file" ) <- tmp; rm( tmp )
            }else{ 
                if( is.null( attr( x = x, which = "file" ) ) ){ 
                    attr( x = x, which = "file" ) <- xDep
                }   
            }   
            
            # if( !"index" %in% colnames( x ) ){ 
                # stop( "If 'x' is a 'macroTimeSeriesList' and data.frame it must contain a column 'index'" )
            # }   
            
        }else{ 
            test.class <- FALSE 
        }   
        
    }else{ 
        test.class <- FALSE 
    }   
    
    if( any( !test.class ) ){ 
        stop( "'x' must be a (list of) data.frame" ) 
    }   
    
    
    file <- attr( x = x, which = "file" ) 
    if( is.null( file ) ){ 
        warning( "'x' is missing an attribute 'file'. Something went wrong" )
        
        file <- sprintf( "item(%s)", 1:length(x) )
    }   
    
    
    ## List column names:
    Y.name <- lapply( 
        X   = x, 
        FUN = function(X){ 
            colnames(X) 
        }   #
    )   #
    
    
    ## Check that there is a Date format
    test.Date <- unlist( lapply( 
        X   = Y.name, 
        FUN = function(X){ 
            ("Date" %in% X) & (length(X) >= 2)
        }   
    ) ) 
    
    if( any( !test.Date ) ){
         stop( "data.frame(s) in 'x' must have a 'Date' column and at least another column" ) 
    }   
    
    Y.name <- lapply( 
        X   = Y.name, 
        FUN = function(X){ 
            X[ X != "Date" ] 
        }   
    )   
    
    
    if( !is.null( z ) ){ 
        if( "Date" %in% z ){ 
            warning( "'z' should not include 'Date'" ) 
            
            z <- z[ z != "Date" ] 
        }   
        
        Y.name <- lapply( 
            X   = Y.name, 
            FUN = function(X){ 
                testZ <- z %in% X 
                
                if( !all( testZ ) ){ 
                    stop( sprintf( 
                        "Some columns in 'z' are missing in 'x': %s", 
                        paste( z[ !testZ ], collapse = "; " )
                    ) ) 
                }   
                
                return( z ) 
            }   
        )   
    }   
    
    
    
    # +---------------------------------------------------------+
    # | Loop over the main menu                                 |
    # +---------------------------------------------------------+
    loopCount <- 1L 
    zoomSet   <- FALSE 
    n         <- 1L 
    
    repeat{ 
        
        # +---------------------------+
        # | Main menu                 |
        # +---------------------------+
        if( (loopCount > 1) & gui & interactive() ){ 
            mainMenuItem <- c( 
                "1" = "Change the variable(s)", 
                "2" = "Change the type of plot",  
                "3" = "Zoom in", 
                "4" = "Reset the zoom",  
                "5" = "Exit the function" 
            )   
            
            if( !zoomSet ){ mainMenuItem <- 
                mainMenuItem[ names(mainMenuItem) != "4" ] } 
            if( n == 1L ){  mainMenuItem <- 
                mainMenuItem[ names(mainMenuItem) != "2" ] } 
            
            mainMenu <- .macroMenu(
                choices  = mainMenuItem,  
                graphics = FALSE, 
                title    = "Main plot menu. Do you want to:", 
                error    = "You have not chosen any action!", 
                multi    = TRUE 
            )   
            
            mainMenu <- mainMenuItem[ mainMenu ] 
            mainMenu <- as.integer( names( mainMenu ) ) 
            
            
            ## Reset the loop count if the variables are changed:
            loopCount <- ifelse( mainMenu == 1L, 1L, loopCount ) 
            
            ## Reset the zoom "indicator"
            zoomSet   <- ifelse( mainMenu == 1L, FALSE, zoomSet ) 
        }else{ 
            mainMenu <- 0L 
        }   
        
        
        
        # +---------------------------+ 
        # | Case: exit                | 
        # +---------------------------+ 
        
        if( mainMenu == 5L ){ 
            message( "Plot operations finished (interrupted by the user)" ) 
            
            break 
        }   
        
        
        
        # +---------------------------+
        # | Choose the variables      |
        # +---------------------------+
        if( gui & interactive() & ((loopCount == 1L) | mainMenu == 1L) ){ 
            # if( verbose ){ message( 
            #     "'gui' is TRUE. You will be asked which variable you want to plot, and how you want to plot them (pop-up menu)\n" 
            # ) } 
            
            Y.name0 <- lapply( 
                X   = 1:length(Y.name), 
                FUN = function(X){ 
                    Y.name <- Y.name[[ X ]] 
                    
                    mRes <- .macroMenu(
                        choices  = Y.name, 
                        graphics = FALSE, 
                        title    = sprintf( 
                            "Choose one or several variables to plot from table %s", 
                            file[ X ] ), 
                        error    = sprintf( 
                            "You have not chosen any variables from table %s", 
                            X ), 
                        multi    = TRUE 
                    )   
                    
                    return( Y.name[ mRes ] )  
                }   
            )   
            
            
            # How many variables?
            n <- unlist( lapply( 
                X   = Y.name0, 
                FUN = function(X){ 
                    length( X ) 
                }   
            ) ) 
            n <- sum(n)
            
            if( verbose ){ message( 
                sprintf( "You have chosen %s variables\n", n ) 
            ) } 
        
        }else if( gui & !interactive() ){ 
            stop( "'gui' can not be TRUE when R is not running in interactive mode" )
        }else if( !gui ){ 
            Y.name0 <- Y.name 
            
            # How many variables?
            n <- unlist( lapply( 
                X   = Y.name0, 
                FUN = function(X){ 
                    length( X ) 
                }   
            ) ) 
            n <- sum(n)
        }   
        
        
        # +---------------------------+ 
        # | Zoom & xlim               | 
        # +---------------------------+ 
        
        ## Case 1: zoom
        if( gui & interactive() & (loopCount != 1L) & (mainMenu == 3L) ){ 
            
            message( "Zoom selection. NOTE: USE THE LAST PLOT (MOST BOTTOM RIGHT)" ) 
            
            
            message( "Select date-time boundary 1 (lower or higher), on the plot area" ) 
            
            utils::flush.console() 
            
            ## Select date boundary 1:
            l1 <- l1a <- graphics::locator( n = 1, type = "n" )$"x" 
            
            ## Convert it to a Date (was integer)
            l1 <- as.POSIXct( l1, origin = "1970-01-01 00:00:00", tz = "GMT" ) 
            l1 <- format.POSIXct( l1, tz = getMuPar( "tz" ) ) 
            l1 <- as.POSIXct( l1, , tz = getMuPar( "tz" ) ) 
            
            ## Display a line at that date-time
            abline( v = l1a, col = "pink" ) 
            
            
            message( "Select date-time boundary 2 (lower or higher), on the plot area" ) 
            
            utils::flush.console() 
            
            ## Select date boundary 1:
            l2 <- graphics::locator( n = 1, type = "n" )$"x" 
            
            ## Convert it to a Date (was integer)
            l2 <- as.POSIXct( l2, origin = "1970-01-01 00:00:00", tz = "GMT" ) 
            l2 <- format.POSIXct( l2, tz = getMuPar( "tz" ) ) 
            l2 <- as.POSIXct( l2, , tz = getMuPar( "tz" ) ) 
            
            ## Display a line at that date-time
            graphics::abline( v = l2, col = "pink" ) 
            
            
            ## Convert that into ylim 
            xlim0 <- c( l1, l2 ) 
            xlim0 <- c( min(xlim0), max(xlim0) ) 
            
            message( sprintf( "Date-time range chosen: %s to %s\n", xlim0[1], xlim0[2] ) ) 
            
            
            ## Set the zoom indicator
            zoomSet <- TRUE 
        
        ## Case 2: set or re-set the zoom
        }else if( (loopCount == 1L) | (mainMenu == 4L) | !gui ){ 
            if( is.null( xlim ) ){ 
                xlim0 <- lapply( 
                    X   = 1:length(Y.name0), 
                    FUN = function(X){ 
                        ## Select the table:
                        x <- x[[ X ]]
                        
                        ## Select the columns:
                        x <- x[, "Date" ] 
                        
                        x <- data.frame( "min" = min( x ), "max" = max( x ) ) 
                        
                        ## Get the max value
                        return( x ) 
                    }   
                )   
                
                xlim0 <- do.call( what = "rbind", args = xlim0 )
                
                xlim0 <- c( min( xlim0[,"min"] ), max( xlim0[,"max"] ) ) 
            }else{ 
                xlim0 <- xlim 
            }   
        }   
        
        
        # +---------------------------+ 
        # | Single plot or multiple   | 
        # | sub-plots                 | 
        # +---------------------------+ 
        
        ## subPlots variables in sub-plots?
        if( gui & interactive() & ((loopCount == 1L) | mainMenu == 2L) ){
            if( n > 1 ){ 
                mRes <- .macroMenu(
                    title    = sprintf( "Should all %s variables be plotted:", n ), 
                    choices  = c( "In a single plot", "In stacked sub-plots"), 
                    graphics = FALSE, 
                    error    = "You have not chosen how the variables should de plotted :o(", 
                    multi    = FALSE 
                )   
                
                if( verbose & (mRes == 1) ){ message( 
                    "You have chosen a single plot. You can use 'subPlots = FALSE' to do that when 'gui = FALSE'\n"  
                ) } 
                
                if( verbose & (mRes == 2) ){ message( 
                    "You have chosen sub-plots. You can use 'subPlots = TRUE' to do that when 'gui = FALSE'\n"  
                ) } 
                
                subPlots <- ifelse( mRes == 1, FALSE, TRUE )
            }else{ 
                subPlots <- TRUE 
            }   
        }   
        
        
        
        # +---------------------------+ 
        # | Settings                  | 
        # +---------------------------+ 
        
        # +---------------------------+ 
        # | ylab: Y-axis labels
        if( is.null(ylab) ){  
            ylab0 <- unlist( lapply( 
                X   = 1:length(Y.name0), 
                FUN = function(X){  
                    # paste( nm.x[ X ], Y.name0[[ X ]], sep = ":" ) 
                    return( Y.name0[[ X ]] )
                }   
            ) ) 
            
        }else if( (length(ylab) != 1) & (length(ylab) != n) ){ 
            if( !subPlots ){ 
                stop( "When 'subPlots' is 'FALSE' 'ylab' must be 'NULL' or length 1" ) 
            }   
            
            stop( "'ylab' must be 'NULL', or length 1, or the same length as the variables (chosen) in 'x'" ) 
        }else{ 
            ylab0 <- ylab 
        }   
        
        if( length( ylab0 ) == 1 ){ ylab0 <- rep(ylab0,n) } 
        
        
        # # +---------------------------+ 
        # # | panel.first & grid() 
        # if( is.null(panel.first) ){ 
            # panel.first <- call( "grid" ) 
        # }   
        
        
        # +---------------------------+ 
        # | col: line colors
        if( is.null( col ) ){ 
            col0 <- grDevices::hcl( 
                h = seq( from = 15, to = 360+15, length.out = n+1 ), 
                c = 100, 
                l = 50 )[ -(n+1) ] 
            
        }else{ 
            col0 <- col 
        }   
        
        if( length( col0 ) == 1 ){ col0 <- rep(col0,n) } 
        
        
        # +---------------------------+ 
        # | lwd: line(s) width(s)
        if( length( lwd ) == 1 ){ 
            lwd0 <- rep( lwd, n ) 
            oldParLwd <- graphics::par( "lwd" = lwd )$"lwd" 
        }else{ 
            lwd0 <- lwd 
        }   
        
        
        # +---------------------------+ 
        # | lwd: line(s) type(s)
        if( is.null( lty ) & !subPlots ){ 
            lty0 <- rep( 1:4, length.out = n ) 
        }else{ 
            lty0 <- lty 
        }   
        
        
        # +---------------------------+ 
        # | sub: subtitles
        fileNames <- unlist( lapply( 
            X   = 1:length(file), 
            FUN = function(X){ 
                rep( file[ X ], length( Y.name0[[ X ]] ) ) 
            }   
        ) ) 
        
        if( is.null( sub ) & subPlots ){ 
            sub0 <- paste0( "File: ", fileNames ) 
        }else{ 
            sub0 <- sub 
        }   
        
        if( length( sub0 ) == 1 ){ sub0 <- rep( sub0, n ) } 
        
        
        # +---------------------------+ 
        # | main: main title
        if( is.null( main ) & subPlots ){ 
            main0 <- ylab0 
        }else{ 
            main0 <- main 
        }   
        
        if( length( main0 ) == 1 ){ main0 <- rep(main0,n) } 
        
        
        # +---------------------------+ 
        # | cex.main: main title 'expansion'
        if( is.null( cex.main ) & subPlots ){ 
            cex.main0 <- 0.8 
        }else{ 
            cex.main0 <- cex.main 
        }   
        
        if( length( lty0 ) == 1 ){ lty0 <- rep(lty0,n) } 
        
        
        # +---------------------------+ 
        # | Plot layout (and ylim)    |
        # +---------------------------+ 
        
        #   Set axis label style
        oldParLas <- graphics::par( "las" = las )$"las"
        
        ## If more than one variable, create sub-plots
        if( (n > 1) & (!subPlots) ){ 
            
            # +---------------------------+ 
            # | Plot layout (no subplots) 
            mx <- ifelse( dLegend, 2L, 1L ) 
            
            mat <- matrix( 
                data = 1:mx, 
                nrow = mx, 
                ncol = 1 )
            
            graphics::layout( mat = mat, heights = c(1,2)[ 1:mx ] ) 
            
            if( n > 1 ){ 
                ylab2 <- "(see legend)" # expression( italic( "(see the legend)" ) ) 
            }else{ 
                ylab2 <- ylab0 
            }   
            
            # +---------------------------+ 
            # | ylim: variable range        
            if( is.null( ylim ) ){ 
                ylim0 <- range( unlist( lapply( 
                    X   = 1:length(Y.name0), 
                    FUN = function(X){ 
                        ## Select the table:
                        x <- x[[ X ]]
                        
                        ## Select the columns:
                        x <- x[, Y.name0[[ X ]] ] 
                        
                        ## Get the max value
                        return( range(x) ) 
                    }   
                ) ) ) 
            }else{ 
                ylim0 <- ylim 
            }   
            
            
            # +---------------------------+ 
            # | Add a legend              |
            # +---------------------------+ 
            
            # oldParMar <- par( c("mar","bg") ) 
            if( dLegend ){ 
                oldParMar <- graphics::par( "mar" = c(0,0,0,0) )$"mar" 
                # par( "bg" = grDevices::gray( .9 ) ) 
                
                graphics::plot( x = 1, y = 1, xlab = "", ylab = "", bty = "n", 
                    xaxt = "n", yaxt = "n", type = "n" ) 
                
                # Draw a gray background rectangle:
                usr <- graphics::par( "usr" ) 
                graphics::rect(
                    xleft   = usr[1], 
                    ybottom = usr[3], 
                    xright  = usr[2], 
                    ytop    = usr[4], 
                    col     = NA, 
                    border  = grDevices::gray( .5 ) 
                )   
                
                ## Add the general legend:
                graphics::legend( 
                    x       = "center", 
                    title   = "File(s) and Variable(s):", 
                    legend  = paste0( fileNames, ", ", ylab0 ), 
                    lwd     = lwd0, 
                    col     = col0, 
                    lty     = lty0, 
                    bty     = "n"
                )   
                
                graphics::par( "mar" = oldParMar ) 
            }   
            
            # +---------------------------+ 
            # | Empty plot on which lines 
            # | will be plotted
            
            graphics::plot( 
                x           = xlim0, 
                y           = ylim0, 
                xlab        = xlab, 
                ylab        = ylab2, 
                xlim        = xlim0, 
                type        = "n", 
                sub         = sub0, 
                panel.first = eval( panel.first ), 
                #las         = las, 
                bty         = bty, 
                axes        = FALSE, 
                ... 
            )   
        }else{ 
            
            # +---------------------------+ 
            # | Plot layout (with subplots) 
            if( n > 2 ){ 
                #n <- 3 
                nrowz <- ceiling( sqrt( n ) ) 
                ncolz <- ceiling( n / nrowz ) 
                #nrowz; ncolz 
                
                mat <- matrix( 
                    data = 1:(nrowz*ncolz), 
                    nrow = nrowz, 
                    ncol = ncolz ) 
            }else{ 
                mat <- matrix( 
                    data = 1:n, 
                    nrow = n, 
                    ncol = 1 ) 
            }   
            
            # if( n != 1 ){ layout( mat = mat ) } 
            graphics::layout( mat = mat ) 
        }   
        
        
        # +---------------------------+ 
        # | Generate the plot         |
        # +---------------------------+ 
        
        # +---------------------------+ 
        # | Plot variables: table by table 
        plotCount <- 0 
        
        for( subTbl in 1:length(Y.name0) ){ 
            subTbl.name <- Y.name0[[ subTbl ]] 
            
            # +---------------------------+ 
            # | Plot variables: variables by variables 
            # | in a given table
            for( varNb in 1:length(subTbl.name) ){ 
                plotCount <- plotCount + 1 
                
                plotVar <- subTbl.name[ varNb ] 
                
                if( subPlots ){ 
                    graphics::plot( 
                        x           = x[[ subTbl ]][, "Date" ], 
                        y           = x[[ subTbl ]][, plotVar ], 
                        xlab        = xlab, 
                        ylab        = "(see title)", # expression( italic( "(see the title)" ) ), 
                        type        = "l", 
                        xlim        = xlim0, 
                        col         = col0[ plotCount ], 
                        main        = main0[ plotCount ], 
                        cex.main    = cex.main0, 
                        sub         = sub0[ plotCount ], 
                        lwd         = lwd0[ plotCount ], 
                        lty         = lty0[ plotCount ], 
                        panel.first = eval( panel.first ), 
                        #las         = las, 
                        bty         = bty, 
                        axes        = FALSE, 
                        ... 
                    )   
                }else{ 
                    graphics::lines( 
                        x    = x[[ subTbl ]][, "Date" ], 
                        y    = x[[ subTbl ]][, plotVar ], 
                        col  = col0[ plotCount ], 
                        lwd  = lwd0[ plotCount ], 
                        lty  = lty0[ plotCount ], 
                        ...  
                    )  
                }   
            }   
        }   
        
        
        # +---------------------------+ 
        # | Case: exit (no gui case)  | 
        # +---------------------------+ 
        
        if( !gui ){ 
            break 
        }else{ 
            message( "Plot created\n" )
        }   
        
        
        ## Increment the loop counter
        loopCount <- loopCount + 1L 
        
    }   ## End of the repeat loop over menus
    
    
    #   Reset par(lwd,las)
    graphics::par( "lwd" = oldParLwd, "las" = oldParLas )
    
    
    return( invisible( x ) ) 
}   




# ==================== macroAggregateBin ====================

#' Aggregate simulation results by some date subsets, using various functions
#'
#' Aggregate simulation results by some date subsets, using various functions.
#'  macroAggregateBin can be used on a data.frame containing simulation results (or
#'  weather data or any time series data) to compute some aggregation function
#'  (FUN = sum, mean, median, ...) over subsets of dates (aggregate by day, week,
#'  month, ...).
#'
#'
#'@param x 
#'  A data.frame, with a date column named after 'dateCol' (default
#'  "Date") and one or several variables to be aggregated on a certain time
#'  interval (defined in 'by'). The column 'dateCol' must be in POSIXct format.
#'  
#'@param columns 
#'  A vector of character strings. Name of the columns to be
#'  selected and aggregated. 'dateCol' does not need to be specified here, but
#'  can be included.
#'  
#'@param by 
#'  A charcater string representing a POSIXct format (see
#'  ?format.POSIXct). "\%Y-\%m-\%d" (the default) will aggregate the data by days
#'  and "\%Y-\%m-\%d \%H", "\%Y-\%W", "\%Y-\%m" will aggregate the data by hour,
#'  week of the year or month, respectively. Other combinations are possible.
#'  
#'@param FUN 
#'  A function to be applied to aggregate the data on each element of
#'  'by'. Can be 'sum', 'mean', 'median', etc. For removing missing values,
#'  choose something like 'function(x)sum(x,na.rm=TRUE)'.  Another possibility
#'  would be 'function(x)quantile(x,probs=.75)'.  The same function is applied
#'  for all columns, so consider applying different macroAggregateBin() on different
#'  data types if needed.
#'
#'@param dateCol 
#'  Name of the column cotaining the POSIXct date values. Default
#'  is 'Date'.
#'  
#'  
#'@return 
#'  Returns a data.frame with the values in columns aggregated by 'by'
#'  with the function 'FUN'. Notice that the format of 'dateCol' is then
#'  "character", and not any more POSIXct (because no uniform date format are
#'  possible for exporying back the dates).
#'  
#'  
#'@example inst/examples/macroAggregateBin.R
#'
#'
#'@export
#'
#'
#'@importFrom stats aggregate
macroAggregateBin <- function(
    x, 
    columns = colnames(x), 
    by      = "%Y-%m-%d", 
    FUN     = sum, 
    dateCol = "Date" 
){  #
    if( !dateCol %in% columns ){ columns <- c(dateCol,columns) } 
    #
    testColumns <- columns %in% colnames( x ) 
    #
    if( any( !testColumns ) )
    {   #
        stop( paste( 
            sep = "", 
            "Some column(s) was/were not found in x:", 
            paste( columns[ !testColumns ], collapse = ", " ), 
            "." 
        ) ) #
    }   #
    #
    x <- x[, columns ] 
    columns2 <- colnames(x) != dateCol 
    columns2 <- columns[ columns2 ] 
    x <- x[, c(dateCol,columns2) ] 
    #
    if( !("POSIXct" %in% class( x[,dateCol] ) ) ){ stop("'dateCol' must be of class POSIXct") } 
    #
    byIndex  <- format.POSIXct( x = x[,dateCol], format = by ) 
    # byIndex2 <- as.integer( as.factor( byIndex ) ) 
    #
    FUN2 <- FUN; rm( FUN ) 
    
    
    x <- stats::aggregate( 
        x        = x[, -1, drop = FALSE ], 
        by       = list( "Date" = byIndex ), 
        FUN      = FUN2, 
        simplify = TRUE 
    )   #
    #
    colnames( x )[ -1 ] <- columns2 
    #
    # x <- data.frame( 
    #     "Date" = unique( byIndex ), 
    #     x, 
    #     stringsAsFactors = FALSE 
    # )   #
    #
    colnames(x)[1] <- dateCol 
    #
    x <- x[, columns ] 
    #
    return( x ) 
}   




# ==================== macroStripRunID ====================

## # Remove the Run ID from the column names of a MACRO simulation 
## #    result.
## #
## # Remove the Run ID from the column names of a MACRO simulation 
## #    result.
## #
## #
## #@param x
## #    A vector of character strings. Column names of a MACRO input 
## #    result table.
## # 
## #@param splitChar 
## #    Single character string. Character that separates the different 
## #    parts of the columns names.
## #    
## # 
## #@return
## #    Returns a data.frame, with 'clean' column names.
## #
## #
macroStripRunID <- function(
    x, 
    splitChar = "_"
){  
    split.colz <- strsplit( 
        x     = x, 
        split = splitChar, 
        fixed = TRUE  
    )   #
    
    # Remove the RUNID from columns names
    split.colz <- lapply( 
        X   = split.colz, 
        FUN = function(X){ 
            l <- length( X ) 
            
            if( l > 1 ){ 
                # Extract the last item
                last <- suppressWarnings( as.integer( X[ l ] ) ) 
                
                # Extract the 2nd last item
                secondLast <- suppressWarnings( as.integer( X[ l-1 ] ) ) 
                
                # Set the ID to ""
                if( !is.na( last ) ){ 
                    if( !is.na( secondLast ) ){ 
                        
                        # ID is 2nd last
                        X[ l-1 ] <- "" 
                    }else{ 
                        
                        # ID is last
                        X[ l ] <- ""
                    }   
                }   
            }   
            
            return( X )
        }   
    )   
    
    # split.colz[ l.split.colz == 2 ] <- lapply( 
        # X   = split.colz[ l.split.colz == 2 ], 
        # FUN = function(X){ 
            # X[ -2 ] 
        # }   #
    # )   #
    
    # split.colz[ l.split.colz == 5 ] <- lapply( 
        # X   = split.colz[ l.split.colz == 5 ], 
        # FUN = function(X){ 
            # X[ -4 ] 
        # }   #
    # )   #
     
    x <- unlist( lapply( 
            X   = split.colz, 
            FUN = function(X){ 
                paste( 
                    X[ X != "" ], 
                    collapse = splitChar 
                )   
            }   
    )   )   
    
    return( x ) 
}   #




# ==================== macroConvertBin ====================

#' Converts MACRO/SOIL binary files into CSV or TXT text files
#'
#' Converts MACRO/SOIL binary files into CSV or TXT text files. 
#'  The function is a wrapper around \code{\link{macroReadBin}} 
#'  and \code{\link[utils]{write.table}}. It is possible to choose 
#'  the field delimiter and the decimal mark.
#'
#'
#'@param file 
#'  Vector of character strings or a single \code{\link{connection}}
#'  to a binary file. If a vector character strings, it should be the name(s) of
#'  the binary file(s) which the data are to be read from. The path of the
#'  file(s) may be provided as well, if file(s) is (are) not in the working
#'  directory.
#'  
#'@param gui 
#'  Single logical. Set to \code{TRUE} if you want to choose only some
#'  of the columns in the table passed to \code{x}. Will be ignored if
#'  \code{\link[base]{interactive}} is \code{FALSE} (i.e.  if ran outside R GUI
#'  for Windows).
#'  
#'@param sep 
#'  Single character. Columns / field separator. Ignored if
#'  \code{gui=TRUE}. Choose \code{','} for comma, \code{';'} for semi-colon,
#'  \code{'\t'} for tabulation and \code{' '} for space.
#'  
#'@param dec 
#'  Single character. Decimal mark. Ignored if \code{gui=TRUE}.
#'  
#'@param fileOut 
#'  Vector of character strings or a single
#'  \code{\link{connection}} to a text. If a vector character strings, it should
#'  be the name(s) of the text file(s) where the data are to be written to. The
#'  path of the file(s) may be provided as well, if these file(s) is (are) not in
#'  the working directory. If \code{NULL}, file names will be generated
#'  automatically.
#'  
#'@param writeArgs 
#'  List of additional arguments passed to \code{\link[utils]{write.table}}
#'  
#'@param overwrite 
#'  Single logical value. If \code{TRUE} (not the default), 
#'  Existing output files are overwritten without warning.
#'  
#'@param \dots 
#'  More arguments passed to \code{\link{macroReadBin}}.
#'
#'
#'@export
#'
#' 
macroConvertBin <- function(# Converts MACRO/SOIL binary files into CSV or TXT text files
    file, 
    gui       = TRUE, 
    sep       = ",", 
    dec       = ".", 
    fileOut   = NULL, 
    writeArgs = list( "row.names" = FALSE ), 
    overwrite = FALSE, 
    ...
){  ## If no file name is provided
    if( missing( file ) ){ 
        if( interactive() ){ 
            ## Pop-up a menu to choose the bin file to be 
            ## imported
            file <- .chooseBinFiles(
                caption = "Select one or several binary file(s)", 
                multi   = TRUE  
            )   
            
            if( length(file) == 0 ){ 
                stop( "You haven't choosen any binary file to read :o(" )
            }   
            
            file <- sort( file ) 
        }else{ 
            stop( "'file' can not be missing when R is not being used interactively" ) 
        }   
    }   
    
    
    ## Read the files:
    x <- macroReadBin( file = file, ... ) 
    
    
    if( gui ){ 
        sep <- .macroMenu( 
            title       = "Choose the field separator:", 
            choices     = c( 
                "Comma              (',').  Extension: .csv", # 1 
                "Semicolon          (';').  Extension: .csv", # 2 
                "Tabulation         ('\t'). Extension: .txt", # 3 
                "Single-space       (' ').  Extension: .txt"  # 4 
                #"Space, fixed-width ('').   Extension: .txt"  # 5 
            ),  
            graphics    = FALSE, 
            preselect   = ",", 
            error       = "You haven't chosen anything :o(", 
            multi       = FALSE 
        )   
        
        
        sep <- if( sep == 1 ){ 
            sep <- ","  
        }else if( sep == 2 ){ 
            sep <- ";"  
        }else if( sep == 3 ){ 
            sep <- "\t" 
        }else if( sep == 4 ){ 
            sep <- " " 
        }   
        # else if( sep == 5 ){ 
        #     sep <- "" 
        # }   
        
        
        dec <- .macroMenu( 
            title       = "Choose the decimal mark:", 
            choices     = c( ".", "," ), 
            graphics    = FALSE, 
            preselect   = ".", 
            error       = "You haven't chosen anything :o(", 
            multi       = FALSE 
        )   
        dec <- ifelse( dec == 1, ".", "," )  
    }   
    
    if( sep == dec ){ 
        stop( "'sep' and 'dec' are identical" )
    }   
    
    if( is.data.frame(x) ){ 
        x <- list( x ) 
    }   
    
    
    ## Create new file names (if needed):
    if( is.null( fileOut ) ){ 
        fileOut <- paste0( 
            file, 
            ifelse( sep %in% c("\t"," ",""), ".txt", ".csv" )
        )   
    }else{ 
        if( length( fileOut ) != length( file ) ){ 
            stop( "'file' and 'fileOut' must be of the same length" )
        }   
    }   
    
    
    ## Test if the file exists:
    testFile <- file.exists( fileOut ) 
    
    if( any( testFile ) ){ 
        #   Select only the 1st files
        testFile  <- which( testFile ) 
        moreFiles <- ifelse( max( testFile ) > 3, "...", 
            character(0) )
        testFile  <- testFile[ testFile <= 3 ]
        
        if( gui & (!overwrite) ){
            message( sprintf( 
                "Some output file(s) already exist(s) (%s)", 
                paste( c( fileOut[ testFile ], moreFiles ), collapse = ", " )  
            ) ) 
            
            overwrite2 <- .macroMenu( 
                title       = "Do you want to overwrite these files?", 
                choices     = c( "No", "Yes" ), 
                graphics    = FALSE, 
                preselect   = "No", 
                error       = "You haven't chosen anything :o(", 
                multi       = FALSE 
            )   
            overwrite2 <- ifelse( overwrite2 == 1, FALSE, TRUE ) 
            
            message( "Note: Set 'overwrite' to TRUE to avoid the question above." )
            
            if( !overwrite2 ){ 
                stop( "Operation aborded by the user" )
            }   
        }else if( !overwrite ){
            stop( sprintf( 
                "Some output file(s) already exist(s) (%s). Set 'overwrite' to TRUE to ignore existing files.", 
                paste( c( fileOut[ testFile ], moreFiles ), collapse = ", " )  
            ) ) 
        }   
    }     
    
    
    # ## Format the table, if sep = ""
    # if( sep = "" ){ 
    #     x <- lapply( 
    #         X   = x, 
    #         FUN = function(X){ 
    #             ## Prepare arguments for formatC 
    #             formatArgs0 <- c( list( 
    #                 "x" = X[,-1, drop = FALSE ], formatArgs ) ) 
    #             
    #             ## Apply formatC 
    #             X2 <- do.call( 
    #                 what = "formatC", 
    #                 args = formatArgs0 
    #             )   
    #             
    #             ## Add a single-space
    #             X2 <- paste0( X2, " " ) 
    #             
    #             ## Re-format as a data-frame 
    #             X2 <- data.frame( 
    #                 "Date"  = paste0( X[, "Date" ], " " ), 
    #                 X2, 
    #                 stringsAsFactors = FALSE 
    #             )   
    #             
    #             ## Find the maximum number of characters
    #             cnX2 <- colnames( X2 ) 
    #             
    #             lapply( 
    #                 X   = 1:ncol(X2), 
    #                 FUN = function(X){ 
    #                     ## Max nb of char in the data
    #                     mx <- max( nchar( X2[, X ] ) ) 
    #                     
    #                     newName <- cnX2[ X ]
    #                     
    #                     ## Nb of char in the column name
    #                     n <- nchar( newName ) 
    #                     
    #                     newName <- paste0( 
    #                         newName, 
    #                         rep( " ", times =  )
    #                     )   
    #                 }   
    #             )   
    #             
    #             return( X2 ) 
    #         }   
    #     )   
    # }   
    
    
    for( f in 1:length(file) ){ 
        x0 <- x[[f]] 
        class(x0) <- "data.frame"
        
        writeArgs0 <- c( list( 
            "x"     = x0, 
            "file"  = fileOut[f], 
            "sep"   = sep, 
            "dec"   = dec  
        ), writeArgs ) 
        
        do.call( what = "write.table", args = writeArgs0 ) 
    }   
    
    
    return( invisible( x ) ) 
}   




# ==================== macroViewBin ====================

#' Reads a MACRO/SOIL binary file and view it as a table.
#'
#' Reads a MACRO/SOIL binary file and view it as a table.
#'
#'
#'@param file 
#'  Single character strings or a single \code{\link{connection}} to
#'  a binary file. If a vector character strings, it should be the name of the
#'  binary file which the data are to be read from. The path of the file may be
#'  provided as well, if file is not in the working directory.
#'  
#'@param \dots 
#'  More arguments passed to \code{\link{macroReadBin}}.
#'
#'
#'@export
#'
#'
#'importFrom utils View
macroViewBin <- function(
    file, 
    ...
){  ## If no file name is provided
    if( missing( file ) ){ 
        if( interactive() ){ 
            ## Pop-up a menu to choose the bin file to be 
            ## imported
            file <- .chooseBinFiles(
                caption = "Select one or several binary file(s)", 
                multi   = FALSE   
            )   
            
            if( length(file) == 0 ){ 
                stop( "You haven't choosen any binary file to read :o(" )
            }   
            
            file <- sort( file ) 
        }else{ 
            stop( "'file' can not be missing when R is not being used interactively" ) 
        }   
    }   
    
    
    ## Read the files:
    x <- macroReadBin( file = file[1], ... ) 
    
    
    ## View the file     
    utils::View( x, title = file[1] )  
    
    return( invisible( x ) ) 
}   


