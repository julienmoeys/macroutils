

# .chooseAccessFiles ============================================

#'@importFrom tcltk tk_choose.files

## # Pop-up a menu to choose MS Access file from the file system.
## # 
## # Pop-up a menu to choose MS Access file from the file system.
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
.chooseAccessFiles <- function(
    caption = "Select one or several MACRO parameter database(s) (MS Access)", 
    multi   = TRUE
){  
    if( !interactive() ){ 
        stop( "'.chooseAccessFiles' can only be used in interactive mode" )
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
            "Access files (*.mdb)", "*.mdb", 
            "All",                  "*" ), 
        nrow  = 2, 
        ncol  = 2, 
        byrow = TRUE  
    )   
    rownames( filterz ) <- c( "mdb", "all" ) 
    
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
        
        file <- tcltk::tk_choose.files(
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



# macroBugFixCleanDb ============================================

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
#'  to a MACRO GUI MS Access parameter database. If a vector of 
#'  character strings, it should be the name(s) of
#'  the Access database(s) contianing MACRO parameters. The path 
#'  of the file(s) may be provided as well, if file(s) 
#'  is (are) not in the working directory.
#'
#'@param paranoia 
#'  Single logical value. If \code{TRUE}, the user is asked 
#'  if he made a backup copy of the parameter database.
#'
#'@param \dots Additional options passed to specific 
#'  methods.
#'
#'
#'@return 
#'  Do not return anything. Used for it side effect on a MACRO 
#'  parameter database.
#'
#'
#'@rdname macroBugFixCleanDb
#'
#'@export
#'
#'
macroBugFixCleanDb <- function(
 file, 
 paranoia = TRUE, 
 ...
){  
    # paranoia <- TRUE; file <- "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/perform/pkg/rmacro/inst/PROJECT_MACRO_5.rmacro.mdb"
    
    testRODBC <- "RODBC" %in% rownames( installed.packages() )
    
    if( !testRODBC ){ 
        stop( "'RODBC' package not available. Please install RODBC first: install.package('RODBC')" )
    }else{ 
        # require( "RODBC" ) 
    }   
    
    
    ## If no file name is provided
    if( missing( file ) ){ 
        if( interactive() ){ 
            ## Pop-up a menu to choose the bin file to be 
            ## imported
            file <- .chooseAccessFiles(
                caption = "Select one or several MACRO parameter database(s) (MS Access)", 
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
    
    
    if( interactive() & paranoia ){ 
        cp <- select.list( 
            title       = "Did you made a backup-copy of your parameter database?",
            choices     = c( "Yes", "No" ), 
            preselect   = NULL, 
            multiple    = FALSE, 
            graphics    = FALSE 
        )   
        
        if( cp == "No" ){ 
            stop( "Then make a backup-copy of your parameter database" )
        }   
    }   
    
    
    silent <- lapply( 
        X   = file, 
        FUN = function(f){ 
            # f <- file[1]
            
            channel <- RODBC::odbcConnectAccess( access.file = f ) 
            
            on.exit( try( RODBC::odbcClose( channel = channel ) ) )
            
            tablesList <- RODBC::sqlTables( channel = channel )
            
            .tables <- c( "Output()", "Run_ID" ) 
            testTables <- .tables %in% 
                tablesList[, "TABLE_NAME" ]
            
            if( !all(testTables) ){ 
                stop( sprintf( 
                    "The table(s) %s cannot be found in the database (%s)", 
                    paste( .tables[ !testTables ], collapse = "; " ), 
                    f 
                ) ) 
            };  rm( .tables, testTables ) 
            
            output <- RODBC::sqlFetch( channel = channel, sqtable = "Output()" )
            
            runIdTbl <- RODBC::sqlFetch( channel = channel, sqtable = "Run_ID" )
            
            # runIds <- runIdTbl[, "R_ID" ]
            
            #   ID in "Output()" but not in "Run_ID"
            missId <- unique( output[ 
                !(output[, "R_ID" ] %in% runIdTbl[, "R_ID" ]), 
                "R_ID" ] ) 
            
            #   Delete IDs in Output() that are 'orphan'
            if( length( missId ) > 0 ){ 
                message( sprintf( 
                    "Found orphan values in `Output()` for RUNID(s) %s", 
                    paste( missId, collapse = "; " )
                ) ) 
                
                for( id in missId ){ 
                    RODBC::sqlQuery( 
                        channel = channel, 
                        query   = sprintf( "DELETE * FROM `Output()` WHERE `R_ID` = %s", id ), 
                    )   
                }   
                
                message( "Orphan values deleted" )
            }   
            
            #   Re-fetch Output()
            output <- RODBC::sqlFetch( channel = channel, sqtable = "Output()" ) 
            
            #   Find RUNID with dupliacted export parameters
            uOutput   <- output[, c( "R_ID", "Var" ) ] 
            selDuplic <- duplicated( uOutput ) 
            
            duplicId  <- unique( uOutput[ selDuplic, "R_ID" ] ) 
            rm( uOutput, selDuplic )
            
            if( length( duplicId ) > 0 ){ 
                message( sprintf( 
                    "Found duplicated values in `Output()` for RUNID(s) %s", 
                    paste( duplicId, collapse = "; " ) 
                ) ) 
                
                for( id in duplicId ){ 
                    # id <- duplicId[ 1 ]
                    
                    sOutput <- subset( 
                        x      = output, 
                        subset = eval( quote( R_ID == id ) ) )
                    
                    #   Order the table
                    sOutput <- sOutput[ 
                        order( sOutput[, "Var" ], sOutput[, "Output()ID" ] ), ]
                    
                    #   Unique list of variables
                    uVar <- unique( sOutput[, "Var" ] ) 
                    
                    nrow( sOutput ) # 98
                    
                    for( v in uVar ){ 
                        # v <- uVar[ 1 ]
                        
                        outputId <- sOutput[ 
                            sOutput[,"Var"] == v, 
                            "Output()ID" ] 
                        
                        if( length( outputId ) > 1 ){ 
                            RODBC::sqlQuery( 
                                channel = channel, 
                                query   = sprintf( "DELETE * FROM `Output()` WHERE `Output()ID` = %s", min( outputId ) ), 
                            )   
                        }   
                        
                    }   
                    
                    nrow( sOutput ) # 98
                    
                }   
                
                message( "Duplicated values deleted" )
            }   
            
            RODBC::odbcClose( channel = channel ) 
            
            on.exit() 
        }   
    )   
    
    message( "Database cleaned" ) 
}   


