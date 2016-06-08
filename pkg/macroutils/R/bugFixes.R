

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

#' Clean-up MACRO 5.2 parameter databases. Fixes 4 known bugs (orphan, incomplete or unnecessary values)
#'
#' Clean-up MACRO 5.2 parameter databases. Fixes 4 known bugs 
#'  (orphan, incomplete or unnecessary values). It is very 
#'  highly recommended to make a backup-copy of MACRO 5.2 
#'  parameter databases before you try this utility. The 
#'  R \bold{\code{\link[RODBC]{RODBC-package}}} is required to run 
#'  this function, and you also need to run a \bold{32 bit 
#'  (i386)} version of R (maybe located in 
#'  \code{\{R installation directory\}bin/i386/Rgui.exe}, 
#'  if it has been installed).
#'
#'
#'@param file 
#'  Vector of character strings or a single \code{\link{connection}}
#'  to a MACRO GUI MS Access parameter database. If a vector of 
#'  character strings, it should be the name(s) of
#'  the Access database(s) containing MACRO parameters. The path 
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
#'@importFrom utils sessionInfo
#'@importFrom utils select.list
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
        arch <- utils::sessionInfo()[[ "R.version" ]][[ "arch" ]]
        
        if( arch != "i386" ){
            warning( sprintf( 
                "'RODBC' MS Access interface requires a 32 bit version of R (i386) (now: %s). Consider running R i386 instead ({R install dir}/i386/Rgui.exe)", 
                arch
            ) ) 
        }   
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
        cp <- utils::select.list( 
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
            
            message( sprintf( "Starts processing database: '%s'.", f ) )
            
            channel <- RODBC::odbcConnectAccess( access.file = f ) 
            
            on.exit( try( RODBC::odbcClose( channel = channel ) ) )
            
            tablesList <- RODBC::sqlTables( channel = channel )
            
            .tables <- c( "Output()", "Run_ID", "OutputLayers" ) 
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
            
            outputLayers <- RODBC::sqlFetch( channel = channel, sqtable = "OutputLayers" )
            
            # runIds <- runIdTbl[, "R_ID" ]
            
            
            
            # 1 - ORPHAN `R_ID` IN `Output()` (NOT ANY MORE 
            #     IN `Run_ID`)
            # ----------------------------------------------
            
            #   ID in "Output()" but not in "Run_ID"
            missId <- unique( missId0 <- output[ 
                !(output[, "R_ID" ] %in% runIdTbl[, "R_ID" ]), 
                "R_ID" ] ) 
            
            #   Delete IDs in Output() that are 'orphan'
            if( length( missId ) > 0 ){ 
                message( sprintf( 
                    "Found %s orphan values in `Output()` for RUNID(s) %s", 
                    length( missId0 ), 
                    paste( missId, collapse = "; " )
                ) ) 
                
                rm( missId0 )
                
                for( id in missId ){ 
                    RODBC::sqlQuery( 
                        channel = channel, 
                        query   = sprintf( "DELETE * FROM `Output()` WHERE `R_ID` = %s", id ), 
                    )   
                }   
                
                message( "Orphan values deleted in `Output()`" )
            }else{
                message( "Found no orphan values in `Output()` (fine!)" )
            }   
            
            rm( missId )
            
            #   Re-fetch Output()
            output <- RODBC::sqlFetch( channel = channel, sqtable = "Output()" ) 
            
            
            
            # 2 - DUPLICATED `R_ID`-`Var` IN `Output()`
            # ----------------------------------------------
            
            #   Find RUNID with duplicated export parameters
            uOutput   <- output[, c( "R_ID", "Var" ) ] 
            selDuplic <- duplicated( uOutput ) 
            
            duplicId  <- unique( uOutput[ selDuplic, "R_ID" ] ) 
            rm( uOutput, selDuplic )
            
            if( length( duplicId ) > 0 ){ 
                message( sprintf( 
                    "Found %s duplicated values in `Output()` for RUNID(s) %s", 
                    length( selDuplic ), 
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
                        
                        rm( outputId )
                        
                    }   
                    
                    # nrow( sOutput ) # 98
                    
                    rm( sOutput, uVar, v )
                }   
                
                message( "Duplicated values deleted in `Output()`" )
            }else{
                message( "Found no duplicated values in `Output()` (fine!)" )
            }   
            
            rm( duplicId )
            
            
            
            # 3 - EXPORT PARAMS IN `Output()` NOT SELECTED 
            #     BUT STILL PRESENT IN `OutputLayers`
            # ----------------------------------------------
            
            #   Find outputs that are not selected in `Output()`
            #   but nonetheless present in `OutputLayers`
            uOutput2 <- unique( output[, c( "R_ID", "Var", "Output()ID", "selected" ) ] )
            
            #   Only keep those that are not selected 
            #   as layered output
            uOutput2 <- subset( x = uOutput2, subset = eval( quote( selected != 1 ) ) )
            
            #   Find the one that should not be there
            testOutLay <- outputLayers[, "Output()ID" ] %in% 
                uOutput2[, "Output()ID" ]
            
            if( any( testOutLay ) ){
                #   Reverse selection: entries in `Output()`
                #   that have unnecessary layers parameters in 
                #   `OutputLayers`
                testOut <- uOutput2[, "Output()ID" ] %in% 
                    outputLayers[ testOutLay, "Output()ID" ] 
                
                message( sprintf( 
                    "Found %s unnecessary entries in `OutputLayers` for RUNID(s) %s", 
                    length( testOutLay ),
                    paste( unique( uOutput2[ testOut, "R_ID" ] ), collapse = "; " ) 
                ) ) 
                
                rm( testOut )
                
                #   Find the OutputLayerID to be removed
                idOut <- outputLayers[ testOutLay, "OutputLayerID" ]
                
                RODBC::sqlQuery( 
                    channel = channel, 
                    query   = sprintf( 
                        "DELETE * FROM `OutputLayers` WHERE `OutputLayerID` IN (%s)", 
                        paste( as.character( idOut ), collapse = ", " ) 
                    ),  
                )   
                
                message( sprintf( 
                    "Deleted %s unnecessary entries in `OutputLayers`", 
                    length( idOut ) 
                ) ) 
                
                rm( idOut )
            }else{
                message( "Found no unnecessary entries in `OutputLayers` (fine!)" )
            }   
            
            rm( uOutput2, testOutLay )
            
            
            
            # 4 - EXPORT PARAMS IN `OutputLayers` WHERE THE 
            #     COLUMN `Selected` IS NOT SET (neither 0 nor 
            #     1), presumably after more layers were 
            #     added
            # ----------------------------------------------
            
            uOutput2 <- unique( output[, c( "R_ID", "Var", "Output()ID", "selected" ) ] )
            
            #   Find the one that should not be there
            selFixSelCol <- is.na( outputLayers[, "Selected" ] )
            
            if( any( selFixSelCol ) ){
                #   Reverse selection: entries in `Output()`
                #   that have unnecessary layers parameters in 
                #   `OutputLayers`
                testOut <- uOutput2[, "Output()ID" ] %in% 
                    outputLayers[ selFixSelCol, "Output()ID" ] 
                
                message( sprintf( 
                    "Found %s entries in `OutputLayers` where selected is not set, for RUNID(s) %s", 
                    sum( selFixSelCol ), 
                    paste( unique( uOutput2[ testOut, "R_ID" ] ), collapse = "; " ) 
                ) ) 
                
                rm( testOut )
                
                #   Find the OutputLayerID to be removed
                idOut <- outputLayers[ selFixSelCol, "OutputLayerID" ]
                
                RODBC::sqlQuery( 
                    channel = channel, 
                    query   = sprintf( 
                        "UPDATE `OutputLayers` SET `Selected`=0 WHERE `OutputLayerID` IN (%s)", 
                        paste( as.character( idOut ), collapse = ", " ) 
                    ),  
                )   
                
                message( sprintf( 
                    "Set %s entries in `OutputLayers` (`Selected` set to 0)", 
                    length( idOut ) 
                ) ) 
                
                rm( idOut )
            }else{
                message( "Found no entries with `Selected` not set in `OutputLayers` (fine!)" )
            }   
            
            rm( selFixSelCol )
            
            
            
            # Close and exit
            # ----------------------------------------------
            
            RODBC::odbcClose( channel = channel ) 
            
            on.exit() 
        }   
    )   
    
    message( "Database cleaned" ) 
}   


