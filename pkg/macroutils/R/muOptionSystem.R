## +------------------------------------------------------------+
## | Package's parameter system (START)                         |
## +------------------------------------------------------------+

## Create two environment that will contain the package's
## parameters.

## - Backup / reference 
.muParList <- new.env() 

## - User visible container
muParList  <- new.env() 



## Set some default parameters: 

.muParList[[ "dateMethod" ]]          <- 1 
.muParList[[ "removeSpace" ]]         <- TRUE 
.muParList[[ "alphaNumOnly" ]]        <- TRUE 
.muParList[[ "stripRunID" ]]          <- TRUE 
.muParList[[ "tz" ]]                  <- "GMT" 
.muParList[[ "alphaNum" ]]            <- c( letters, LETTERS, 0:9, " ", "_" ) 
.muParList[[ "header" ]]              <- TRUE 
.muParList[[ "lastBinWd" ]]           <- character(0) 
.muParList[[ "timeSeriesValid" ]]     <- NULL 



# ==================== muPar ====================

#' Get or set default parameters for the package.
#'
#' Get or set default parameters for the package. Notice changes done to the
#'  parameter values are reset everytime the R session is closed and the package
#'  is reloaded.
#'
#'  The function has 3 possible, non-exclusive behaviours: \itemize{ \item If
#'  \code{reset=TRUE}, resetting the parameters to their initial values, as
#'  defined in this function. \item (Silently) returning the actual value of the
#'  package parameters. If \code{par=NULL}, all the values are returned.  If
#'  \code{par} is a vector of parameter names, their value will be returned.
#'  \item Setting-up the value of some parameters, passing a list of parameter
#'  value to \code{par} OR setting some of the parameters listed above. }
#'
#'  Notice that when \code{reset=TRUE} and some new parameter values are
#'  provided, the parameters are first reset, and then the new parameter values
#'  are set. If \code{par} is a list, parameters are set first according to
#'  values in \code{par}, and then according to values in the parameters listed
#'  below. This combination is not recommended, but nonetheless possible.
#'
#'  The actual value of the parameters is stored in (and can be retrieved from)
#'  the environment \code{rspPars}. The default value of the parameters are
#'  stored in the environment \code{rspPars}. Do not use them directly.
#'
#'
#'@param par 
#'  Three possible cases: \itemize{ \item If \code{par} is \code{NULL}
#'  (default): All the actual value of the parameters will be silently returned.
#'  \item If \code{par} is a vector of character strings representing parameter
#'  names. The value of the parameters named here will be (silently) returned.
#'  \item If \code{par} is a list following the format \code{tag = value}, where
#'  \code{tag} is the name of the parameter to be changed, and \code{value} is
#'  its new value.  Such a list is returned by \code{muPar()}. Notice that
#'  parameters can also be set indivudually, using the options listed below. }
#'
#'@param reset 
#'  Single logical. If TRUE, all the parameters will be set to their
#'  default value. Values are reset before any change to the parameter values, as
#'  listed below.
#'
#'@param dateMethod 
#'  Single integer. If 1 uses a new (shorter) method for
#'  converting dates (from the weird bin file format to POSIXct), if 2 uses the
#'  old / slower method implemented for the SOIL model (and MACRO?) and if 0 (or
#'  any other value than 1 or 2) returns the original date in minutes since 2
#'  days before the 1st of January of year 0001 at 00:00. For 1 and 2 the date
#'  returned is POSIXct with time-zone \code{tz}, and for 0 it is integers.
#'
#'@param removeSpace 
#'  Single logical. If TRUE remove extra spaces and minus
#'  signs in column names and replace them by underscores _. Multiple spaces are
#'  grouped. Trailing (end) space(s) are always removed (whatever is the value of
#'  \code{removeSpace}). If \code{gui} is \code{TRUE}, \code{removeSpace} is
#'  ignored, and a menu will ask you what to do.
#'  
#'@param alphaNumOnly 
#'  Single logical. If TRUE remove all non alpha-numeric
#'  characters from the column names (and replace them by underscores). See also
#'  the \code{alphaNum} parameter. Use this option to obtain database compatible
#'  column names. If \code{gui} is \code{TRUE}, \code{alphaNumOnly} is ignored,
#'  and a menu will ask you what to do.
#'  
#'@param stripRunID 
#'  Single logical. If TRUE remove the simulation ID at the end
#'  of each column name. \code{removeSpace} must be \code{TRUE} for using this
#'  option (otherwise ignored). If \code{gui} is \code{TRUE}, \code{stripRunID}
#'  is ignored, and a menu will ask you what to do.
#'  
#'@param tz 
#'  Single character string. "A timezone specification to be used for
#'  the conversion. System-specific (see \code{\link{as.POSIXlt}}), but "" is the
#'  current time zone, and "GMT" is UTC".
#'  
#'@param alphaNum 
#'  Vector of single characters. List of characters allowed in
#'  the column names when \code{alphaNumOnly == TRUE}.
#'  
#'@param header 
#'  Single logical. If TRUE the header is present in the bin file,
#'  if FALSE it is not present.
#'  
#'@param lastBinWd 
#'  Single character string. Last folder in which some binary files
#'  were fetched.
#'  
#'@param timeSeriesValid 
#'  A valid R function. The first parameter of the function 
#'  must accept a Date or POSIXct time series (as read from 
#'  or exported to a BIN-file). The purpose of the 
#'  function is to check that the time series is "valid". 
#'  The default function 
#'  \code{\link[macroutils]{isValidTimeSeries}} (set when 
#'  the package is attached) will for example check that 
#'  date-times in the time series are unique, sorted and 
#'  regular(ly increasing). Set to \code{NULL} or 
#'  \code{function(x){TRUE}} to cancel any check.
#' 
#'  
#'@return 
#'  Returns a partial or complete list of (actual) parameter values, as a
#'  named list.
#'  
#'  
#'@seealso \code{\link{getMuPar}}.
#'
#'
#'@export
#'
#'
muPar <- function(
    par     = NULL, 
    reset   = FALSE, 
    dateMethod, 
    removeSpace,
    alphaNumOnly,
    stripRunID, 
    tz,
    alphaNum, 
    header, 
    lastBinWd, 
    timeSeriesValid
){  
    parList <- names( formals(muPar) ) 
    parList <- parList[ !(parList %in% c( "par", "reset" )) ] 
    
    
    ## (1) Reset the parameter values:
    if( reset ){ 
        v  <- as.list( .muParList ) 
        nv <- names( v ) 
        
        lapply( 
            X   = 1:length(v), 
            FUN = function(X){ 
                assign( x = nv[ X ], value = v[[ X ]], envir = muParList ) 
            }   
        )   
        
        rm( nv, v ) 
    }   
    
    
    ## (2) Change the parameter values:
    
    # Get actual parameter values:
    muParValues <- as.list( get( x = "muParList" ) ) 
    
    # Case: par is a list of parameters to be set
    if( is.list( par ) ){
        parNames <- names( par ) 
         
        if( is.null( parNames ) ){ 
            stop( "If 'par' is a list, its item must be named." )
        }   
        
        # Check that all parameters in par exists:
        testpar1 <- !(parNames %in% names(muParValues)) 
        
        if( any( testpar1 ) ){ 
            stop( sprintf( 
                "Some of the parameter names listed in 'par' could not be found: %s.", 
                paste( parNames[ testpar1 ], collapse=", " ) 
            ) ) 
        }  
        
        # Set the values
        for( i in parNames ){ 
            if( is.null( par[[ i ]] ) ){
                muParValues[ i ] <- list( NULL ) # Fixed 2016/01/27
            }else{
                muParValues[[ i ]] <- par[[ i ]] 
            }   
        }   
    }   
    
    # Set all the individual parameters provided as a function's 
    # argument(s)
    for( parLabel in parList ){ 
        testExpr <- substitute( 
            expr = !missing(theLabel), 
            env  = list( theLabel = as.symbol(parLabel) ) 
        )   
        
        if( eval( testExpr ) ){ 
            tmpPar <- get( x = parLabel )  
            
            if( is.null( tmpPar ) ){
                muParValues[ parLabel ] <- list( NULL )
            }else{
                muParValues[[ parLabel ]] <- tmpPar
            };  rm( tmpPar )
            
        }   
    }   
    
    # Set the parameter values at once 
    nv <- names( muParValues ) 
    lapply( 
        X   = 1:length(muParValues), 
        FUN = function(X){ 
            assign( x = nv[ X ], value = muParValues[[ X ]], envir = muParList ) 
        }   
    )   
    
    
    ## (3) Return the parameter values:
    
    # Case: return the value of some parameters:
    if( is.character(par) & (length(par) != 0) ){ 
        # Test that all demanded parameters exists:    
        testpar <- !(par %in% names(muParValues)) 
        
        if( any( testpar ) ){ 
            stop( sprintf( 
                "Some of the parameter names listed in 'par' could not be found: %s.", 
                paste( par[ testpar ], collapse=", " ) 
            ) ) 
        }  
        
        ret <- muParValues[ par ] 
    
    # Case: return the value of all parameters:
    }else{ 
        ret <- muParValues 
    }   
    
    return( invisible( ret ) ) 
### Returns a partial or complete list of (actual) parameter values, 
### as a named list.
}   




# ==================== muPar ====================

#' Get a single default parameters for the package.
#'
#' Get a single default parameters for the package. Wrapper around
#'  \code{\link{muPar}}.
#'
#'
#'@param par 
#'  See the \code{par} argument in \code{\link{muPar}}. Notice that
#'  if more than one parameter name is provided, only the first one will be
#'  returned.
#'  
#'  
#'@return 
#'  Returns the value of the parameter \code{par}, without the list
#'  container of \code{\link{muPar}}.
#'
#'
#'@export
#'
#'
getMuPar <- function(
    par 
){  
    return( muPar( par = par )[[ 1L ]] ) 
}   






## Test that all parameters in '.muParList' have been included in 
## the function rspParameters() 

# List of parameter names:
parNames <- names( as.list( .muParList ) ) 

# List of argument names
muParF <- names(formals(muPar))
muParF <- muParF[ !(muParF %in% c("par","reset")) ]

# List of parameters handled by muPar(): do they match with 
# the default parameters?
testpar  <- !(parNames %in% muParF)

if( any(testpar) ){ 
    stop( sprintf( 
        "Some parameters in '.muParList' are not in names(formals(muPar)): %s", 
        paste( parNames[ testpar ], collapse = ", " ) 
    ) )  
}   

# Other way round
testpar2 <- !(muParF %in% parNames)

if( any(testpar2) ){ 
    stop( sprintf( 
        "Some parameters in names(formals(muPar)) are not in '.muParList': %s", 
        paste( muParF[ testpar2 ], collapse = ", " ) 
    ) )  
}   

rm( testpar, parNames, testpar2, muParF ) 



## Set the current list of parameters
muParList <- list2env( as.list( .muParList ) ) 
