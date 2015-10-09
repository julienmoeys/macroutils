
#' INTERNAL: Calculate the yearly and 80th percentile groundwater concentration from a MACROInFOCUS output bin file
#'
#' INTERNAL & EXPERIMENTAL: Calculate the yearly and 80th percentile 
#'  groundwater concentration from a MACROInFOCUS output bin 
#'  file. \bold{WARNING} This function is \bold{not} part 
#'  of the official MACROInFOCUS program. It is provided 
#'  for test-purpose, without any guarantee or support from 
#'  the authors, CKB or SLU. You are strongly recommended to 
#'  benchmark the function against a range of (official) 
#'  MACROInFOCUS simulation results, before you use the 
#'  function. You are also strongly recommended to inspect 
#'  the code of these functions before you use them. To 
#'  inspect the content of these functions, simply type 
#'  \code{body( macroutils:::macroInFocusGWConc.data.frame )} 
#'  after you have loaded the package \code{macroutils}.
#'
#'
#'@references 
#'  European Commission (2014) "Assessing Potential for 
#'  Movement of Active Substances and their Metabolites to 
#'  Ground Water in the EU Report of the FOCUS Ground Water 
#'  Work Group, EC Document Reference Sanco/13144/2010 
#'  version 3, 613 pp. \url{http://focus.jrc.ec.europa.eu/gw/docs/NewDocs/focusGWReportOct2014.pdf}
#'  See in particular the last sentence page 475. 
#'
#'
#'@author 
#'  Julien Moeys \email{Julien.Moeys@@slu.se}, 
#'  contributions from Stefan Reichenberger 
#'  \email{SReichenberger@@knoell.com}.
#'
#'
#'@param x
#'  Either a vector of character strings, a 
#'  \code{\link{data.frame}}, or a list of 
#'  \code{\link{data.frame}}s. If a vector of character 
#'  strings, names (and possibly paths to) a \code{.bin} file 
#'  output by MACROInFOCUS. The argument is passed internally 
#'  to \code{\link[macroutils]{macroReadBin}} (its 
#'  \code{file}-argument). If a (list of) 
#'  \code{\link{data.frame}}(s), it should be imported from 
#'  a \code{.bin} file output by MACROInFOCUS (for example 
#'  with \code{\link[macroutils]{macroReadBin}}).
#'
#'@param nbYrsWarmUp
#'  Single integer values: Number of warm-up years that 
#'  should be removed from the beginning of the model output.
#'  Six years of warn-up are considered in FOCUS.
#'
#'@param years80th
#'  Vector of integer values: Index of the years who should 
#'  be used to calculate the 80th percentile concentration 
#'  after removing the warm-up years and (then) sorting in 
#'  order of increasing by concentrations. See FOCUS 
#'  Groundwater main report p 475 (reference above).
#'  Years 16 and 17 are used in FOCUS.
#'
#'@param quiet
#'  Single logical value. Set to \code{TRUE} to suppress the 
#'  warning message. Default value to \code{FALSE}.
#'
#'@param negToZero
#'  Single logical value. If \code{TRUE} (not the default) 
#'  negative concentrations will be set to 0 (presumably like 
#'  in MACROInFOCUS). If \code{FALSE}, they will not be set 
#'  to 0, but if some of the concentrations used to calculate 
#'  the 80th percentile (see \code{years80th}) are negative, 
#'  a warning will be issued (so that the user knows that 
#'  concentrations may differ from those in MACROInFOCUS).
#' 
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[macroutils]{macroReadBin}}.
#'
#'
#'@return 
#'  Returns a \code{\link{data.frame}} with one row per 
#'  input file (or table), and the following columns
#'  \itemize{
#'    \item \code{CONC_PERC_80TH}: The 80th percentile of the 
#'      yearly pesticide concentration in the water percolating 
#'      at the lower boundary of the soil. Derived from the 
#'      columns \code{TSOUT} and \code{TFLOWOUT}. Corresponds 
#'      to \code{CONC_PERC} in the table of yearly concentrations 
#'      (see below).
#'    \item \code{CONC_LAYER_80TH}: The 80th percentile of the 
#'      yearly pesticide concentration in the water flowing 
#'      through layer X (X depending on the FOCUS-scenario, 
#'      also called "target depth" in MACROInFOCUS)  
#'      at depth Y. Derived from \code{SFLOW_}, 
#'      \code{SFLOWOUT_}, \code{WOUT_} and \code{WFLOWOUT_}, 
#'      after converting these variables to daily values and 
#'      cumulating them for each year. Corresponds 
#'      to \code{CONC_LAYER} in the table of yearly concentrations 
#'      (see below). 
#'    \item \code{yearFrom}: The year corresponding to the 
#'      16th (or \code{years80th[1]}th) concentration after 
#'      ordering them by order of increasing \code{CONC_LAYER}.
#'    \item \code{yearTo}: The year corresponding to the 
#'      17th (or \code{years80th[2]}th) concentration after 
#'      ordering them by order of increasing \code{CONC_LAYER}.
#'    \item \code{file}: Name of the input file, or index of 
#'      the table in the input list of table.
#'  }   
#'  In addition to this table, an \code{\link{attr}}ibute  
#'  is attached to the output table. This attribute is 
#'  names \code{more}, can be obtained by typing 
#'  \code{\link{attr}( output, "more" )}, and contains 
#'  a table with all the yearly results for all the input files.
#'  It has the following columns: \code{"year"}, \code{"TSOUT"}, 
#'  \code{"TFLOWOUT"}, \code{"acc_SFLOW"}, \code{"acc_SFLOWOUT"}, 
#'  \code{"acc_WOUT"}, \code{"acc_WFLOWOUT"}, \code{"acc_WFLOWTOT"}, 
#'  \code{"acc_SFLOWTOT"}, \code{"CONC_PERC"}, \code{"CONC_LAYER"}, 
#'  \code{"file"}. \code{"TSOUT"} and \code{"TFLOWOUT"} have 
#'  been de-accumulated and then re-accumulated for each simulation 
#'  \code{year}. All the columns starting with \code{acc_} 
#'  have been first converted to daily flow (instead of hourly 
#'  flow) and then accumulated for each simulation \code{year}. 
#'  \code{file} is the same as above. The total number of rows 
#'  should be \code{20 * length(x)} (20 simulation years times 
#'  number of input tables from MACROInFOCUS).
#'
#'
#'@example inst/examples/macroInFocusGWConc-examples.R
#'
#'@rdname macroInFocusGWConc-methods
#'
#'@keywords internal
#'
#'@export
#'
macroInFocusGWConc <- function( 
 x, 
 nbYrsWarmUp = 6L, 
 years80th = 16:17, 
 negToZero = FALSE, 
 quiet = FALSE, 
 ...
){  
    UseMethod( "macroInFocusGWConc" )
}   

#'@rdname macroInFocusGWConc-methods
#'
#'@method macroInFocusGWConc character
#'
#'@export 
#'
macroInFocusGWConc.character <- function( 
 x, 
 nbYrsWarmUp = 6L, 
 years80th = 16:17, 
 negToZero = FALSE, 
 quiet = FALSE, 
 ...
){  
    # if( length( x ) > 1L ){ 
        # stop( "length( x ) > 1L. One file at a time" ) 
    # }   
    
    out <- macroutils::macroReadBin( file = x, ... ) 
    
    #   Add the file name to the table, as a column
    #   so it can be used later to identify the simulation
    if( length(x) > 1L ){
        out <- lapply(
            X   = 1:length(x), 
            FUN = function( i ){
                out_i <- out[[ i ]]
                out_i[, "file" ] <- x[ i ]
                return( out_i )
            }   
        )   
    }else{
        out[, "file" ] <- x
    }   
    
    return( macroInFocusGWConc( x = out, nbYrsWarmUp = nbYrsWarmUp, 
        years80th = years80th, quiet = quiet, ... ) ) 
}   

#'@rdname macroInFocusGWConc-methods
#'
#'@method macroInFocusGWConc list
#'
#'@export 
#'
macroInFocusGWConc.list <- function( 
 x, 
 nbYrsWarmUp = 6L, 
 years80th = 16:17, 
 negToZero = FALSE, 
 quiet = FALSE, 
 ...
){  
    #   Add the column 'file' if it is not in there yet
    x <- lapply(
        X   = 1:length( x ), 
        FUN = function( i ){
            xSubset <- x[[ i ]] 
            
            if( !("file" %in% colnames( xSubset )) ){
                xSubset[, "file" ] <- as.character( i ) 
            }   
            
            return( xSubset ) 
        }   
    )   
    
    #   Process each table one by one
    out <- lapply(
        X   = x, 
        FUN = function( xSubset ){
            return( macroInFocusGWConc( x = xSubset, 
                nbYrsWarmUp = nbYrsWarmUp, years80th = years80th, 
                quiet = quiet, ... ) )
        }   
    )   
    
    #   Recover and bind the additional attribute into 
    #   a big table
    more <- lapply(
        X   = out, 
        FUN = function(o){
            return( attr( x = o, which = "more" ) ) 
        }   
    )   
    more <- do.call( what = "rbind", args = more )
    
    #   Extract other attributes
    nbYrsWarmUp <- attr( x = out[[ 1L ]], which = "nbYrsWarmUp" ) 
    years80th   <- attr( x = out[[ 1L ]], which = "years80th" ) 
    negToZero   <- attr( x = out[[ 1L ]], which = "negToZero" ) 
    
    #   Bind the main output into a table too
    out <- do.call( what = "rbind", args = out )
    
    #   Add an attribute to the final table
    attr( x = out, which = "more" )        <- more 
    attr( x = out, which = "nbYrsWarmUp" ) <- nbYrsWarmUp
    attr( x = out, which = "years80th" )   <- years80th
    attr( x = out, which = "negToZero" )   <- negToZero

    return( out ) 
}   

#'@rdname macroInFocusGWConc-methods
#'
#'@method macroInFocusGWConc data.frame
#'
#'@export 
#'
macroInFocusGWConc.data.frame <- function( 
 x, 
 nbYrsWarmUp = 6L, 
 years80th = 16:17, 
 negToZero = FALSE, 
 quiet = FALSE, 
 ...
){  
    if( !quiet ){
        message( "WARNING: Not part of the official MACROInFOCUS program" )
        message( "  Provided only for test purpose. See help page for more information." )
        message( "  Set 'quiet' to TRUE to suppress these messages" )
    }   
    
    #   Find out the relevant column names (independently of 
    #   the layer-suffix)
    #   SR20151006: try to get rid of the arguments after 
    #   WOUT etc.
    wOutCol     <- colnames( x )[ substr( x = colnames( x ), 1, 5 ) == "WOUT_" ]
    wFlowOutCol <- colnames( x )[ substr( x = colnames( x ), 1, 9 ) == "WFLOWOUT_" ]
    sFlowCol    <- colnames( x )[ substr( x = colnames( x ), 1, 6 ) == "SFLOW_" ]
    sFlowOutCol <- colnames( x )[ substr( x = colnames( x ), 1, 9 ) == "SFLOWOUT_" ]
    
    if( length( wOutCol ) != 1L ){
        stop( "No or more than one column matching 'WOUT_'" )
    }   
    
    if( length( wFlowOutCol ) != 1L ){
        stop( "No or more than one column matching 'WFLOWOUT_'" )
    }   
    
    if( length( sFlowCol ) != 1L ){
        stop( "No or more than one column matching 'SFLOW_'" )
    }   
    
    if( length( sFlowOutCol ) != 1L ){
        stop( "No or more than one column matching 'SFLOWOUT_'" )
    }   
    
    #   Check that expected columns are present
    expectCols <- c( "Date", wOutCol, 
       wFlowOutCol, sFlowCol, 
       sFlowOutCol, "TFLOWOUT", "TSOUT", "file" )
    
    testCols <- expectCols %in% colnames( x )   
    
    if( !all( testCols ) ){
        stop( sprintf( 
            "Some expected columns are missing: %s", 
            paste( expectCols[ !testCols ], collapse = "; " ) 
        ) ) 
    }   
    
    #   De-aggregate TSOUT and TFLOWOUT
    x[, "dTSOUT" ]    <- NA_real_ 
    x[ 1L, "dTSOUT" ] <- x[ 1L, "TSOUT" ]
    x[ 2L:nrow(x), "dTSOUT" ] <- 
        x[ 2L:nrow(x), "TSOUT" ] - x[ 1L:(nrow(x)-1L), "TSOUT" ]
    
    x[, "dTFLOWOUT" ]    <- NA_real_ 
    x[ 1L, "dTFLOWOUT" ] <- x[ 1L, "TFLOWOUT" ]
    x[ 2L:nrow(x), "dTFLOWOUT" ] <- 
        x[ 2L:nrow(x), "TFLOWOUT" ] - x[ 1L:(nrow(x)-1L), "TFLOWOUT" ]
    
    #   Convert flow rates from hourly to daily
    #   Note: no quotes around sFlowCol, as it is a variable 
    #   containing the column name (and not a column  name)
    x[, "SFLOW_DAILY" ]    <- x[, sFlowCol ]    * 24
    x[, "SFLOWOUT_DAILY" ] <- x[, sFlowOutCol ] * 24
    x[, "WOUT_DAILY" ]     <- x[, wOutCol ]     * 24
    x[, "WFLOWOUT_DAILY" ] <- x[, wFlowOutCol ] * 24
    
    x[, "SFLOWTOT_DAILY"]  <- (x[, sFlowCol] + x[, sFlowOutCol]) * 24
    x[, "WFLOWTOT_DAILY"]  <- (x[, wOutCol]  + x[, wFlowOutCol]) * 24
    
    # #   Version of solute flow without negative upward flow
    # x[, "SFLOW_DAILY2b" ] <- x[, "SFLOW_DAILY" ]
    # x[ x[, "SFLOW_DAILY2b" ] < 0, "SFLOW_DAILY2b" ] <- 0
    
    # x[, "SFLOWOUT_DAILY2b" ] <- x[, "SFLOWOUT_DAILY" ]
    # x[ x[, "SFLOWOUT_DAILY2b" ] < 0, "SFLOWOUT_DAILY2b" ] <- 0
        
    #   Extract the year
    years <- format.POSIXct( x = x[, "Date" ], format = "%Y" ) 
    years <- as.integer( years ) 
    
    if( nbYrsWarmUp >= 0 ){ 
        yearsOut <- sort( unique( years ) )[ 1:nbYrsWarmUp ]    
    }   
    
    #   Remove the warm-up years
    x     <- x[ !(years %in% yearsOut), ]
    years <- years[ !(years %in% yearsOut) ]
    
    #   Check that there are indeed 20 years left
    nbYears <- length( unique( years ) )
    if( nbYears != 20 ){
        warning( sprintf( 
            "Number of years left after removing warm-up is not 20: %s", nbYears 
        ) ) 
    }   
    
    #   Aggregate water and solute flow for each year
    #   (This will accumulate all flow, for each year)
    xYearly <- aggregate(
        x   = x[, c( "dTSOUT", "dTFLOWOUT", "SFLOW_DAILY", 
            "SFLOWOUT_DAILY", "WOUT_DAILY", 
            "WFLOWOUT_DAILY", "WFLOWTOT_DAILY", "SFLOWTOT_DAILY") ], 
            # , "SFLOW_DAILY2b", "SFLOWOUT_DAILY2b"
        by  = list( "year" = years ), 
        FUN = sum 
    )   
    
    #   Add the prefix acc_ to all other columns
    colnames( xYearly )[ -1L ] <- paste( "acc", 
        colnames( xYearly )[ -1L ], sep = "_" )
    
    #   Rename the columns dTSOUT and dTFLOWOUT, as they 
    #   are now re-accumulated (per year)
    colnames( xYearly )[ colnames( xYearly ) == "acc_dTSOUT" ]    <- "TSOUT"
    colnames( xYearly )[ colnames( xYearly ) == "acc_dTFLOWOUT" ] <- "TFLOWOUT"
    
    #   Suppress the daily prefix, as variables are now 
    #   accumulated 
    colnames( xYearly ) <- gsub( x = colnames( xYearly ), 
        pattern = "_DAILY", replacement = "", fixed = TRUE )
    
    #   Calculate the concentrations
    xYearly[, "CONC_PERC" ] <- (xYearly[, "TSOUT" ] / (xYearly[, "TFLOWOUT" ] / 1000))
    
    xYearly[, "CONC_LAYER" ] <- 
        (xYearly[, "acc_SFLOW" ] + xYearly[, "acc_SFLOWOUT" ]) / 
        ((xYearly[, "acc_WOUT" ] + xYearly[, "acc_WFLOWOUT" ]) / 1000)
    
    # xYearly[, "CONC_LAYERb" ] <- 
        # (xYearly[, "SFLOW_DAILY2b" ] + xYearly[, "SFLOWOUT_DAILY2b" ]) / 
        # ((xYearly[, "WOUT_DAILY" ] + xYearly[, "WFLOWOUT_DAILY" ]) / 1000)
    
    #   Add the file name to the table:
    xYearly[, "file" ] <- x[ 1L, "file" ]
    
    #   Handle possible negative values in the concentrations
    if( negToZero ){
        xYearly[ xYearly[, "CONC_PERC"  ] < 0, "CONC_PERC"  ] <- 0
        xYearly[ xYearly[, "CONC_LAYER" ] < 0, "CONC_LAYER" ] <- 0
    }else{
        testNegPerc <- 
            any( xYearly[ order( xYearly[, "CONC_PERC" ] ),  ][ years80th, "CONC_PERC"  ] < 0 ) 
        
        testNegLayer <- 
            any( xYearly[ order( xYearly[, "CONC_LAYER" ] ), ][ years80th, "CONC_LAYER" ] < 0 ) 
        
        if( testNegPerc ){
            warning( paste(
                "Some of the concentrations used for calculating the 80th percentile are < 0", 
                "(at bottom boundary).", 
                "Estimated 80th percentiles may differ from MACROInFOCUS GUI", 
                "Consider setting 'negToZero'-argument to TRUE"
            ) )  
        }   
        
        if( testNegLayer ){
            warning( paste(
                "Some of the concentrations used for calculating the 80th percentile are < 0", 
                "(at target depth).", 
                "Estimated 80th percentiles may differ from MACROInFOCUS GUI", 
                "Consider setting 'negToZero'-argument to TRUE"
            ) ) 
        }   
    }   
    
    #   Calculate the percentile-concentrations (different 
    #   methods)
    CONC_PERC_80TH  <- mean( xYearly[ order( xYearly[, "CONC_PERC" ] ), ][ years80th, "CONC_PERC" ] ) 
    CONC_LAYER_80TH <- mean( xYearly[ order( xYearly[, "CONC_LAYER" ] ), ][ years80th, "CONC_LAYER" ] ) 
    
    # CONC_LAYER_80THb <- mean( xYearly[ order( xYearly[, "CONC_LAYERb" ] ), ][ years80th, "CONC_LAYERb" ] ) 
    
    # #   Percentile-percolation
    # #   Notice that the ranking is done according to 
    # #   concentration 2
    # p_TFLOWOUT     <- mean( xYearly[ order( xYearly[, "CONC_LAYER" ] ), ][ years80th, "TFLOWOUT" ] ) 
    # p_acc_WOUT     <- mean( xYearly[ order( xYearly[, "CONC_LAYER" ] ), ][ years80th, "acc_WOUT" ] ) 
    # p_acc_WFLOWOUT <- mean( xYearly[ order( xYearly[, "CONC_LAYER" ] ), ][ years80th, "acc_WFLOWOUT" ] )
    
    # #   Percentile of pesticide load:
    # p_TSOUT        <- mean( xYearly[ order( xYearly[, "CONC_LAYER" ] ), ][ years80th, "TSOUT" ] ) 
    # p_acc_SFLOWTOT <- rowSums( xYearly[ order( xYearly[, "CONC_LAYER" ] ), ][ years80th, c( "acc_SFLOW", "acc_SFLOWOUT" ) ] ) 
    # p_acc_SFLOWTOT <- mean( p_acc_SFLOWTOT )  
    
    #   Create a list of named values that will 
    #   contain all the percentiles calculated
    percentilesOut <- data.frame( 
        "CONC_PERC_80TH"    = CONC_PERC_80TH, 
        "CONC_LAYER_80TH"   = CONC_LAYER_80TH, 
        # "CONC_LAYER_80THb"= CONC_LAYER_80THb, 
        # "p_TFLOWOUT"      = p_TFLOWOUT, 
        # "p_acc_WOUT"      = p_acc_WOUT,
        # "p_acc_WFLOWOUT"  = p_acc_WFLOWOUT,
        # "p_TSOUT"         = p_TSOUT, 
        # "p_acc_SFLOWTOT"  = p_acc_SFLOWTOT, 
        "yearFrom"          = xYearly[ order( xYearly[, "CONC_LAYER" ] ), ][ min( years80th ), "year" ], 
        "yearTo"            = xYearly[ order( xYearly[, "CONC_LAYER" ] ), ][ max( years80th ), "year" ], 
        "file"              = x[ 1L, "file" ], 
        stringsAsFactors = FALSE 
    )   
    
    #   Format the output and output attributes
    out <- percentilesOut 
    attr( x = out, which = "more" )        <- xYearly
    attr( x = out, which = "nbYrsWarmUp" ) <- nbYrsWarmUp
    attr( x = out, which = "years80th" )   <- years80th
    attr( x = out, which = "negToZero" )   <- negToZero
    
    return( out ) 
}   
