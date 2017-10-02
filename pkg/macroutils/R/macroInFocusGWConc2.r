
# +-------------------------------------------------------------+ 
# | Package:    See 'Package' in file ../DESCRIPTION            | 
# | Author:     Julien MOEYS                                    | 
# | Language:   R                                               | 
# | Contact:    See 'Maintainer' in file ../DESCRIPTION         | 
# | License:    See 'License' in file ../DESCRIPTION            | 
# +-------------------------------------------------------------+ 



#' INTERNAL/NON-OFFICIAL: Calculate the yearly and Xth percentile groundwater concentration from a MACROInFOCUS output.
#'
#' INTERNAL & NON-OFFICIAL: Calculate the yearly and Xth percentile 
#'  groundwater concentration from a MACROInFOCUS output. 
#'  \bold{WARNING} This function is \bold{not} part 
#'  of the official MACROInFOCUS program. It is provided 
#'  for test-purpose, without any guarantee or support from 
#'  the authors, CKB or SLU. You are strongly recommended to 
#'  benchmark the function against a range of (official) 
#'  MACROInFOCUS simulation results, before you use the 
#'  function. You are also strongly recommended to inspect 
#'  the code of these functions before you use them. To 
#'  inspect the content of these functions, simply type 
#'  \code{body( macroutils:::macroInFocusGWConc2.data.frame )} 
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
#'  Julien Moeys \email{jules_m78-soiltexture@@yahooDOTfr}, 
#'  contributions from Stefan Reichenberger 
#'  \email{SReichenberger@@knoellDOTcom}.
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
#'  A default of 6 years of warn-up are used in FOCUS.
#'
#'@param yearsAvg
#'  Single integer values: Number of simulation years to 
#'  "aggregate" when calculating yearly- or biennial- or 
#'  triennial- (etc.) average concentrations. 
#'  If \code{yearsAvg=1}, the function calculates yearly-
#'  average concentrations before calculating the Xth 
#'  worst-case percentile. If \code{yearsAvg=2}, the function 
#'  calculates biennial-average concentrations. If 
#'  \code{yearsAvg=3}, the function calculates 
#'  triennial-average concentrations (etc.). The default in 
#'  FOCUS is to calculate yearly avegares when the pesticide 
#'  is applied yearly, biennial-averages when the pesticide 
#'  is applied every two years and triennial averages when 
#'  the pesticide is applied every three years. The function 
#'  does not automatically set or control this parameter, 
#'  and the default value is 1: if you simulate biennial 
#'  or triennial concentrations, you need to change this 
#'  parameter accordingly.
#'
#'@param prob
#'  Single numeric value, between 0 and 1. Probability 
#'  (percentile/100) of the worst case concentration 
#'  that shall be calculated. In FOCUS, the yearly results 
#'  are ordered by increasing yearly average concentrations 
#'  before the percentile is calculated, as the average 
#'  concentration of the two years closest to the Xth percentile 
#'  (X always being 80, in FOCUS). Here, in practice, the 
#'  index of the 1st and 2nd year used for calculating the 
#'  average are selected as follow: 
#'  \code{min_index = floor(prob *(number of sim years used))} and 
#'  \code{max_index = ceiling(prob *(number of sim years used))},
#'  but in cases \code{min_index} is indentical to \code{max_index}, 
#'  then \code{max_index} is defined as \code{min_index + 1}, 
#'  unless \code{prob} is 0 or 1 (to get the minimum 
#'  or the maximum yearly concentrations, respectively). 
#'  The number of simulatiob years used is equal to the total 
#'  number of simulation years in \code{x} minus 
#'  \code{nbYrsWarmUp}. In practice, what is calculated 
#'  "a la FOCUS", when \code{prob = 0.8}, is an average 
#'  between the 80th and the 85th percentile yearly 
#'  concentrations. See FOCUS Groundwater main report p 475 
#'  (reference above). Notice that the algorithm also calculates 
#'  a Xth percentile concentration (\code{x = prob * 100}) 
#'  using R function \code{\link[stats]{quantile}}, with 
#'  its default parametrisation and quantile-calculation 
#'  method (Note: see the help page of the function if you 
#'  are interested to see how that percentile is obtained).
#'
#'@param negToZero
#'  Single logical value. If \code{TRUE} (not the default) 
#'  negative concentrations will be set to 0 (presumably like 
#'  in MACROInFOCUS). If \code{FALSE}, they will not be set 
#'  to 0, but if some of the concentrations used to calculate 
#'  the Xth percentile (see \code{yearsXth}) are negative, 
#'  a warning will be issued (so that the user knows that 
#'  concentrations may differ from those in MACROInFOCUS).
#'
#'@param quiet
#'  Single logical value. Set to \code{TRUE} to suppress the 
#'  warning message. Default value to \code{FALSE}.
#'
#'@param \dots
#'  Additional parameters passed to 
#'  \code{\link[macroutils]{macroReadBin}}, when \code{x} is 
#'  a character string naming one or several files to be 
#'  imported. Not used otherwise.
#'
#'
#'@return 
#'  Returns a \code{\link{data.frame}} with one row per 
#'  input file (or table), and the following columns
#'  \itemize{
#'    \item \code{CONC_PERC_XTH1}: The FOCUS-like 
#'      Xth percentile of the yearly, binennial or triennial 
#'      pesticide concentration in the water percolating 
#'      at the lower boundary of the soil. Derived from the 
#'      columns \code{TSOUT} and \code{TFLOWOUT}. Corresponds 
#'      to \code{CONC_PERC} in the table of yearly concentrations 
#'      (see below).
#'    \item \code{CONC_LAYER_XTH1}: The FOCUS-like 
#'      Xth percentile of the yearly, binennial or triennial 
#'      pesticide concentration in the water flowing 
#'      through layer T (T depending on the FOCUS-scenario, 
#'      also called "target depth" in MACROInFOCUS)  
#'      at depth Y. Derived from \code{SFLOW_}, 
#'      \code{SFLOWOUT_}, \code{WOUT_} and \code{WFLOWOUT_}, 
#'      after converting these variables to daily values and 
#'      cumulating them for each year. Corresponds 
#'      to \code{CONC_LAYER} in the table of yearly concentrations 
#'      (see below). 
#'    \item \code{avgPerFrom}: The year-index corresponding to the 
#'      Xth-percentile concentration after 
#'      ordering them by order of increasing \code{CONC_LAYER}.
#'    \item \code{avgPerTo}: The year-index corresponding to the 
#'      Xth-percentile concentration after 
#'      ordering them by order of increasing \code{CONC_LAYER}.
#'    \item \code{F_SOL_LAYER_MIC_XTH1}: Experimental. The average 
#'      mass-fraction of the total solute flow due to transport 
#'      in the micropores, for the same two years as used 
#'      to calculate the Xth percentile of 
#'      \code{CONC_LAYER_XTH1}.
#'    \item \code{F_SOL_LAYER_MIC_XTH1}: Experimental. The average 
#'      mass-fraction of the total solute flow due to transport 
#'      in the macropores, for the same two years as used 
#'      to calculate the Xth percentile of 
#'      \code{CONC_LAYER_XTH1}.
#'    \item \code{CONC_PERC_XTH2}: Same as 
#'      \code{CONC_PERC_XTH1}, except that the percentile 
#'      is calculated with R \code{\link[stats]{quantile}}-function.
#'    \item \code{CONC_LAYER_XTH2}: Same as 
#'      \code{CONC_LAYER_XTH1}, except that the percentile 
#'      is calculated with R \code{\link[stats]{quantile}}-function.
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
#'@example inst/examples/macroInFocusGWConc2-examples.r
#'
#'@rdname macroInFocusGWConc2-methods
#'
#'@keywords internal
#'
#'@export
#'
#'@importFrom stats quantile
macroInFocusGWConc2 <- function( 
    x, 
    nbYrsWarmUp = 6L, 
    yearsAvg = 1, 
    prob = 0.8, 
    negToZero = FALSE, 
    quiet = FALSE, 
    ...
){  
    UseMethod( "macroInFocusGWConc2" )
}  



#'@rdname macroInFocusGWConc2-methods
#'
#'@method macroInFocusGWConc2 character
#'
#'@export 
#'
macroInFocusGWConc2.character <- function( 
    x, 
    nbYrsWarmUp = 6L, 
    yearsAvg = 1,  
    prob = 0.8, 
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
    
    return( macroInFocusGWConc2( x = out, nbYrsWarmUp = nbYrsWarmUp, 
        yearsAvg = yearsAvg, prob = prob, negToZero = negToZero, 
        quiet = quiet, ... ) ) 
}   



#'@rdname macroInFocusGWConc2-methods
#'
#'@method macroInFocusGWConc2 list
#'
#'@export 
#'
macroInFocusGWConc2.list <- function( 
    x, 
    nbYrsWarmUp = 6L, 
    yearsAvg = 1,  
    prob = 0.8, 
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
            return( macroInFocusGWConc2( x = xSubset, 
                nbYrsWarmUp = nbYrsWarmUp, yearsAvg = yearsAvg, 
                prob = prob, negToZero = negToZero, quiet = quiet, ... ) )
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
    yearsXth   <- attr( x = out[[ 1L ]], which = "yearsXth" ) 
    negToZero   <- attr( x = out[[ 1L ]], which = "negToZero" ) 
    
    #   Bind the main output into a table too
    out <- do.call( what = "rbind", args = out )
    
    #   Add an attribute to the final table
    attr( x = out, which = "more" )        <- more 
    attr( x = out, which = "nbYrsWarmUp" ) <- nbYrsWarmUp
    attr( x = out, which = "yearsXth" )    <- yearsXth
    attr( x = out, which = "negToZero" )   <- negToZero

    return( out ) 
}   

#'@rdname macroInFocusGWConc2-methods
#'
#'@method macroInFocusGWConc2 data.frame
#'
#'@export 
#'
#'@importFrom stats aggregate
macroInFocusGWConc2.data.frame <- function( 
    x, 
    nbYrsWarmUp = 6L, 
    yearsAvg = 1,  # 1 = 1 year averaging, 2 = 2 year averaging, etc. 
    prob = 0.8, 
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
    
    if( !("file" %in% colnames( x )) ){
        x[, "file" ] <- as.character( NA )
    }   
    
    #   Check that expected columns are present
    expectCols <- c( "Date", wOutCol, 
       wFlowOutCol, sFlowCol, 
       sFlowOutCol, "TFLOWOUT", "TSOUT" ) # , "file"
    
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
    xOriginal <- x 
    x     <- x[ !(years %in% yearsOut), ]
    years <- years[ !(years %in% yearsOut) ]
    
    #   Check that 'yearsAvg' is correct:
    if( (yearsAvg != (yearsAvg %/% 1)) | (yearsAvg < 1) ){
        stop( sprintf( "'yearsAvg' must be an integer and >= 1, now: %s", yearsAvg ) )
    }   
    
    #   Check that there are indeed 20 years left
    nbYears <- length( unique( years ) )
    
    #   Calculate how many averaging periods there will be:
    nbAvgPer <- nbYears / yearsAvg
    
    #   Check that 'yearsAvg' is correct:
    if( (nbAvgPer != (nbAvgPer %/% 1)) ){
        stop( sprintf( 
            "'yearsAvg' (%s) must be a multiple of the total number of simulation year (%s) minus the number of warmup years (%s).", 
            yearsAvg, nbYears + length( yearsOut ), nbYrsWarmUp ) )
    }   
    
    # #   Check that 'yearsAvg' is correct:
    # if( (nbAvgPer != (nbAvgPer %/% 1)) ){
        # warning( sprintf( 
            # "Number of simulation years (%s) divided by 'yearsAvg' (%s) is not an integer (%s)", 
            # nbYears, 
            # yearsAvg, 
            # nbAvgPer ) )
    # }   
    
    #   Averaging periods (vector):
    avgPer <- rep( 1:nbAvgPer, each = ceiling( yearsAvg ) )
    avgPer <- avgPer[ 1:nbYears ]
    
    yearToAvgPer <- data.frame(
        "year"   = unique( years ),
        "avgPer" = avgPer
    )   
    rownames( yearToAvgPer ) <- as.character( unique( years ) )
    
    x[, "year" ]   <- years 
    x[, "avgPer" ] <- yearToAvgPer[ 
        as.character( x[, "year" ] ), 
        "avgPer" ]
    
    rm( years, yearToAvgPer, avgPer )
    
    #   Conversion table from averaging period to text 
    #   (year from - year to)
    outAvgPer <- data.frame( 
        "avgPer"   = NA_integer_, 
        "yearFrom" = NA_integer_, 
        "yearTo"   = NA_integer_ ) 
    
    avgPer2Range <- lapply(
        X   = split( x = x, f = x[, "avgPer" ] ), 
        FUN = function(sx){
            outAvgPer0 <- outAvgPer
            outAvgPer0[, "yearFrom" ] <- min( sx[, "year" ] ) 
            outAvgPer0[, "yearTo" ]   <- max( sx[, "year" ] ) 
            return( outAvgPer0 )
        }   
    )   
    
    avgPer2Range <- do.call( what = "rbind", args = avgPer2Range  ) 
    
    avgPer2Range[, "avgPer" ] <- unique( x[, "avgPer" ] ) 
    # rownames( avgPer2Range )  <- as.character( avgPer2Range[, "avgPer" ] )
    
    #   Aggregate water and solute flow for each averaging period
    #   (This will accumulate all flow, for each averaging period)
    xPeriod <- stats::aggregate(
        x   = x[, c( "dTSOUT", "dTFLOWOUT", "SFLOW_DAILY", 
            "SFLOWOUT_DAILY", "WOUT_DAILY", 
            "WFLOWOUT_DAILY", "WFLOWTOT_DAILY", "SFLOWTOT_DAILY") ], 
            # , "SFLOW_DAILY2b", "SFLOWOUT_DAILY2b"
        by  = list( "avgPer" = x[, "avgPer" ] ), 
        FUN = sum 
    )   
    
    #   Add the prefix acc_ to all other columns
    colnames( xPeriod )[ -1L ] <- paste( "acc", 
        colnames( xPeriod )[ -1L ], sep = "_" )
    
    #   Rename the columns dTSOUT and dTFLOWOUT, as they 
    #   are now re-accumulated (per averaging period)
    colnames( xPeriod )[ colnames( xPeriod ) == "acc_dTSOUT" ]    <- "TSOUT"
    colnames( xPeriod )[ colnames( xPeriod ) == "acc_dTFLOWOUT" ] <- "TFLOWOUT"
    
    #   Suppress the daily prefix, as variables are now 
    #   accumulated 
    colnames( xPeriod ) <- gsub( x = colnames( xPeriod ), 
        pattern = "_DAILY", replacement = "", fixed = TRUE )
    
    #   Add the year range (min -  max) to the table
    xPeriod <- merge(
        x     = xPeriod, 
        y     = avgPer2Range, 
        by    = "avgPer", 
        all.x = TRUE 
    )   
    rm( avgPer2Range )
    
    #   Calculate the concentrations
    xPeriod[, "CONC_PERC" ] <- (xPeriod[, "TSOUT" ] / (xPeriod[, "TFLOWOUT" ] / 1000))
    
    xPeriod[, "CONC_LAYER" ] <- 
        (xPeriod[, "acc_SFLOW" ] + xPeriod[, "acc_SFLOWOUT" ]) / 
        ((xPeriod[, "acc_WOUT" ] + xPeriod[, "acc_WFLOWOUT" ]) / 1000)
    
    xPeriod[, "F_SOL_LAYER_MIC" ] <- 
        xPeriod[, "acc_SFLOW" ] / 
        (xPeriod[, "acc_SFLOW" ] + xPeriod[, "acc_SFLOWOUT" ]) 
    
    xPeriod[, "F_SOL_LAYER_MAC" ] <- 
        xPeriod[, "acc_SFLOWOUT" ] / 
        (xPeriod[, "acc_SFLOW" ] + xPeriod[, "acc_SFLOWOUT" ]) 
    
    #   Add the file name to the table:
    xPeriod[, "file" ] <- x[ 1L, "file" ]
    
    #   Define the two years for the percentile calculation
    if( (prob < 0) | (prob > 1) ){
        stop( sprintf( "'prob' (%s) should be a number >= 0 and <= 1", prob ) )
    }   
    
    yearsXth <- prob * nbAvgPer
    yearsXth <- c( floor(yearsXth), ceiling(yearsXth) )
    if( yearsXth[ 1L ] == yearsXth[ 2L ] ){ 
        if( (prob != 0) & (prob != 1) ){
            yearsXth[ 2L ] <- yearsXth[ 1L ] + 1L 
        }   
    }   
    
    #   Handle possible negative values in the concentrations
    if( negToZero ){
        xPeriod[ xPeriod[, "CONC_PERC"  ] < 0, "CONC_PERC"  ] <- 0
        xPeriod[ xPeriod[, "CONC_LAYER" ] < 0, "CONC_LAYER" ] <- 0
        
    }else{
        testNegPerc <- 
            any( xPeriod[ order( xPeriod[, "CONC_PERC" ] ),  ][ yearsXth, "CONC_PERC"  ] < 0 ) 
        
        testNegLayer <- 
            any( xPeriod[ order( xPeriod[, "CONC_LAYER" ] ), ][ yearsXth, "CONC_LAYER" ] < 0 ) 
        
        if( testNegPerc ){
            warning( paste(
                sprintf( "Some of the concentrations used for calculating the %sth percentile are < 0", prob * 100 ), 
                "(at bottom boundary).", 
                sprintf( "Estimated %sth percentiles may differ from MACROInFOCUS GUI", prob * 100 ), 
                "Consider setting 'negToZero'-argument to TRUE"
            ) )  
        }   
        
        if( testNegLayer ){
            warning( paste(
                sprintf( "Some of the concentrations used for calculating the %sth percentile are < 0", prob * 100 ), 
                "(at target depth).", 
                sprintf( "Estimated %sth percentiles may differ from MACROInFOCUS GUI", prob * 100 ), 
                "Consider setting 'negToZero'-argument to TRUE"
            ) ) 
        }   
    }   
    
    #   Calculate the percentile-concentrations (different 
    #   methods)
    CONC_PERC_XTH1  <- mean( xPeriod[ order( xPeriod[, "CONC_PERC" ] ), ][ yearsXth, "CONC_PERC" ] ) 
    CONC_LAYER_XTH1 <- mean( xPeriod[ order( xPeriod[, "CONC_LAYER" ] ), ][ yearsXth, "CONC_LAYER" ] ) 
    
    CONC_PERC_XTH2  <- as.numeric( quantile( xPeriod[, "CONC_PERC" ],  probs = prob ) )
    CONC_LAYER_XTH2 <- as.numeric( quantile( xPeriod[, "CONC_LAYER" ], probs = prob ) )
    
    F_SOL_LAYER_MAC_XTH1 <- mean( xPeriod[ order( xPeriod[, "CONC_LAYER" ] ), ][ yearsXth, "F_SOL_LAYER_MAC" ] ) 
    F_SOL_LAYER_MIC_XTH1 <- mean( xPeriod[ order( xPeriod[, "CONC_LAYER" ] ), ][ yearsXth, "F_SOL_LAYER_MIC" ] ) 
    
    #   Create a list of named values that will 
    #   contain all the percentiles calculated
    percentilesOut <- data.frame( 
        "CONC_PERC_XTH1"    = CONC_PERC_XTH1, 
        "CONC_LAYER_XTH1"   = CONC_LAYER_XTH1, 
        "avgPerFrom"        = xPeriod[ order( xPeriod[, "CONC_LAYER" ] ), ][ min( yearsXth ), "avgPer" ], 
        "avgPerTo"          = xPeriod[ order( xPeriod[, "CONC_LAYER" ] ), ][ max( yearsXth ), "avgPer" ], 
        "F_SOL_LAYER_MAC_XTH1" = F_SOL_LAYER_MAC_XTH1, 
        "F_SOL_LAYER_MIC_XTH1" = F_SOL_LAYER_MIC_XTH1, 
        "CONC_PERC_XTH2"     = CONC_PERC_XTH2, 
        "CONC_LAYER_XTH2"    = CONC_LAYER_XTH2, 
        "file"              = x[ 1L, "file" ], 
        stringsAsFactors = FALSE 
    )   
    
    #   Format the output and output attributes
    out <- percentilesOut 
    attr( x = out, which = "more" )        <- xPeriod
    attr( x = out, which = "nbYrsWarmUp" ) <- nbYrsWarmUp
    attr( x = out, which = "yearsXth" )    <- yearsXth
    attr( x = out, which = "negToZero" )   <- negToZero
    
    return( out ) 
}   
