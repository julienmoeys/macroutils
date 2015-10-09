
library( "macroutils" ) 

#   Maximum differences acceptable in conc
maxConcDif <- 1.6e-06
maxPercDif <- 1.7e-04

#   Path to the file to be read
( filenm <- system.file( "bintest/MACRO001_20151005.BIN", 
    package = "macroutils", mustWork = TRUE ) )

res <- macroInFocusGWConc( x = filenm ) 

res 

attr( res, "more" ) 



# Test concentrations
# ==========================================================

#   Import the corresponding file produced by MACROInFOCUS
rdsConc <- system.file( "bintest/macroInFocus/conc005.rds", 
    package = "macroutils", mustWork = TRUE ) 

#   Format the name of the expected RDS files
conc <- readRDS( file = rdsConc )

#   Eliminate warm-up
conc <- conc[ -c(1:6), ]

#   Calculate the absolute differences
CONC_LAYER <- attr( res, "more" )[, "CONC_LAYER" ] 

concDiffs <- conc[, "Av_FluxConc_at_reporting_depth" ] - 
    CONC_LAYER

concDiffs <- abs( concDiffs ) 

xyRange <- range( c( CONC_LAYER, 
    conc[, "Av_FluxConc_at_reporting_depth" ] ) )

plot( CONC_LAYER ~ 
    conc[, "Av_FluxConc_at_reporting_depth" ], xlim = xyRange, 
    ylim = xyRange ) 
abline( a = 0, b = 1, col = "red" ) 

#   Compare the two side by side
cbind( CONC_LAYER, conc[, "Av_FluxConc_at_reporting_depth" ] )

#   It seems negative values are set to 0 in MACROInFOCUS
#   Set them to 0 in macroutils too

CONC_LAYER[ CONC_LAYER < 0 ] <- 0

#   Calculate the absolute differences again
concDiffs <- conc[, "Av_FluxConc_at_reporting_depth" ] - 
    CONC_LAYER

concDiffs <- abs( concDiffs ) 

#   Test that the differences are not too big
if( any( concDiffs > maxConcDif ) ){
    stop( sprintf( 
        "Some differences bigger than the maximum acceptable difference: %s > %s (file: conc005.rds)", 
        max( concDiffs ), maxConcDif 
    ) ) 
}   



# Test percolation
# ==========================================================

#   Import the corresponding file produced by MACROInFOCUS
rdsPerc <- system.file( "bintest/macroInFocus/perc005.rds", 
    package = "macroutils", mustWork = TRUE ) 

#   Format the name of the expected RDS files
perc <- readRDS( file = rdsPerc )

#   Eliminate warm-up
perc <- perc[ -c(1:6), ]

#   Calculate the absolute differences
percDiffs <- perc[, "Percolation_at_reporting_depth" ] - 
    attr( res, "more" )[, "acc_WFLOWTOT" ]

percDiffs <- abs( percDiffs ) 

#   Test that the differences are not too big
if( any( percDiffs > maxPercDif ) ){
    stop( sprintf( 
        "Some differences bigger than the maximum acceptable difference: %s > %s (file: conc005.rds)", 
        max( concDiffs ), maxConcDif 
    ) ) 
}   
