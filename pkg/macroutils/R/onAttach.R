
# +-------------------------------------------------------------+ 
# | Title:      Utilities to read, write and plot bin files     | 
# | Author:     Julien MOEYS -- SLU/CKB                         | 
# | Language:   R                                               | 
# | Contact:    Julien.Moeys@slu.se                             | 
# | License:    AGPL3 (GNU AFFERO GENERAL PUBLIC LICENSE v 3)   | 
# +-------------------------------------------------------------+ 



#'@importFrom utils packageVersion
NULL

.onAttach <- function(# Internal. Message displayed when loading the package.
 libname, 
 pkgname  
){  
    muPar( "timeSeriesValid" = isValidTimeSeries ) 
    
    # Welcome message
    if( interactive() ){ 
        gitVersion <- system.file( "GIT_VERSION", package = pkgname ) 
        
        if( gitVersion != "" ){ 
            gitVersion <- readLines( con = gitVersion )[ 1L ] 
            gitVersion <- strsplit( x = gitVersion, split = " ", 
                fixed = TRUE )[[ 1L ]][ 1L ]
            
            gitVersion <- sprintf( "(git revision: %s)", gitVersion ) 
        }else{ 
            gitVersion <- "(git revision: ?)" 
        }   
        
        msg <- sprintf( 
            "%s %s %s. For help type: help(pack='%s')", 
            pkgname, 
            as.character( utils::packageVersion( pkgname ) ), 
            gitVersion, # svnVersion
            pkgname ) 
        
        packageStartupMessage( msg ) 
    }   
}   


