
rm(list=ls(all=TRUE)) 
pkgName     <- "macroutils"
pkgDir      <- file.path( 
    "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages/macro-sp/", 
    pkgName, "pkg" ) 


library( "roxygen2" )


# Source some utility functions
source( file.path( pkgDir, "..", "packageUtilities.R" ) ) 


# Change the description file:
pkgDescription( 
    pkgName     = pkgName, 
    pkgDir      = file.path( pkgDir ), 
    pkgVersion  = "1.8.1", 
    pkgDepends  = NULL, 
    pkgSuggests = c( "tcltk", "RODBC", "tools" ), 
    RVersion    = NULL   
)   



roxygenize( 
    package.dir   = file.path( pkgDir, pkgName ), 
    # unlink.target = TRUE, 
    roclets       = c( "namespace", "rd" ) # "collate" 
)   


pkgRemove( pkgName = pkgName ) 
