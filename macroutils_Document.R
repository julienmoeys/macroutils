
rm(list=ls(all=TRUE)) 
pkgName     <- "macroutils"
pkgDir      <- file.path( sprintf( 
    "F:/Users/%s/Documents/_r-packages", 
    Sys.info()[[ "user" ]] 
    ), pkgName, "pkg" ) 


library( "roxygen2" )


# Source some utility functions
source( file.path( pkgDir, "..", "packageUtilities.R" ) ) 


# Change the description file:
pkgDescription( 
    pkgName     = pkgName, 
    pkgDir      = file.path( pkgDir ), 
    pkgVersion  = "1.15.0", 
    pkgDepends  = "utils", # Must be in "Depends" as choose.files not available on Unix
    pkgImports  = c( "tcltk", "tools", "graphics", "grDevices", "stats" ), 
    pkgSuggests = c( "RODBC" ), 
    RVersion    = NULL 
)   



roxygenize( 
    package.dir   = file.path( pkgDir, pkgName ), 
    # unlink.target = TRUE, 
    roclets       = c( "namespace", "rd" ) # "collate" 
)   


pkgRemove( pkgName = pkgName ) 

