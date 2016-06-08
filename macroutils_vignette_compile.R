
detach( package:macroutils ) 
rm(list=ls(all=TRUE)) 
    
setwd( sprintf( 
    "%s/macro-se/macroutils/pkg/macroutils/vignettes", 
    Sys.getenv("rPackagesDir") ) )

Stangle( "macroutils_vignette.Rnw" ) 

Sweave( "macroutils_vignette.Rnw" ) 

for( clean in c(FALSE,FALSE,TRUE) ){ 
    msg <- tools::texi2dvi( 
        file        = "macroutils_vignette.tex", 
        pdf         = TRUE, 
        clean       = clean, 
        texinputs   = getwd() 
    )   
    
    # if( !clean ){ 
    #     detach( package:macroutils ) 
    # }   
}   



## Copy the vignette's pdf into the 'doc' folder
file.copy( 
    from      = "macroutils_vignette.pdf", 
    to        = "../inst/doc/macroutils_vignette.pdf", 
    overwrite = TRUE )    

# file.remove( "macroutils_vignette.pdf" ) 

for( ext in c( "\\.tex$", "\\.bib.bak$", "\\.R$", "\\.aux$", 
    "\\.bbl$", "\\.blg$", "\\.log$", "\\.out$", "\\.toc$", "\\.pdf$" ) ){ 
    
    file.remove( list.files( getwd(), ext, full.names = TRUE ) ) 
}   

# file.remove( list.files( 
    # getwd(), "macroutils_vignette-", full.names = TRUE ) ) 

# library("tools") # Now use R CMD build --compact-vignette="gs" instead
# res <- compactPDF( paths = getwd(), gs_quality = "ebook" ) # paste(sep="",file.name.root,".pdf") 
# res 
