
#   Location of the working directory
wd <- setwd( sprintf( 
    "D:/Users/%s/Documents/_WORKS/_PROJECTS/r_packages/macro-se/macroutils_www", 
    Sys.info()[[ "user" ]] ) ) 

pkg <- "macroutils"

# setwd( wd <- file.path( "D:/Users/julienm/Documents/_WORKS/_PROJECTS/r_packages", pkg, "www", pkg ) )

descNews <- c( "DESCRIPTION", "NEWS" )
index    <- c( "00Index.html", "index.html" )

library( "knitr" ) 

library( pkg, character.only = TRUE ) 

knit_rd( pkg = pkg, frame = TRUE ) 

file.copy(
    from = system.file( descNews, package = pkg ), 
    to   = descNews, 
    overwrite = TRUE 
)   

for( i in index ){ 
    index.html <- readLines( i )
    index.html <- gsub( x = index.html, pattern = "../", replacement = "", 
        fixed = TRUE ) 
    writeLines( text = index.html, con = i ) 
    rm( index.html )
}   

#   Remove the user and computer name from html files
change <- c( "login", "user", "effective_user", "nodename" )    

#   *   List html files:
htmlFiles <- list.files( pattern = ".html" ) 

#   *   Replace the content
for( ch in change ){ 
    chOld <- Sys.info()[[ ch ]]
    chNew <- paste( rep( "*", nchar(chOld) ), collapse = "" )
    
    for( f in htmlFiles ){
        htmlFile <- readLines( f )
        htmlFile <- gsub( x = htmlFile, 
            pattern = chOld, replacement = chNew, 
            fixed = TRUE ) 
        writeLines( text = htmlFile, con = f ) 
    }   
}   

rm( htmlFile, htmlFiles, ch, f )
