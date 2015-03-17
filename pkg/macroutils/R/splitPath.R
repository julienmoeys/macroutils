
.pathSplit <- function(# Split paths into single items, folder(s) or file name(s)
### Split paths into single items, folder(s) or file name(s)
 
 p, 
### Vector of character strings. Paths.
 
 fsep = NULL  
### Vector of character strings. File separators accounted for. 

){  
    if( is.null(fsep) ){ fsep <- c("/","\\") } 
    
    # Strip the file path 
    p <- lapply( X  = p, FUN = function(X){ 
        for( fs in fsep ){ 
            X <- unlist( strsplit(
                x     = X, 
                split = fs, 
                fixed = TRUE
            ) ) 
        }   
        
        return( X[ nchar(X) != 0 ] ) 
    } ) 
    
    return( p ) 
### Returns a list of vector of character strings, of the same 
### length as \code{p}  
}   




.pathLastItem <- function(# Returns the last item in a path
### Returns the last item in a path
 
 p, 
### Vector of character strings. Paths.
 
 fsep = NULL, 
### Vector of character strings. File separators accounted for. 

 noExt=NULL
### Single character string. Extension to be removed from the 
### last item. For example \code{noExt = ".txt"} 
 
){  
    # Strip the file path 
    p <- .pathSplit( p, fsep = fsep )
    
    # Remove the last bit (presumably the file name) 
    p <- lapply( X = p, FUN = function(X){ X[ length(X) ] } ) 
    
    # Remove the file extension
    if( !is.null( noExt ) ){ 
        p <- lapply( X   = p, FUN = function(X){ 
            for( noE in noExt ){ 
                X <- unlist( strsplit(
                    x     = X, 
                    split = noE, 
                    fixed = TRUE
                ) ) 
            }   
            
            return( X ) 
        } )   
    }   
    
    return( unlist( p ) ) 
### Returns path without the file name at the end.
}   




.pathNoLastItem <- function(# Returns a path without its last item
### Returns the last item in a path
 
 p, 
### Vector of character strings. Paths.
 
 fsep = NULL, 
### Vector of character strings. File separators accounted for. 
 
 collapse=.Platform$file.sep, 
### Final file separator to be used
 
 normalise=TRUE, 
### Single logical value. If \code{TRUE}, \code{\link[base]{normalizePath}} 
### is ran on the paths.
 
 mustWork = FALSE
### See \code{\link[base]{normalizePath}}.
 
){  
    # Strip the file path 
    p <- .pathSplit( p, fsep = fsep )
    
    # Remove the last bit (presumably the file name) 
    p <- lapply( X = p, FUN = function(X){ 
        X <- X[ -length(X) ] 
        
        # Concatenate again the file path
        X <- paste( X, collapse = collapse ) 
        
        return( X )
    } ) 
    
    
    # Normalise the paths: 
    if( normalise ){ p <- normalizePath( unlist( p ), mustWork = mustWork ) } 
    
    
    return( p ) 
### Returns path without the file name at the end.
}   

