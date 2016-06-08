library( "macroutils" )


# ====== Write a binary file in SOIL or MACRO style ======

# 1.1. Read a table, that will then be written back


#   Path to the file to be read
( filenm <- system.file( "bintest/METFILE.BIN", 
    package = "macroutils", mustWork = TRUE ) )

#   Read the file
tmp1 <- macroReadBin( file = filenm ) 


# 1.2. Generate a dummy temporary file where the table will be 
#   written

( filenm <- tempfile(  ) )


# 1.3. Write this table in SOIL or MACRO bin style 

#   NB: This table is NOT a standard SOIL or MACRO input file!

macroWriteBin( file = filenm, x = tmp1 ) 

#   NB: When writing the bin file, time zones are ignored!



# 1.4. Read that file again and check that it is the same:

tmp1.b <- macroReadBin( file = filenm ) 

# Maximum difference (small numerical differences)
unlist( lapply(
    X      = colnames(tmp1), 
    FUN    = function(X){ 
        max( tmp1[,X] - tmp1.b[,X] ) 
    }   
) ) 



# ====== Miscellaneous (developers niche) ======

#   Comparison with the initial implementation of the R code 
#   Note: programmers playground

filenm2 <- tempfile(  ) 
filenm2 

#   Write the table a second time
macroutils:::.write.BIN.old( 
    filename = filenm2, 
    data.to.write = tmp1 )   

#   Read the table a second time:
tmp1.c <- macroutils:::.read.BIN.old( 
    filename = filenm2 ) 

#   Compare the 2 file written / read:
identical( tmp1.b, tmp1.c ) 

data.frame( "A"=tmp1[,1], "B"=tmp1.b[,1], "C"=tmp1.c[,1] )

# Maximum difference (small numerical differences)
unlist( lapply(
    X      = colnames(tmp1), 
    FUN    = function(X){ 
        max( tmp1.c[,X] - tmp1.b[,X] ) 
    }   
) ) 

#   NB: difference in time zone! + real difference in time.


#   Now compare the bin file themselves
tmp1a <- readBin( con = filenm, what = "raw", n = 1000000 ) 
tmp2a <- readBin( con = filenm2, what = "raw", n = 1000000 ) 


#   FALSE: There is a difference in the min - max after the column name
#   + eventual differences in time if not 12:00 
identical( tmp1a, tmp2a ) 

test <- tmp1a != tmp2a 
readBin( con = tmp1a[ test ], what = "double", size = 4, n = sum(test) ) 
readBin( con = tmp2a[ test ], what = "double", size = 4, n = sum(test) ) 

# Remove the temporary file
file.remove( filenm ) 
file.remove( filenm2 ) 
