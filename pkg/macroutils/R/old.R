# +-------------------------------------------------------------+ 
# | Title:      Utilities to read, write and plot bin files     | 
# | Author:     Julien MOEYS -- SLU/CKB                         | 
# | Language:   R                                               | 
# | Contact:    Julien.Moeys@slu.se                             | 
# | License:    AGPL3 (GNU AFFERO GENERAL PUBLIC LICENSE v 3)   | 
# +-------------------------------------------------------------+ 

# source( "C:/_MACRO_SE/macroutils/pkg/macroutils/R/old.R" ) 



.MU.read.bin.old <- function(# Deprecated. Old function to read MACRO output binary files (Julien Moeys)
### Deprecated. Old function to read MACRO output binary files 
### (Julien Moeys)

##seealso<<\code{\link{macroReadBin}}.

 bin.path=getwd(),
### Single character string. Path, name and extension of the MACRO 
### binary file that will be read.

 endian=.Platform$endian,
### Single character string. See ?readBin

 date.transf=function(x,a=-48/24){as.POSIXlt("0001/01/01 00:00:00",tz="GMT") + x*60 + a*24*60*60}, 
### R function with 1 x argument. Function that will be called to 
### transform the "date" in the bin file into a real date.
### As far as I understand, the date in the bin files is in 
### minutes since 2 day before day 1 of year 1.

 robust=TRUE,
### Single logical. If TRUE (default), uses a more robust method 
### to read the file header. This mode is not always optimal as 
### it may leave some extra characters in column names (for some 
### weather files).

 remove.space=TRUE
### Single logical. If TRUE remove extra spaces in column names 
### and replace them by underscores _. Multiple spaces are grouped.

){  #
    int.byte.size   <- 4 
    int.byte.size.b <- 2 
    int.byte.size.c <- 1 
    # 
    # First "bite" of the binary file: should catch the file information
    binfile.a <- readBin( 
        con     = bin.path, 
        n       = 2, 
        what    = "integer", 
        size    = int.byte.size, 
        endian  = endian
    )   #
    #
    nb.ln <- binfile.a[1] 
    #
    nb.bt <- binfile.a[2] 
    #
    # Deduce the number of columns:
    nb.col      <- nb.bt / int.byte.size    # Each column is 4 byte wide
    nb.add.col  <- nb.col - 1               # There is always a date column at the beginning
    #
    # Second "bite" of the binary, this time trying to catch the whole
    binfile <- readBin( 
        con     = bin.path, 
        #       # The file length is 1 * nb.bt (info) + nb.ln * nb.bt (table) + nb.bt * nb.col  
        n       = (nb.bt * (1 + nb.ln) + nb.col * 60), 
        what    = "raw", 
        size    = NA, 
        endian  = endian
    )   #
    #
    # # # Read the extra info up to 1 column width equivalent (after = the real table)
    # extra <- readBin( 
    #     con  = binfile[9:nb.bt], # binfile[9:nb.bt], 
    #     n    = (nb.bt - 8) / 1, # (nb.bt - 8) / int.byte.size.b, 
    #     what = "character", 
    #     size = 1  # int.byte.size.b 
    # )   #
    # #
    # extra.start <- (2 * int.byte.size) / int.byte.size.b
    # extra       <- extra[ extra.start:length(extra.start) ] 
    # cat( paste( extra, collapse = ", " ), "\n" ) 
    #
    # Position of the first bite after the table:
    pos         <- nb.bt * (nb.ln + 1) + 1
    # 
    # Whole header data selection:
    end.sel       <- pos:length( binfile ) 
    binfile.end   <- binfile[ end.sel ] 
    binfile.end.l <- length( binfile.end ) 
    #
    # Check the places where the -65 binary end-column indicator is present:
    sel.mrk <- readBin( 
        con     = binfile.end, 
        what    = "integer", 
        n       = binfile.end.l/int.byte.size.c, 
        size    = int.byte.size.c  
        #endian = endian 
    )   #
    #
    # cat( paste(sel.mrk,collapse=","), "\n" ) 
    #
    if( robust )
    {   #
        cols.nm <- readBin( 
            con     = binfile.end, 
            what    = "character", 
            n       = binfile.end.l, # not known?
            size    = binfile.end.l  # nb.bt  
            #endian = endian 
        )   #
        #
        cols.nm  <- paste( cols.nm, collapse = "" ) 
        #
        cols.nm2 <- strsplit( x = cols.nm, split = "" )[[1]] 
        cols.nm2 <- cols.nm2[ !(cols.nm2 %in% c(letters,LETTERS,0:9," ","-",".")) ] 
        cols.nm2 <- paste( unique( cols.nm2 ), collapse = "" ) 
        #
        cols.nm  <- strsplit( 
            x = cols.nm, 
            split = paste( sep = "", "[", cols.nm2, "]" ) 
        )[[1]] 
        cols.nm  <- cols.nm[ nchar(cols.nm) >= 3 ] 
    }else{ 
        sel.mrk <- sel.mrk == -65 
        #
        sel.mrk <- (1:binfile.end.l)[ sel.mrk ] 
        #
        if( any(diff(sel.mrk) != 60 ) ) 
        {   #
            stop("the header definition width is not consistent with expected value (60). Check the proceedure")
        }   #
        #
        sel.mrk <- sel.mrk[ -length(sel.mrk) ]
        #
        vec.col <- mapply( 
            FUN  = "seq.int", 
            from = c(1, sel.mrk + 1), 
            to   = c(sel.mrk - 1,binfile.end.l), 
            SIMPLIFY = FALSE
        )   #
        #
        cols.nm <- unlist( 
            lapply( 
                X   = vec.col, 
                FUN = function(X){ 
                    readBin( 
                        con     = binfile.end[ X ], 
                        what    = "character", 
                        n       = 1, 
                        size    = length(X) # nb.bt  
                    )   #
                }   #
            )   #
        )   #
    }   #
    #
    if( remove.space ) # Replacing spaces by underscores
    {   #
        cols.nm <- gsub( pattern = "-", replacement = " ", x = cols.nm ) 
        #
        cols.nm  <- strsplit(
            x        = cols.nm, 
            split    = " ", 
            fixed    = FALSE, 
            perl     = FALSE
        )   # 
        #
        cols.nm  <- unlist( lapply( 
                X   = cols.nm, 
                FUN = function(X){ 
                    paste( X[ X != "" ], collapse = "_" ) 
                }   #
        )   )   #
    }   #
    #
    vec   <- nb.bt * (1:nb.ln) + 1
    #
    vec.a <- unlist( 
        mapply( 
            FUN  = "seq.int", 
            from = vec,
            to   = vec + 3,
            SIMPLIFY = FALSE
        )   #
    )   #
    #
    vec.b <- unlist( 
        mapply( 
            FUN  = "seq.int", 
            from = vec + 4,
            to   = vec + nb.bt - 1,
            SIMPLIFY = FALSE
        )   #
    )   #
    #
    col.Date <- readBin( 
        con  = binfile[ vec.a ], 
        n    = 1 * nb.ln, 
        what = "integer", 
        size = 4 
    )   #
    #
    tbl <- readBin( 
        con  = binfile[ vec.b ], 
        n    = nb.add.col * nb.ln, 
        what = "double", 
        size = 4 
    )   #
    #
    tbl <- matrix( 
        data  = tbl, 
        ncol  = nb.add.col, 
        nrow  = nb.ln, 
        byrow = TRUE 
    )   #
    #
    tbl <- data.frame( 
        #"Date1" = format( datez, "%Y%m%d%H%M"), 
        "Date" = format( 
            date.transf( x = col.Date ), 
            "%Y/%m/%d %H:%M:%S"
        ),  #
        tbl, 
        stringsAsFactors = FALSE 
    )   #
    #
    # ifelse( robust, cols.nm, cols.nm[-length(cols.nm)] ) 
    cols.nm <- cols.nm[ 1:nb.add.col ] 
    #
    colnames( tbl ) <- c("Date",cols.nm) # "Date2",
    #
    return( tbl ) 
### Return the binary fine in the formof an R data.frame whose 
### fisrt column ("Date") is the date in POSIXlt (unless a different 
### format is used in 'date.transf').
}   #






.read.BIN.old <- function(# Deprecated. Old function to read bin file from the Soil and MACRO models (Kristian Persson).
### Deprecated. Old function to read bin file from the Soil and 
### MACRO models (Kristian Persson).

##seealso<<\code{\link{macroReadBin}}.

 filename
### Name and eventually path of the bin file to be read.

){  #
    #Version 1.2
    # opens binary file for reading
    con = file(filename, open="rb")
    # Reads an integer (4 byte) containting the numner of records 
    record.number <- readBin(con, "int")  
    # Reads an integer (4 byte( containting the length of each record
	record.length <- readBin(con, "int")  
	# Number of variables in the file
	variables.number <- record.length / 4 -1
	# Move read position to start of first record
	pos.start <- seek(con, where=record.length)
	
	# Create a matrix to store the data
	data.values <- matrix(nrow = record.number, ncol = variables.number) 
	# Create a vector to hold the integer values representing the date 
	data.julianDate <- vector() 
	# Create a vector to hold date in POSIXct format
	data.date <- vector() 
	dateStr <- vector()
	
	
	# A double for loop to read the data
	for(row.nr in 1:record.number)
	{
		data.julianDate[row.nr] <- readBin(con, what = "int", size = 4) #date field, julian
		for(col.nr in 1:variables.number)						#value fields
		{
			data.values[row.nr, col.nr] <- readBin(con, what = "double", size = 4)
		}
	}
	
	# Read in column names
	col.Names <- vector()
	for(col.nr in 1:variables.number)
	{
		 temp <-readChar(con,52) 	#Read column names, 52 characters
		 readBin(con, what = "int", size = 4)	#4 bytes of nonessential data
		 readBin(con, what = "int", size = 4)   #4 bytes of nonessential data
 		 col.Names[col.nr]<- sub ("[ ]*$", "", temp)	#Remove trailing blanks
	}
	colnames(data.values) <- col.Names
	
	#Close file
	close(con) 
	
	
	#Julian to date
	for(row.nr in 1:record.number)
	{
		jul<-data.julianDate[row.nr]    
		mi <- jul %% (60 * 24)
		ho <- mi / 60
		mi <- mi - ho * 60
		jul <- (jul - mi - ho * 60) / 60 / 24 + 1721424
		ja <- jul
		if(jul >= 2299161)
		{
		    jalpha <- ((jul - 1867216) - 0.25) / 36524.25
		    jalpha <- floor(jalpha)
		    ja = jul + 1 + jalpha - floor(jalpha * 0.25)
		}
		jb <- ja + 1524
		jc <- floor(6680.0 + ((jb - 2439870) - 122.1) / 365.25)
		jd <- 365 * jc + floor(0.25 * jc)
		je <- floor((jb - jd) / 30.6001)
		da <- jb - jd - floor(30.6001 * je)
		mon <- je - 1
		if (mon > 12) 
		{
			mon <- mon - 12
		}
		yea <- jc - 4715
		if (mon > 2)
		{
			yea = yea - 1
		}
		if (yea <= 0)
		{
			 yea = yea - 1
		}
		dateStr[row.nr] <- paste(yea,"-",mon,"-", da, " ", ho, ":" , mi, sep="") #make a date string 
	}
	
	#Transform text format date to POSIXct
	data.date <- as.POSIXct( 
		dateStr, #vektor att transformera
		format = "%Y-%m-%d %H:%M"  #Description of current date string to convert
	)  	
	
	#Assembly the data.frame
	data.complete <- data.frame(data.date, data.values)
	#Set the column name for the date column 
	colnames(data.complete)[1] = "Date"
	
	#Return the data.frame
	return (data.complete)
### Returns a data.frame with the content of the bin file. Columns 
### names found in the bin file are returned as well. The "Date" 
### column in the bin file is converted from "Julian Date" into 
### POSIXct date format.
}   #

#     setwd( "C:/_MACRO_SE/macroutils/pkg/macroutils/inst/bintest" )
#     gc() 
#     system.time( tmp4 <- .read.BIN.old( "Fert.bin" ) ) 
#     gc() 
#     system.time( tmp4 <- .read.BIN.old( "soiln001.BIN" ) )

#     colnames( tmp3 ) <- colnames( tmp4 ) 
#     identical(tmp3,tmp4) 
#     identical(tmp3[,1],tmp4[,1]) 
#     data.frame(tmp3[1:20,1],tmp4[1:20,1]) 

#     colnames( tmp2 ) <- colnames( tmp4 ) 
#     identical(tmp2,tmp4) 
#     identical(tmp2[,1],tmp4[,1]) 
#     data.frame(tmp2[1:20,1],tmp4[1:20,1]) 






.write.BIN.old <- function(# Deprecated. Old function to write bin file for the Soil and MACRO models (Kristian Persson).

 filename, 
### Name and eventually path of the bin file to be written.

 data.to.write
### data.frame or matrix to be written in the bin file. The first 
### column must contain a date in POSIXct format.

){  #
    #Version 1.0
    #Writes the contest of a dataframe to a bin file
    
    # opens binary file for writing
    con = file(filename, open="wb")    
    
    #get size of data
    n.row <- dim(data.to.write)[1]
    n.col <- dim(data.to.write)[2]
    PostSize <- 4
    #Size of a record in bytes
    rec_length <- n.col * PostSize
    #Number of records
    rec_ant <- n.row

    #Write first record containg the number of records and the size of the records
    writeBin(rec_ant, con, size=4)
    writeBin(as.integer(rec_length), con, size=4)
    #Pad the record with zeroes
    for (zero in 1:(n.col-2))
    {
         writeBin(0, con, size=4)
    }
    
    #Save the data in the dataframe

    #Create a vector to hold dates in julian format
    data.julian <- vector() 
    #Convert date in POSIXct to julian format
    for(row.nr in 1:n.row)
    {
	#print (data.to.write[row.nr,1])
	da <- as.POSIXlt(data.to.write[row.nr,1])$mday
	mon <- as.POSIXlt(data.to.write[row.nr,1])$mon + 1
	yea <- as.POSIXlt(data.to.write[row.nr,1])$year + 1900

         ho = 12
         mi = 0
         if (yea < 0)
	 {
	    yea = yea + 1
	 }
         if (mon > 2)
	 {
             jy = yea
             jm = mon + 1
         }
	 else
         {
	    jy = yea - 1
            jm = mon + 13
	 }

         jd = floor(365.25 * jy) + floor(30.6001 * jm) + da + 1720995
         tl = da + 31 * (mon + 12 * yea)
         if (tl > 588829) 
	 {
             ja = round(0.01 * jy)
             jd = jd + 2 - ja + floor(0.25 * ja)
         }
         jd = jd - 1721424
         jd = jd * 24 * 60 + ho * 60 + mi
         data.julian[row.nr] <- jd
      }

    # A double for loop to write the data
	for(row.nr in 1:n.row)
	{
		writeBin(as.integer(data.julian[row.nr]), con, size=4) #date field, julian
		for(col.nr in 1:(n.col-1))#value fields
		{
    		writeBin(data.to.write[row.nr, col.nr+1], con, size=4)
		}
	}
    
	# Write column names
	col.Names <- colnames(data.to.write) 
	print (col.Names)
	for(col.nr in 2:n.col) #First column is date time and has no columnname
	{	
		print (col.Names[col.nr])
#  		 writeChar(col.Names[col.nr], con, eos=NULL) 	#Write column names, padded to 52 characters
		writeChar(sprintf("%-52s", col.Names[col.nr]), con, eos=NULL)

		 writeBin(0, con, size=4)	#4 bytes of nonessential data
		 writeBin(0, con, size=4)   #4 bytes of nonessential data
	}
	

	#Close file
	close(con) 
    
}


