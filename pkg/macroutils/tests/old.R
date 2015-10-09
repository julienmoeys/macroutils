

### -------------------------------------------------------------
### Old help file from read.BIN (now = .read.BIN.old())
### hidden function 

# get the function:
.read.BIN.old <- macroutils:::.read.BIN.old 


filename <- system.file( "bintest/Fert.bin", 
    package = "macroutils", mustWork = TRUE ) 

data <- .read.BIN.old( filename )

#List column names
colnames(data)

#Plot Actual plant uptake vs date
plot(data[,1], data[,5], type = "b" ) 



### -------------------------------------------------------------
### Old help file from MU.read.bin (now = .MU.read.bin.old())
### hidden function

# get the function:
.MU.read.bin.old <- macroutils:::.MU.read.bin.old 

# Test 1:
tbl1 <- .MU.read.bin.old( 
    bin.path = system.file( "bintest/METFILE.BIN", 
        package = "macroutils", mustWork = TRUE )
)   #

dim( tbl1 ) 
head( tbl1 ) 
tail( tbl1 ) 

# Same file exported with MACRO-GUI (5.2) 

# tbl1b <- read.table(
    # file    = system.file( "bintest/METFILE.TXT", 
        # package = "macroutils", mustWork = TRUE ), 
    # header  = TRUE, 
    # sep     = "\t", 
    # dec     = ".", 
    # strip.white = TRUE
# )   #

tbl1b <- readRDS( file = system.file( 
    "bintest/macro52convertedBin/METFILE.rds", 
    package = "macroutils", 
    mustWork = TRUE ) ) 

dim( tbl1b ) 
head( tbl1b ) 
tail( tbl1b ) 

tbl1  <- tbl1[,-1] 
tbl1b <- tbl1b[,-c(1,ncol(tbl1b))] 

data.frame( 
    colnames(tbl1), 
    colnames(tbl1b) 
)   #

sapply(
    X   = 1:ncol(tbl1), 
    FUN = function(X){ 
        max( abs(tbl1[,X] - tbl1b[,X]) ) 
    }   #
)   #

plot( tbl1b[,1] ~ tbl1[,1] ) 






# Test 2:
tbl2 <- .MU.read.bin.old(
    bin.path = system.file( "bintest/RAINFALL.BIN", 
        package = "macroutils", mustWork = TRUE ) 
)   #

dim( tbl2 ) 
head( tbl2 ) 
tail( tbl2 ) 

# Same file exported with MACRO-GUI (5.2) 
# tbl2b <- read.table(
    # file    = system.file( "bintest/RAINFALL.TXT", 
        # package = "macroutils", mustWork = TRUE ), 
    # header  = TRUE, 
    # sep     = "\t", 
    # dec     = ".", 
    # strip.white = TRUE
# )   #

tbl2b <- readRDS( file = system.file( 
    "bintest/macro52convertedBin/RAINFALL.rds", 
    package = "macroutils", 
    mustWork = TRUE ) ) 

dim( tbl2b ) 
head( tbl2b ) 
tail( tbl2b ) 

tbl2  <- tbl2[,-1,drop=FALSE] 
tbl2b <- tbl2b[,-c(1,ncol(tbl2b)),drop=FALSE] 

data.frame( 
    colnames(tbl2), 
    colnames(tbl2b) 
)   #

sapply(
    X   = 1:ncol(tbl2), 
    FUN = function(X){ 
        max( abs(tbl2[,X] - tbl2b[,X]) ) 
    }   #
)   #

plot( tbl2b[,1] ~ tbl2[,1] ) 






# Test 3:
tbl3 <- .MU.read.bin.old( 
    bin.path = system.file( "bintest/defaultrun.bin", 
        package = "macroutils", mustWork = TRUE )
)   #

dim( tbl3 ) 
head( tbl3 ) 
tail( tbl3 ) 

# Same file exported with MACRO-GUI (5.2) 
# tbl3b <- read.table(
    # file    = system.file( "bintest/defaultrun.txt", 
        # package = "macroutils", mustWork = TRUE ), 
    # header  = TRUE, 
    # sep     = "\t", 
    # dec     = ".", 
    # strip.white = TRUE
# )   #

tbl3b <- readRDS( file = system.file( 
    "bintest/macro52convertedBin/defaultrun.rds", 
    package = "macroutils", 
    mustWork = TRUE ) ) 

dim( tbl3b ) 
head( tbl3b ) 
tail( tbl3b ) 

tbl3  <- tbl3[,-1] 
tbl3b <- tbl3b[,-c(1,ncol(tbl3b))] 

data.frame( 
    colnames(tbl3), 
    colnames(tbl3b) 
)   #

sapply(
    X   = 1:ncol(tbl3), 
    FUN = function(X){ 
        max( abs(tbl3[,X] - tbl3b[,X]) ) 
    }   #
)   #

plot( tbl3b[,1] ~ tbl3[,1] ) 

