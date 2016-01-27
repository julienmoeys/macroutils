set pkgname=macroutils
cd /D "%rPackagesDir%\macro-se\%pkgname%\pkg" 

R CMD check --no-tests --no-examples --no-vignettes %pkgname%

pause
