set pkgname=macroutils
set version=1.10.1

cd /D "%rPackagesDir%\macro-sp\%pkgname%\pkg" 

R CMD check %pkgname%_%version%.tar.gz

pause
