set pkgname=macroutils
set version=1.13.0

cd /D "%rPackagesDir%\macro-se\%pkgname%\pkg" 

R CMD check %pkgname%_%version%.tar.gz

pause
