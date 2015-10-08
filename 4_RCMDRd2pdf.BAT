set pkgname=macroutils
set version=1.9.0

cd /D "%rPackagesDir%\macro-sp\%pkgname%\pkg"  

R CMD Rd2pdf %pkgname% --output=%pkgname%.%version%.pdf

pause