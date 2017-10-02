set pkgname=macroutils
set version=1.15.0

cd /D "%rPackagesDir%\%pkgname%\pkg" 

R CMD Rd2pdf %pkgname% --output=%pkgname%.%version%.pdf

pause
