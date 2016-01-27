set pkgname=macroutils
set version=1.12.1

cd /D "%rPackagesDir%\macro-se\%pkgname%\pkg" 

R CMD INSTALL --build --compact-docs --byte-compile %pkgname%_%version%.tar.gz

pause
