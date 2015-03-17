set pkgname=macroutils
cd /D "%rPackagesDir%\macro-sp\%pkgname%\pkg" 

R CMD build --no-vignettes --md5 %pkgname% 
@REM  --compact-vignette="gs"
pause
