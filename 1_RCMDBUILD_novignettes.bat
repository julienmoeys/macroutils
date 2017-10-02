set pkgname=macroutils

cd /D "%rPackagesDir%\%pkgname%\pkg" 

REM svnversion > %pkgname%\inst\SVN_VERSION
git log -n 1 --oneline --no-notes > %pkgname%\inst\GIT_VERSION

R CMD build --no-build-vignettes --md5 %pkgname% 
@REM  --no-vignettes
pause
