set pkgname=macroutils

cd /D "%rPackagesDir%\macro-sp\%pkgname%\pkg" 

REM svnversion > %pkgname%\inst\SVN_VERSION
git log -n 1 --oneline --no-notes > %pkgname%\inst\GIT_VERSION

R CMD build --compact-vignette="gs" --md5 %pkgname% 
@REM  --no-vignettes
pause