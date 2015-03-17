macroutils
==========

_R utility functions for the MACRO and SOILNDB models. Read and write_ 
_binary files, create plots, and more._

Author: **Julien MOEYS** ([SLU][]/[CKB][]).
Package description: See [DESCRIPTION](/pkg/macroutils/DESCRIPTION).

Installation
============

For all versions you need to install beforehand the sofware [R][] 
("a free software environment for statistical computing and graphics").

From R homepage, go to the [CRAN homepage][CRAN] or (better) choose 
the nearest [CRAN mirror][CRAN_mirrors].

From there, choose 'Download R for Windows' (or Mac or Linux), and 
then choose 'base' or 'install R for the First time'.

*   If you want to install the official CKB release (see below), you 
    must download the R version that corresponds to the package 
    requirement as indicated on `macroutils` / MACRO 5.2 homepage 
    on [CKB][] website (see below).

*   If you want to install the development version of macroutils (see 
    below), you can download latest R version, or (safer) the R 
    version that is indicated in the 
    [DESCRIPTION](/pkg/macroutils/DESCRIPTION) file.

Download the R installer, and install [R][] (follow the 
instructions).



Official [CKB][] release
------------------------

Note: _Windows only_ (the source package is not provided on this 
page).

On [MACRO 5.2 official homepage][MACRO], you can download the 
latest version of the package

The package binary Files are usually named `macroutils_x.y.z.zip`, 
where `x`, `y` and `z` form the version number (major milestone, 
minor milestone, minor revision).

Notice that you don't need to uncompress this `.zip` File! (it is 
an installer). Open R Graphical User Interface, and in the packages
menu (top menu bar), choose 'Install package(s) from local zip 
File(s)', and select the package binary File (`macroutils_x.y.z.zip`).



Development version
-------------------

Note: _May work on other platforms than Windows, but this has not_ 
_been tested_ ([MACRO][] only runs on Windows).

Note: _If you are not familiar with R (and R developments), you 
should probably avoid that method, as some complications may arise_.

1.  On Windows computers, install [Rtools][] Install the 
    version that corresponds to your the version of R that is 
    installed on your computer. On other machines, read the 
    [instructions here][devtools_readme] (Windows users may also 
    have a look at this page).
2.  Install the package [devtools][]. `devtools` is a package 
    developed by [Hadley Wickham][HadleyWickham] to install 
    development version of R packages that are hosted on [GitHub][] 
    (among other useful things).
3.  Open a new R cession and type `install.packages("devtools")`.
    Follow the additional [instructions here][devtools_readme].
4.  Install the development version of `macroutils`. Open a new R 
    cession and type 
    `devtools::install_github("julienmoeys/macroutils")`



Load and use the package
------------------------

Open R graphical user interface and type:

    library( "macroutils" )

To go further, you can access the help page by typing:

    help( package = "macroutils" )

See in particular the vignette (tutorial) in "User guides, package 
vignettes and other documentation" (to get started).



<!--- Links         -->
[SLU]:              http://www.slu.se/en/ "Swedish University of Agricultural Sciences"
[CKB]:              http://www.slu.se/en/collaborative-centres-and-projects/centre-for-chemical-pesticides-ckb1/ "The Centre for Chemical Pesticides (CKB)"
[MACRO]:            http://www.slu.se/en/collaborative-centres-and-projects/centre-for-chemical-pesticides-ckb1/areas-of-operation-within-ckb/models/macro-52/ "MACRO - pesticide fate in soils (SLU/CKB)"
[GitHub]:           https://github.com/ "GitHub"
[macroutils_gh]:    https://github.com/julienmoeys/macroutils "R package macroutils (on GitHub)"
[R]:                http://www.r-project.org/ "R is a free software environment for statistical computing and graphics"
[CRAN]:             http://cran.r-project.org/ "The Comprehensive R Archive Network"
[CRAN_mirrors]:     http://cran.r-project.org/mirrors.html "CRAN Mirrors"
[Rtools]:           http://cran.r-project.org/bin/windows/Rtools/ "Rtools: Building R for Windows"
[devtools_readme]:  http://cran.r-project.org/web/packages/devtools/README.html "README page of devtools"
[devtools]:         http://cran.r-project.org/web/packages/devtools "R package devtools"
[HadleyWickham]:    http://had.co.nz/ "Hadley Wickham homepage"

