Linux: [![Linux Build Status (Travis CI)](https://travis-ci.org/julienmoeys/macroutils.svg?branch=master)](https://travis-ci.org/julienmoeys/macroutils).  
Windows: [![Windows Build Status (AppVeyor)](https://ci.appveyor.com/api/projects/status/github/julienmoeys/macroutils?branch=master&svg=true)](https://ci.appveyor.com/project/julienmoeys/macroutils)

macroutils
==========

Page content: 
[In short](#inshort) &middot; 
[Installation](#installation) &middot; 
[Official CKB release](#official) &middot; 
[Development version](#development) &middot; 
[Text based graphical user interface](#text_gui) &middot; 
[Documentation](#doc) &middot; 
[Issues and bug reports](#issues) 



_R utility functions for the [MACRO][] and [SOILNDB][] models._ 
_Read and write binary files, create plots, and more._

[MACRO][] and [SOILNDB][] use [binary files][binary_files] for 
both input (climate variables) and output (simulation results). 
The format of these files is not standard and `macroutils` 
provides functions to handle them. 



In short    <a id="inshort"></a>
========

*   Author: **[Julien MOEYS][julienmoeys]** ([SLU][]/[CKB][])
*   [Tutorial][macroutils_tuto] and [On-line documentation][macroutils_help] 
*   Package description: See [DESCRIPTION](https://github.com/julienmoeys/macroutils/blob/master/pkg/macroutils/DESCRIPTION).
*   View and report issues: https://github.com/julienmoeys/macroutils/issues 
    Note: _not_ for reporting issues related to MACRO (core 
    model)
*   Homepage of the MACRO model: [here][MACRO]



Installation    <a id="installation"></a>
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



Official [CKB][] release    <a id="official"></a>
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



Development version    <a id="development"></a>
-------------------

Note: _May work on other platforms than Windows, but this has not_ 
_been tested_ ([MACRO][] only runs on Windows).

Note: _If you are not familiar with R (and R developments), you 
should probably avoid that method, as some complications may arise_.

The development version of `macroutils` is publicly available 
on [GitHub][] ([the page you are actually reading][macroutils_gh]). 
This page allows you to explore the package source code, to install 
the package development version (see below) and to report some 
[issues][macroutils_issues] (bugs)

*   On Windows computers, install [Rtools][] Install the 
    version that corresponds to your the version of R that is 
    installed on your computer. On other machines, read the 
    [instructions here][devtools_readme] (Windows users may also 
    have a look at this page).
    
*   Install the package [devtools][]. `devtools` is a package 
    developed by [Hadley Wickham][HadleyWickham] to install 
    development version of R packages that are hosted on [GitHub][] 
    (among other useful things).
    
*   Open a new R cession and type `install.packages("devtools")`.
    Follow the additional [instructions here][devtools_readme].
    
*   Install the development version of `macroutils`. Open a new R 
    cession and type:


    devtools::install_github("julienmoeys/macroutils/pkg/macroutils")



Text based graphical user interface    <a id="text_gui"></a>
===================================

See also: The [package tutorial][macroutils_tuto]. 

`macroutils` provides simple but effective [text based graphical 
user interfaces][text_gui] for the most useful functions 
provided by the package. Once the package has been 
[installed](#installation), you just need to type one 
simple command (per function) and then follow the instructions 
given to you.



Plotting the content of one (or several) bin file(s)
----------------------------------------------------

Launch R and type:

    macroutils::macroPlot()

and follow the instructions. This is equivalent to:

    library( "macroutils" )
    macroPlot()



Converting one (or several) bin file(s)
----------------------------------------------------

Launch R and type:

    macroutils::macroConvertBin()

and follow the instructions. This is equivalent to:

    library( "macroutils" )
    macroConvertBin()



View one bin file
-----------------

Launch R and type:

    macroutils::macroViewBin()

and follow the instructions. This is equivalent to:

    library( "macroutils" )
    macroViewBin()




Documentation    <a id="doc"></a>
=============

See also: The [package tutorial][macroutils_tuto]. 



On-line documentation
---------------------

The package help pages can be browsed on-line 
[from this page][macroutils_help]. Notice that this may not 
always be the very latest version of the package.



Loading the package
-------------------

Open R graphical user interface and type:

    library( "macroutils" )



Package documentation (help pages)
----------------------------------

To go further, you can access the help page by typing:

    help( package = "macroutils" )

There, see in particular the vignette (tutorial) in "User guides, 
package vignettes and other documentation" (to get started). You 
can also access that tutorial with the command

    vignette( "macroutils_vignette" )

Notice that if you have installed the development version from 
[GitHub][] the vignette may not have been generated with the 
package. But the vignette can be extracted from the package 
Windows binaries that can be downloaded on [MACRO][] homepage 
(`macroutils_x.y.z.zip` > `macroutils` > `doc` > 
`macroutils_vignette.pdf`).



Issues and bug reports    <a id="issues"></a>
======================

Issues and bugs can be reported [here][macroutils_issues]. Do not 
report issues related to the model [MACRO][] (i.e. not related to 
`macroutils`) on that page. Do not either report more general 
questions that are related to [R][] and R usage. Provide a 
reproducible (_self standing_) example of your problem, as it 
generally greatly helps to narrow down the problem.



<!--- Links         -->
[SLU]:              http://www.slu.se/en/ "Swedish University of Agricultural Sciences"
[CKB]:              http://www.slu.se/en/collaborative-centres-and-projects/centre-for-chemical-pesticides-ckb1/ "The Centre for Chemical Pesticides (CKB)"
[MACRO]:            http://www.slu.se/en/collaborative-centres-and-projects/centre-for-chemical-pesticides-ckb1/areas-of-operation-within-ckb/models/macro-52/ "MACRO - pesticide fate in soils (SLU/CKB)"
[GitHub]:           https://github.com/ "GitHub"
[macroutils_gh]:    https://github.com/julienmoeys/macroutils "R package macroutils (on GitHub)"
[macroutils_issues]:https://github.com/julienmoeys/macroutils/issues "Issues on the package macroutils"
[macroutils_help]:  http://julienmoeys.github.io/macroutils/ "Documentation for package 'macroutils'"
[R]:                http://www.r-project.org/ "R is a free software environment for statistical computing and graphics"
[CRAN]:             http://cran.r-project.org/ "The Comprehensive R Archive Network"
[CRAN_mirrors]:     http://cran.r-project.org/mirrors.html "CRAN Mirrors"
[Rtools]:           http://cran.r-project.org/bin/windows/Rtools/ "Rtools: Building R for Windows"
[devtools_readme]:  http://cran.r-project.org/web/packages/devtools/README.html "README page of devtools"
[devtools]:         http://cran.r-project.org/web/packages/devtools "R package devtools"
[HadleyWickham]:    http://had.co.nz/ "Hadley Wickham homepage"
[text_gui]:         http://en.wikipedia.org/wiki/Text-based_user_interface "Text-based user interface (Wikipedia)"
[julienmoeys]:      http://www.julienmoeys.info "Julien Moeys homepage"
[binary_files]:     https://en.wikipedia.org/wiki/Binary_file "Binary files (Wikipedia)"
[SOILNDB]:          http://www.slu.se/en/collaborative-centres-and-projects/slu-water-hub/models/soilndb/ "SoilN DB homepage" 
[macroutils_tuto]:  https://github.com/julienmoeys/macroutils/blob/master/pkg/macroutils/inst/doc/macroutils_vignette.pdf "macroutils tutorial" 

