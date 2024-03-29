# nails-package
[![Travis-CI Build Status](https://travis-ci.org/aknutas/nails-package.svg?branch=master)](https://travis-ci.org/aknutas/nails-package) [![codecov](https://codecov.io/gh/aknutas/nails-package/branch/master/graph/badge.svg)](https://codecov.io/gh/aknutas/nails-package)

**Network Analysis Interface for Literature Studies; R package version**  
by _Juho Salminen, Antti Knutas and Arash Hajikhani_  
at _Lappeenranta University of Technology_

What Is It?
----
This R package provides tools for performing Social Network Analysis (SNA) on citation data. This version is meant to use for R programmers. A general use version to be executed in RStudio is available at the second repository in the alternative [repository repository](https://github.com/aknutas/nails). See more information at the [project page](http://aknutas.github.io/nails/).

SNA is an interesting way for researchers to map large datasets and get insights from new angles. The steps for downloading data from Web of Knowledge and using our tools to process it are detailed below. The set of tools which are required to perform the analyses are free and need a minimum amount of installation. However, please note that as time goes by, more work will be required for installation.

The project files are available as open source here in our [Github repository](https://github.com/aknutas/nails-package). If you link or refer to us, please link to our [project page](http://aknutas.github.io/nails/).

Science!
----
The basic design and bibliometric principles of the system have been published in a research article:

Antti Knutas, Arash Hajikhani, Juho Salminen, Jouni Ikonen, and Jari Porras. 2015. _Cloud-Based Bibliometric Analysis Service for Systematic Mapping Studies_. In Proceedings of the 16th International Conference on Computer Systems and Technologies (CompSysTech '15). DOI: 10.1145/2812428.2812442

A preprint version of the article is [available for download](http://www.codecamp.fi/lib/exe/fetch.php/wiki/nails-compsystech2015-preprint.pdf) as PDF. The official version is now available at the [ACM Digital Library](http://dl.acm.org/citation.cfm?doid=2812428.2812442).

If you use the software in your scientific work, please consider citing us.

How to Use
----
1. Install R 3.4.2 from [CRAN](https://cran.r-project.org/) or a package manager of your choice. Functionality has been tested on Ubuntu LTS.
2. Install R devtools, for example from the R command line. *install.packages("devtools")*
3. Then use R devtools to install the package itself, for example from the R command line. *devtools::install_github("aknutas/nails-package")*
4. Get a dataset. You can follow the [instructions](http://nailsproject.net/#manually) about how to download your own dataset from Web of Science or watch the [introduction video](https://youtu.be/I1bRXQs_zMk) on how to download data from the Web of Science. Alternatively you can download a testing dataset from the package [repository subdirectory](https://github.com/aknutas/nails-package/tree/master/tests/testthat/test_data).
5. Read documentation and the getting started instructions from the package documentation [vignette](http://aknutas.github.io/nails-package/vignettes/literature_report.html).

Requirements
----
For now the project verifiably works on R version 3.4.2. See other package requirements in the DESCRIPTION file.

We are open source and free software
----
This program is [free software](https://www.gnu.org/philosophy/free-sw.html): you can redistribute it and/or modify it under the terms of the [GNU General Public License](https://www.gnu.org/licenses/quick-guide-gplv3.html) as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. See LICENSE file for more information.

What does it mean? We are free as in freedom. You may run the software as you wish, for any purpose; you are free to study how the program works, and change it as you wish; you are free to redistribute copies; and you are free to distribute copies of modified versions to others. You may not distribute this software in a non-free manner or add additional restrictions. The only limitations are that you have to follow the free software license, retain the original copyright notices and acknowledgement texts in the program output (section 7b). See links above for more information. If you edit and improve the software, we would love to hear back from you.
