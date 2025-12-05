# repfun <a href="https://gsk-biostatistics.github.io/repfun/"><img src="man/figures/repfun.png" align="right" height="139" alt="repfun website" /></a>

<a id="readme-top"></a>

[![CRAN status](https://www.r-pkg.org/badges/version/repfun)](https://CRAN.R-project.org/package=repfun)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/license/apache-2-0)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/GSK-Biostatistics/repfun/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/GSK-Biostatistics/repfun/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/GSK-Biostatistics/repfun/graph/badge.svg)](https://app.codecov.io/gh/GSK-Biostatistics/repfun)

## R Reporting Functions Repository

This repository contains an R package of reporting functions that mimic (HARP) SAS macros for clinical reporting.  The package can be used to generate TLFs (Tables, Listings, Figures).  See several examples in the <a href="https://gsk-biostatistics.github.io/repfun/">Vignettes/Articles</a>.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<hr />

## How to Use This Package

1. Download and install the package using the zip file repfun_0.0.0.9000.tar.gz (in "misc" branch) or via library remotes (see below).
2. Load the library in an R session.
3. Review the function documentation under References <br> (https://gsk-biostatistics.github.io/repfun/reference/index.html).
3. Identify a use case that fits your needs from the Articles <br> (https://gsk-biostatistics.github.io/repfun/articles/index.html).
5. Copy and modify the Vignette code for your study.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<hr />

## General Information

- This package contains reporting functions that mimic (HARP) SAS macros.
- View the function documentation at: https://gsk-biostatistics.github.io/repfun/reference/index.html
- Install the package using: 
    library(remotes); install_github("GSK-Biostatistics/repfun")
    or install.packages('/your/path/repfun_0.0.0.9000.tar.gz',repos=NULL,type='source') (see "misc" branch)
    or download the following zip file and install as above: https://github.com/GSK-Biostatistics/repfun/blob/misc/repfun_0.0.0.9000.tar.gz
- Use the package by loading it: library(repfun)
- View use cases at: https://gsk-biostatistics.github.io/repfun/articles/
- View package documentation at: https://github.com/GSK-Biostatistics/repfun/blob/misc/repfun_0.0.0.9000.pdf

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<hr />

<!-- BADGES & SHIELDS FROM https://naereen.github.io/badges/ -->
## Built With

* [![R](https://img.shields.io/badge/-script-276DC3.svg?style=flat&logo=R)](https://cran.r-project.org) 
* [![RStudio](https://img.shields.io/badge/RStudio-project-75AADB.svg?style=flat&logo=RStudio)](https://posit.co/)
* [![PowerShell](https://img.shields.io/badge/PowerShell-%235391FE.svg?style=flat&logo=powershell&logoColor=white)](https://learn.microsoft.com/en-us/powershell/)
* [![Windows Terminal](https://img.shields.io/badge/Windows%20Terminal-%234D4D4D.svg?style=flat&logo=windows-terminal&logoColor=white)](https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/windows-commands)
* [![Google Chrome](https://img.shields.io/badge/Google%20Chrome-4285F4?style=flat&logo=GoogleChrome&logoColor=white)](https://www.google.com/chrome/)
* [![GitHub](https://img.shields.io/badge/GitHub-100000?style=flat&logo=github&logoColor=white)](https://github.com/)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<hr />

## Getting Started

### Prerequisites

``` r
# Before using repfun, install the following R packages:
install.packages(c("tidyr","dplyr","ggplot2","magrittr","Hmisc","haven","stringr","admiral","lubridate","r2rtf")) 
```
<hr />

## License

This project is released under the Apache-2.0 license. See the **[LICENSE](LICENSE.md)** file for details.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<hr />

## Contact

For any questions or comments about this project, please contact **[Yongwei Wang](mailto:yongwei.x.wang@gsk.com?subject=[GitHub]%20Source%20Yongwei%20Wang)** or **[Chris Rook](mailto:cr883296@gmail.com?subject=[GitHub]%20Source%20Chris%20Rook)**.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<hr />

## Acknowledgments

Acknowledgement to the following useful sources:

* [Helpful Read-Me Template](https://github.com/othneildrew/Best-README-Template)
* [Img Shields](https://shields.io)
* [GitHub Pages](https://pages.github.com)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<hr />
