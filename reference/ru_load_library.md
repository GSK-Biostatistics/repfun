# Load a list of libraries

Given a list of packages, check if installed and generate message,
otherwise load package.

## Usage

``` r
ru_load_library(pkgs)
```

## Arguments

- pkgs:

  A list of packages to check if installed and then load.

## Value

No return value, packages are loaded.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
repfun::ru_load_library(c("dplyr", "haven", "magrittr", "r2rtf"))
```
