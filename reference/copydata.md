# Copy package data to the directory specified.

Copy all package data into a temporary directory that can be used when
running examples.

## Usage

``` r
copydata(p)
```

## Arguments

- p:

  A path as a string.

## Value

'NULL' because package dataframes are copied to a folder.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
copydata(tempdir())
```
