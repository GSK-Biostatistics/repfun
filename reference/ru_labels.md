# Assign labels to variables in a data frame

Pass in a data frame along with a named list of columns with their
corresponding labels.

## Usage

``` r
ru_labels(dsetin, varlabels = list(), style = c("base", "Hmisc"))
```

## Arguments

- dsetin:

  Incoming data frame to have labels added to columns.

- varlabels:

  List of variables and their labels.

- style:

  Type of method used to add labels.

## Value

The incoming data frame with labels added.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
df_with_labels <- ru_labels(mtcars,varlabels=list(mpg='Miles per gallon',
                                                  cyl='Number of cylinders'))
```
