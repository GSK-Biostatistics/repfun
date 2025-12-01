# Expand SAS Style Variable/Column List

Pass in a data frame along with column/variable identifiers formatted
with SAS Style (i.e., using colon) and it will be expanded to the actual
variable list.

## Usage

``` r
ru_expvarlist(dsetin, varsin = NULL, keepnotexist = FALSE)
```

## Arguments

- dsetin:

  A dataframe holding columns whose names will be expanded.

- varsin:

  A SAS style list of variable names.

- keepnotexist:

  If the variable does not exist on the dataframe it will be excluded
  from the list.

## Value

A list of column/variable names.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
library(repfun)
df <- data.frame(tt_ac01=c('1','2','3'),
                 tt_ac02=c('a','b','b'),
                 tt_ac03=c('10','11','12'))
chk <- repfun::ru_expvarlist(df, varsin="tt_ac:")
print(chk)
#> [1] "tt_ac01" "tt_ac02" "tt_ac03"
```
