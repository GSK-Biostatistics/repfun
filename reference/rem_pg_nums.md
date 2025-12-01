# Remove floating page numbers from RTF files.

Pass in an RTF file to have floating page numbers removed and result
saved to the same file.

## Usage

``` r
rem_pg_nums(infile)
```

## Arguments

- infile:

  A path to the RTF file as a string.

## Value

No return value, the file is modified.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
library(repfun)
repfun::rem_pg_nums('/path/to/an/rtf/file.rtf')
} # }
```
