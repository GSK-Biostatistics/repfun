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

'NULL' because floating page numbers are removed from rtf files.

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
