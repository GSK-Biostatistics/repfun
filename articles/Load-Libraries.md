# Load-Libraries

## Vignette Build Datetime

``` r
message(paste0('Datetime: ',Sys.Date(),':',Sys.time()))
#> Datetime: 2026-01-02:2026-01-02 10:57:10.228973
```

## Load Libraries

``` r
library(repfun)
library(devtools)
```

## Create a vector of packages to load.

``` r
lbs <- c("janitor", "gmodels", "epiR", "DescTools", "coin", "irr", "Exact", "stats")
lbs <- c('stats','devtools')
```

## Unload all packages in the list above.

``` r
trashbin <- lapply(lbs, 
                   function(x) if (x %in% .packages()) {detach(paste0('package:',x), character.only=TRUE)})
```

## Reload all packages in the list above.

``` r
suppressMessages(repfun::ru_load_library(lbs))
```

## Confirm all packages in the list above are loaded.

``` r
loaded <- lapply(lbs,function(x) x %in% loadedNamespaces())
names(loaded) <- lbs
knitr::kable(stack(loaded)[,c(2,1)], caption = "Is Package Loaded (T/F)?")
```

| ind      | values |
|:---------|:-------|
| stats    | TRUE   |
| devtools | TRUE   |

Is Package Loaded (T/F)?
