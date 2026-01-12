# cran-comments.md

## Resubmission updates

* FIXED Possibly misspelled words in DESCRIPTION
* FIXED Found the following (possibly) invalid file URI
* FIXED Flavor: r-devel-windows-x86_64. Check: tests, Result: ERROR
* FIXED Flavor: r-devel-linux-x86_64-debian-gcc, r-devel-windows-x86_64. Check: HTML version of manual, Result: NOTE
* Note: Submitted to https://win-builder.r-project.org/upload.aspx under 1.) R-release, 2.) R-devel, 3.) R-oldrelease and there are no issues.

## Test environments

* Local machine: Windows 11 Enterprise, R 4.2.2 (RTools42 is the latest PC version available on company portal)
* GitHub Actions Check Environments:
    * macos-latest (release)
    * windows-latest (release)
    * windows-latest (oldrel-1)
    * ubuntu-latest (devel)
    * ubuntu-latest (release)
    * ubuntu-latest (oldrel-1)
* R CMD check --as-cran was run on the tarball using R version 4.5.1 (2025-06-13) on platform x86_64-pc-linux-gnu (Red Hat Enterprise Linux 8.10).

## R CMD check results

0 errors | 0 warnings | 1 notes

Using R CMD check with --as-cran yields the note "Maintainer: Chris Rook <cr883296@gmail.com>"

## Reverse dependencies

There are currently no reverse dependencies for this package.

## Submission

This is the first resubmission.


Thank you very much in advance,

Chris Rook
