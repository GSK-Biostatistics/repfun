# Align Columns for Reporting

Pass in a data frame alignment criteria to have columns aligned for
reporting.

## Usage

``` r
ru_align(
  dsetin,
  varsin,
  byvars = NULL,
  alignment = "Right",
  compresschryn = "Y",
  ncspaces = 1
)
```

## Arguments

- dsetin:

  The data set holding the columns to align.

- varsin:

  Columns to align.

- byvars:

  Set of group-by variables.

- alignment:

  Type of alignment.

- compresschryn:

  Compress by removing leading and trailing spaces for the resulting
  aligned column?

- ncspaces:

  Number of spaces between N and Percent.

## Value

A data frame based having the requested columns aligned for reporting.

## Author

Yongwei Wang, <yongwei.x.wang@viivhealthcare.com>  
Chris Rook, <cr883296@gmail.com>

## Examples

``` r
#=================
# Adverse Events
#=================
library(repfun)
library(dplyr)
datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
dir.create(datdir,showWarnings=FALSE)
repfun::copydata(datdir)
repfun::rs_setup(D_POP="SAFFL",
                 D_POPLBL="Safety",
                 D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
                 D_SUBJID=c("STUDYID","USUBJID"),
                 R_DICTION=NULL,
                 R_OTHERDATA=NULL,
                 R_INPUTDATA=NULL,
                 R_RAWDATA=NULL,
                 R_SDTMDATA=NULL,
                 R_ADAMDATA=datdir)
G_POPDATA <- repfun:::rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=
                     ifelse(TRT01A=='Placebo',1,
                     ifelse(TRT01A=='Xanomeline Low Dose',2,3)))
attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
adae <- repfun:::rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL) %>%
        repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"),
        keeppopvars=c("TRT01AN", "TRT01A"))
aesum_t <- repfun::ru_freq(adae,
              dsetindenom=G_POPDATA,
              countdistinctvars=c('STUDYID','USUBJID'),
              groupbyvarsnumer=c('TRT01AN','TRT01A','AEBODSYS','AEDECOD'),
              anyeventvars = c('AEBODSYS','AEDECOD'),
              anyeventvalues = c('ANY EVENT','ANY EVENT'),
              groupbyvarsdenom=c('TRT01AN'),
              resultstyle="NUMERPCT",
              totalforvar=c('TRT01AN'),
              totalid=99,
              totaldecode='Total',
              codedecodevarpairs=c("TRT01AN", "TRT01A"),
              varcodelistpairs=c(""),
              codelistnames=list(),
              resultpctdps=0) %>%
           repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"),
                    groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
                    acrossvar="TRT01AN",
                    acrossvarlabel="TRT01A",
                    acrossvarprefix=c("tt_ac", "tt_p"))
print('Before Aligning')
#> [1] "Before Aligning"
print(head(aesum_t[,grep('(AEBODSYS|AEDECOD|tt_ac)',names(aesum_t))],20))
#>                                                               AEBODSYS
#> 1                                                            ANY EVENT
#> 2                                                    CARDIAC DISORDERS
#> 3                           CONGENITAL, FAMILIAL AND GENETIC DISORDERS
#> 4                                          EAR AND LABYRINTH DISORDERS
#> 5                                                        EYE DISORDERS
#> 6                                           GASTROINTESTINAL DISORDERS
#> 7                 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
#> 8                                              HEPATOBILIARY DISORDERS
#> 9                                              IMMUNE SYSTEM DISORDERS
#> 10                                         INFECTIONS AND INFESTATIONS
#> 11                      INJURY, POISONING AND PROCEDURAL COMPLICATIONS
#> 12                                                      INVESTIGATIONS
#> 13                                  METABOLISM AND NUTRITION DISORDERS
#> 14                     MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS
#> 15 NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS)
#> 16                                            NERVOUS SYSTEM DISORDERS
#> 17                                               PSYCHIATRIC DISORDERS
#> 18                                         RENAL AND URINARY DISORDERS
#> 19                            REPRODUCTIVE SYSTEM AND BREAST DISORDERS
#> 20                     RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS
#>      AEDECOD  tt_ac01  tt_ac02  tt_ac03   tt_ac99
#> 1  ANY EVENT 69 (80%) 86 (90%) 70 (97%) 225 (89%)
#> 2  ANY EVENT 13 (15%) 16 (17%) 15 (21%)  44 (17%)
#> 3  ANY EVENT   0 (0%)   1 (1%)   2 (3%)    3 (1%)
#> 4  ANY EVENT   1 (1%)   2 (2%)   1 (1%)    4 (2%)
#> 5  ANY EVENT   4 (5%)   2 (2%)   1 (1%)    7 (3%)
#> 6  ANY EVENT 17 (20%) 16 (17%) 20 (28%)  53 (21%)
#> 7  ANY EVENT 21 (24%) 51 (53%) 36 (50%) 108 (43%)
#> 8  ANY EVENT   1 (1%)   0 (0%)   0 (0%)    1 (0%)
#> 9  ANY EVENT   0 (0%)   1 (1%)   1 (1%)    2 (1%)
#> 10 ANY EVENT 16 (19%) 10 (10%) 13 (18%)  39 (15%)
#> 11 ANY EVENT   4 (5%)   5 (5%)   5 (7%)   14 (6%)
#> 12 ANY EVENT 10 (12%)   8 (8%)   5 (7%)   23 (9%)
#> 13 ANY EVENT   6 (7%)   1 (1%)   3 (4%)   10 (4%)
#> 14 ANY EVENT   5 (6%)   7 (7%)  8 (11%)   20 (8%)
#> 15 ANY EVENT   0 (0%)   2 (2%)   1 (1%)    3 (1%)
#> 16 ANY EVENT 12 (14%) 22 (23%) 25 (35%)  59 (23%)
#> 17 ANY EVENT 10 (12%) 11 (11%)  8 (11%)  29 (11%)
#> 18 ANY EVENT   4 (5%)   4 (4%)   3 (4%)   11 (4%)
#> 19 ANY EVENT   2 (2%)   0 (0%)   1 (1%)    3 (1%)
#> 20 ANY EVENT 10 (12%) 10 (10%) 10 (14%)  30 (12%)
aesum_t_a <- repfun::ru_align(aesum_t, "tt_ac:")
print('After Aligning')
#> [1] "After Aligning"
print(head(aesum_t_a[,grep('(AEBODSYS|AEDECOD|tt_ac)',names(aesum_t_a))],20))
#>                                                               AEBODSYS
#> 1                                                            ANY EVENT
#> 2                                                    CARDIAC DISORDERS
#> 3                           CONGENITAL, FAMILIAL AND GENETIC DISORDERS
#> 4                                          EAR AND LABYRINTH DISORDERS
#> 5                                                        EYE DISORDERS
#> 6                                           GASTROINTESTINAL DISORDERS
#> 7                 GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS
#> 8                                              HEPATOBILIARY DISORDERS
#> 9                                              IMMUNE SYSTEM DISORDERS
#> 10                                         INFECTIONS AND INFESTATIONS
#> 11                      INJURY, POISONING AND PROCEDURAL COMPLICATIONS
#> 12                                                      INVESTIGATIONS
#> 13                                  METABOLISM AND NUTRITION DISORDERS
#> 14                     MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS
#> 15 NEOPLASMS BENIGN, MALIGNANT AND UNSPECIFIED (INCL CYSTS AND POLYPS)
#> 16                                            NERVOUS SYSTEM DISORDERS
#> 17                                               PSYCHIATRIC DISORDERS
#> 18                                         RENAL AND URINARY DISORDERS
#> 19                            REPRODUCTIVE SYSTEM AND BREAST DISORDERS
#> 20                     RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS
#>      AEDECOD  tt_ac01  tt_ac02  tt_ac03   tt_ac99
#> 1  ANY EVENT 69 (80%) 86 (90%) 70 (97%) 225 (89%)
#> 2  ANY EVENT 13 (15%) 16 (17%) 15 (21%)  44 (17%)
#> 3  ANY EVENT  0  (0%)  1  (1%)  2  (3%)   3  (1%)
#> 4  ANY EVENT  1  (1%)  2  (2%)  1  (1%)   4  (2%)
#> 5  ANY EVENT  4  (5%)  2  (2%)  1  (1%)   7  (3%)
#> 6  ANY EVENT 17 (20%) 16 (17%) 20 (28%)  53 (21%)
#> 7  ANY EVENT 21 (24%) 51 (53%) 36 (50%) 108 (43%)
#> 8  ANY EVENT  1  (1%)  0  (0%)  0  (0%)   1  (0%)
#> 9  ANY EVENT  0  (0%)  1  (1%)  1  (1%)   2  (1%)
#> 10 ANY EVENT 16 (19%) 10 (10%) 13 (18%)  39 (15%)
#> 11 ANY EVENT  4  (5%)  5  (5%)  5  (7%)  14  (6%)
#> 12 ANY EVENT 10 (12%)  8  (8%)  5  (7%)  23  (9%)
#> 13 ANY EVENT  6  (7%)  1  (1%)  3  (4%)  10  (4%)
#> 14 ANY EVENT  5  (6%)  7  (7%)  8 (11%)  20  (8%)
#> 15 ANY EVENT  0  (0%)  2  (2%)  1  (1%)   3  (1%)
#> 16 ANY EVENT 12 (14%) 22 (23%) 25 (35%)  59 (23%)
#> 17 ANY EVENT 10 (12%) 11 (11%)  8 (11%)  29 (11%)
#> 18 ANY EVENT  4  (5%)  4  (4%)  3  (4%)  11  (4%)
#> 19 ANY EVENT  2  (2%)  0  (0%)  1  (1%)   3  (1%)
#> 20 ANY EVENT 10 (12%) 10 (10%) 10 (14%)  30 (12%)

#===========================
# Baseline Characteristics
#===========================
repfun::rs_setup(D_POP="SAFFL",
                 D_POPLBL="Safety",
                 D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
                 D_SUBJID=c("STUDYID","USUBJID"),
                 R_DICTION=NULL,
                 R_OTHERDATA=NULL,
                 R_INPUTDATA=NULL,
                 R_RAWDATA=NULL,
                 R_SDTMDATA=NULL,
                 R_ADAMDATA=datdir)
G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
             dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
                            ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
  repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
demstats_t <- repfun::ru_sumstats(G_POPDATA,
                 analysisvars=c("AGE","TRTDURD"),
                 groupbyvars=c("STUDYID","TRT01AN"),
                 codedecodevarpairs=c("TRT01AN", "TRT01A"),
                 totalforvar="TRT01AN", totalid=99,
                 totaldecode="Total",
                 statsinrowsyn = "Y",
                 analysisvardps=list("AGE"=1,"TRTDURD"=2),
                 statslist=c("n", "mean", "median", "sd", "min", "max")) %>%
  repfun::ru_denorm(varstodenorm=c("tt_result"),
            groupbyvars=c("tt_avid", "tt_avnm", "tt_svid", "tt_svnm"),
            acrossvar="TRT01AN", acrossvarlabel="TRT01A",
            acrossvarprefix=c("tt_ac"))

print('Before Aligning')
#> [1] "Before Aligning"
print(head(demstats_t,10))
#>    tt_avid tt_avnm tt_svid tt_svnm tt_ac01 tt_ac02 tt_ac03 tt_ac99
#> 1        1     AGE       1       n      86      96      72     254
#> 2        1     AGE       2    Mean   75.21   75.96   73.78   75.09
#> 3        1     AGE       3  Median   76.00   78.00   75.50   77.00
#> 4        1     AGE       4      SD   8.590   8.114   7.944   8.246
#> 5        1     AGE       5     Min    52.0    51.0    56.0    51.0
#> 6        1     AGE       6     Max    89.0    88.0    88.0    89.0
#> 7        2 TRTDURD       1       n      85      95      72     252
#> 8        2 TRTDURD       2    Mean 149.541  86.811 112.222 115.230
#> 9        2 TRTDURD       3  Median 182.000  63.000  96.500 132.000
#> 10       2 TRTDURD       4      SD 60.3544 70.4737 65.5233 70.7137
demstats_t_a <- repfun::ru_align(demstats_t, "tt_ac:", ncspaces=10)
print('After Aligning')
#> [1] "After Aligning"
print(head(demstats_t_a,10))
#>    tt_avid tt_avnm tt_svid tt_svnm  tt_ac01  tt_ac02  tt_ac03  tt_ac99
#> 1        1     AGE       1       n  86       96       72      254     
#> 2        1     AGE       2    Mean  75.21    75.96    73.78    75.09  
#> 3        1     AGE       3  Median  76.00    78.00    75.50    77.00  
#> 4        1     AGE       4      SD   8.590    8.114    7.944    8.246 
#> 5        1     AGE       5     Min  52.0     51.0     56.0     51.0   
#> 6        1     AGE       6     Max  89.0     88.0     88.0     89.0   
#> 7        2 TRTDURD       1       n  85       95       72      252     
#> 8        2 TRTDURD       2    Mean 149.541   86.811  112.222  115.230 
#> 9        2 TRTDURD       3  Median 182.000   63.000   96.500  132.000 
#> 10       2 TRTDURD       4      SD  60.3544  70.4737  65.5233  70.7137
```
