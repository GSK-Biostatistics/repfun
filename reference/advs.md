# ADaM ADVS

advs

## Usage

``` r
advs
```

## Format

A data frame with 105 columns:

- STUDYID :

  Study Identifier

- DOMAIN :

  Domain Abbreviation

- USUBJID :

  Unique Subject Identifier

- VSSEQ :

  Sequence Number

- VSTESTCD :

  Vital Signs Test Short Name

- VSTEST :

  Vital Signs Test Name

- VSPOS :

  Vital Signs Position of Subject

- VSORRES :

  Result or Finding in Original Units

- VSORRESU :

  Original Units

- VSSTRESC :

  Character Result/Finding in Std Format

- VSSTRESN :

  Numeric Result/Finding in Standard Units

- VSSTRESU :

  Standard Units

- VSSTAT :

  Completion Status

- VSLOC :

  Location of Vital Signs Measurement

- VSBLFL :

  Baseline Flag

- VISITNUM :

  Visit Number

- VISIT :

  Visit Name

- VISITDY :

  Planned Study Day of Visit

- VSDTC :

  Date/Time of Measurements

- VSDY :

  Study Day of Vital Signs

- VSTPT :

  Planned Time Point Name

- VSTPTNUM :

  Planned Time Point Number

- VSELTM :

  Planned Elapsed Time from Time Point Ref

- VSTPTREF :

  Time Point Reference

- TRTSDT :

  Date of First Exposure to Treatment

- TRTEDT :

  Date of Last Exposure to Treatment

- TRT01A :

  Actual Treatment for Period 01

- TRT01P :

  Planned Treatment for Period 01

- ADT :

  Analysis Date

- ADY :

  Analysis Relative Day

- PARAMCD :

  Parameter Code

- AVAL :

  Analysis Value

- ATPTN :

  Analysis Timepoint (N)

- ATPT :

  Analysis Timepoint

- AVISIT :

  Analysis Visit

- AVISITN :

  Analysis Visit (N)

- DTYPE :

  Derivation Type

- ONTRTFL :

  On Treatment Record Flag

- ANRLO :

  Analysis Normal Range Lower Limit

- ANRHI :

  Analysis Normal Range Upper Limit

- A1LO :

  Analysis Range 1 Lower Limit

- A1HI :

  Analysis Range 1 Upper Limit

- ANRIND :

  Analysis Reference Range Indicator

- BASETYPE :

  Baseline Type

- ABLFL :

  Baseline Record Flag

- BASE :

  Baseline Value

- BNRIND :

  Baseline Reference Range Indicator

- CHG :

  Change from Baseline

- PCHG :

  Percent Change from Baseline

- ANL01FL :

  Analysis Flag 01

- TRTP :

  Planned Treatment

- TRTA :

  Actual Treatment

- ASEQ :

  Analysis Sequence Number

- AVALCAT1 :

  Analysis Value Category 1

- AVALCA1N :

  Analysis Value Category 1 (N)

- PARAM :

  Parameter

- PARAMN :

  Parameter (N)

- SUBJID :

  Subject Identifier for the Study

- RFSTDTC :

  Subject Reference Start Date/Time

- RFENDTC :

  Subject Reference End Date/Time

- RFXSTDTC :

  Date/Time of First Study Treatment

- RFXENDTC :

  Date/Time of Last Study Treatment

- RFICDTC :

  Date/Time of Informed Consent

- RFPENDTC :

  Date/Time of End of Participation

- DTHDTC :

  Date/Time of Death

- DTHFL :

  Subject Death Flag

- SITEID :

  Study Site Identifier

- AGE :

  Age

- AGEU :

  Age Units

- SEX :

  Sex

- RACE :

  Race

- ETHNIC :

  Ethnicity

- ARMCD :

  Planned Arm Code

- ARM :

  Description of Planned Arm

- ACTARMCD :

  Actual Arm Code

- ACTARM :

  Description of Actual Arm

- COUNTRY :

  Country

- DMDTC :

  Date/Time of Collection

- DMDY :

  Study Day of Collection

- TRTSDTM :

  Datetime of First Exposure to Treatment

- TRTSTMF :

  Time of First Exposure Imput. Flag

- TRTEDTM :

  Datetime of Last Exposure to Treatment

- TRTETMF :

  Time of Last Exposure Imput. Flag

- TRTDURD :

  Total Treatment Duration (Days)

- SCRFDT :

  Screen Failure Date

- EOSDT :

  End of Study Date

- EOSSTT :

  End of Study Status

- FRVDT :

  Final Retrievel Visit Date

- RANDDT :

  Date of Randomization

- DTHDT :

  Date of Death

- DTHDTF :

  undocumented field

- DTHADY :

  Relative Day of Death

- LDDTHELD :

  Elapsed Days from Last Dose to Death

- DTHCAUS :

  undocumented field

- DTHDOM :

  undocumented field

- DTHCGR1 :

  undocumented field

- LSTALVDT :

  Date Last Known Alive

- SAFFL :

  Safety Population Flag

- RACEGR1 :

  Pooled Race Group 1

- AGEGR1 :

  Pooled Age Group 1

- REGION1 :

  Geographic Region 1

- LDDTHGR1 :

  Last Dose to Death - Days Elapsed Grp 1

- DTH30FL :

  Death Within 30 Days of Last Trt Flag

- DTHA30FL :

  Death After 30 Days from Last Trt Flag

- DTHB30FL :

  Death Within 30 Days of First Trt Flag

## Source

Generated from admiral package (template ad_advs.R).

## Details

Vital Signs Analysis

## References

None

## Examples

``` r
data("advs")
```
