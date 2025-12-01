# ADaM ADAE

adae

## Usage

``` r
adae
```

## Format

A data frame with 105 columns:

- STUDYID :

  Study Identifier

- DOMAIN :

  Domain Abbreviation

- USUBJID :

  Unique Subject Identifier

- AESEQ :

  Sequence Number

- AESPID :

  Sponsor-Defined Identifier

- AETERM :

  Reported Term for the Adverse Event

- AELLT :

  Lowest Level Term

- AELLTCD :

  Lowest Level Term Code

- AEDECOD :

  Dictionary-Derived Term

- AEPTCD :

  Preferred Term Code

- AEHLT :

  High Level Term

- AEHLTCD :

  High Level Term Code

- AEHLGT :

  High Level Group Term

- AEHLGTCD :

  High Level Group Term Code

- AEBODSYS :

  Body System or Organ Class

- AEBDSYCD :

  Body System or Organ Class Code

- AESOC :

  Primary System Organ Class

- AESOCCD :

  Primary System Organ Class Code

- AESEV :

  Severity/Intensity

- AESER :

  Serious Event

- AEACN :

  Action Taken with Study Treatment

- AEREL :

  Causality

- AEOUT :

  Outcome of Adverse Event

- AESCAN :

  Involves Cancer

- AESCONG :

  Congenital Anomaly or Birth Defect

- AESDISAB :

  Persist or Signif Disability/Incapacity

- AESDTH :

  Results in Death

- AESHOSP :

  Requires or Prolongs Hospitalization

- AESLIFE :

  Is Life Threatening

- AESOD :

  Occurred with Overdose

- AEDTC :

  Date/Time of Collection

- AESTDTC :

  Start Date/Time of Adverse Event

- AEENDTC :

  End Date/Time of Adverse Event

- AESTDY :

  Study Day of Start of Adverse Event

- AEENDY :

  Study Day of End of Adverse Event

- TRTSDT :

  Date of First Exposure to Treatment

- TRTEDT :

  Date of Last Exposure to Treatment

- DTHDT :

  Date of Death

- EOSDT :

  End of Study Date

- ASTDTM :

  Analysis Start Date/Time

- ASTDTF :

  Analysis Start Date Imputation Flag

- ASTTMF :

  Analysis Start Time Imputation Flag

- AENDTM :

  Analysis End Date/Time

- AENDTF :

  Analysis End Date Imputation Flag

- AENTMF :

  Analysis End Time Imputation Flag

- ASTDT :

  Analysis Start Date

- AENDT :

  Analysis End Date

- ASTDY :

  Analysis Start Relative Day

- AENDY :

  Analysis End Relative Day

- ADURN :

  Analysis Duration (N)

- ADURU :

  Analysis Duration Units

- LDOSEDTM :

  End Date/Time of Last Dose

- ASEV :

  Analysis Severity/Intensity

- AREL :

  Analysis Causality

- TRTEMFL :

  Treatment Emergent Analysis Flag

- ASEVN :

  Analysis Severity/Intensity (N)

- AOCCIFL :

  1st Max Sev./Int. Occurrence Flag

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

- TRT01P :

  Planned Treatment for Period 01

- TRT01A :

  Actual Treatment for Period 01

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

- EOSSTT :

  End of Study Status

- FRVDT :

  Final Retrievel Visit Date

- RANDDT :

  Date of Randomization

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

Generated from admiral package (template ad_adae.R).

## Details

Adverse Events Analysis

## References

None

## Examples

``` r
data("adae")
```
