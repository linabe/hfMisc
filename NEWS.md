# hfmisc 0.0.1

* This is the first version of hfmisc.

# hfmisc 0.0.2

* Changed default to UTDATUM = FALSE in prep_sosdata. The index date will 
therefore be compared with INDATUM. 
* Added optional parameter, allowedcenslimit, were possible to specify limit were 
an outcome is still considered. This is to ensure that patients
that are discharged AFTER date of death are still counted as an event, should
only have inpact if UTDATUM = TRUE.
* Added optional parameter valsclass with option to output the comorb/outcome as
a character or factor variable. 
* Added function create_deathvar and dataset dors_data

# hfmisc 0.0.3

* Trim trailing whitespace for variables DIA_all, OP_all, ekod_all, ORSAK_all in prep_sosdata

# hfmisc 0.0.4

* Trim all extra whitespaces for variables DIA_all, OP_all, ekod_all, ORSAK_all in prep_sosdata

# hfmisc 0.0.5

* [] not shown in metaout for create_sosvar

# hfmisc 0.0.6

* added restriction that death cannot occur after censdate in create_deathvar 
* [] not shown in metaout for create_deathvar

# hfmisc 0.0.7

* added option (noof = TRUE) that calculates number of comorbs/outcomes

# hfmisc 0.0.8

* update to R v 4

# hfmisc 0.0.9

* sos_outtime_xx is calculated in accordance with starttime and stoptime

# hfmisc 0.0.10

* added function read_sasdata
* capital letter for Yes/No in create_sosvar/create_deathvar
* optional names for dia, op, ekod variables in prep_sosdata
* fix bug renaming HDIA/ULORSAK if remove = FALSE in prep_sosdata
* added imputation of DODSDAT in prep_sosdata 

# hfmisc 0.0.11

* added option to negate strings to search for in create_sosvar

# hfmisc 0.0.12

* added functions default_kable (default kable table from kableExtra) and fn (format numbers)

# hfmisc 0.0.13

* added option to calculate duration of comorbidity (comduration) in function create_sosvar

# hfmisc 0.0.14

* fixed bug when calculating noof events with stoptime and unique id variable 
* fixed bug when calculating noof events and valsclass != "num"

# hfmisc 0.0.15

* added option pequalsign to fn function and made possible to have digits != 3 if p = TRUE

# hfmisc 0.0.16

* added functions create_crevent, create_crvar, cut_surv

# hfmisc 0.0.17

* bug fix in create_sosvar valsclass = "fac"

# hfmisc 0.0.18

* added argument forcenum + warning to create_crvar
* added function create_medvar

# hfmisc 0.0.19

* added text conversions to create_crvar

# hfmisc 0.0.20

* changed default name to create lm vars in create_medvar
