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
