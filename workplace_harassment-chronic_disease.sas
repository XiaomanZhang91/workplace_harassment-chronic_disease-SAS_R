/*------------------------------------------ Import Data --------------------------------------------------------*/
LIBNAME har '/home/u63463818/BSTT 550_Biostatistical_investigations';

FILENAME REFFILE '/home/u63463818/BSTT 550_Biostatistical_investigations/SHNSH.xlsx';
PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=har.shn0;
	GETNAMES=YES;
RUN;
PROC CONTENTS DATA=har.shn0; RUN;

PROC FORMAT;
    VALUE sex_fmt
        0 = "Male"
        1 = "Female";

    VALUE race_fmt
        0 = "Other Race"
        1 = "White";

    VALUE group_fmt
        1 = "Secretarial/Admin"
        2 = "Faculty"
        3 = "RA/TA/Resident"
        4 = "Service Maintenance";
RUN;

DATA har.shn0;
    SET har.shn0;
    FORMAT female sex_fmt.
           white race_fmt.
           group group_fmt.;
RUN;

* Unify the missing data format;
DATA har.shn0;
	SET har.shn0;
	ARRAY cesd_vars cesd_bl cesd_yr01 cesd_yr05 cesd_yr06 cesd_yr07 cesd_yr09 cesd_yr10 cesd_yr11 cesd_yr23;
	DO OVER cesd_vars;
		IF cesd_vars=999 THEN cesd_vars=.;
	END;
RUN;

/************** Rename the time-varying cov for easier manupulation  *********/
* 1. Build the rename list using regular expressions;

DATA har.shn1;
	SET har.shn0;
RUN;

PROC SQL NOPRINT;
	SELECT name 
		INTO :varlist SEPARATED BY ' '
	FROM dictionary.columns
	WHERE libname='HAR' AND memname='SHN1';
	/* THM:
	1. dictionary.columns contains all column names for all datasets in SAS.
	2. 'HAR' and 'SHN1' needs to be uppercase
	*/
QUIT;
%put Varlist is: &varlist;

DATA _null_;
	LENGTH newname $16 rename_list $500 var $16;
	rename_list = "";
	
	DO i=1 TO COUNTW("&varlist", " ");
		var = SCAN("&varlist", i, " ");
		/* Replace _bl at the end with _0; s-substitution */		
		newname = PRXCHANGE('s/_bl$/_0/', 1, var); /*Debug: these aren't included in the rename_list*/
		/* 1: replace the first match; -1: replace all matches */
		newname = PRXCHANGE('s/_yr0*(\d+)/_\1/', 1, newname); /* can't use ('s/_yr0/', 1, var), need to take care of yr10 */
		IF newname NE var THEN rename_list = CATX(' ', rename_list, CATS(var, '=', newname));		
	END;
	CALL SYMPUTX('rename_list', rename_list);
RUN;

%PUT rename_list is: &rename_list;

* 2. Use PROC DATASETS to apply the renames;
proc datasets lib=har nolist;
   modify shn1;
   rename &rename_list nsh_bl=nsh_0 seq_bl=seq_0 cesd_bl=cesd_0; 
quit;

* Create age_group;
PROC FORMAT;
    VALUE age_grp_fmt
        20 -< 30 = "20-30"
        30 -< 40 = "30-40"
        40 -< 50 = "40-50"
        50 - HIGH = "50-70";
RUN;

DATA har.shn1;
    SET har.shn1;
    age_group = PUT(age, age_grp_fmt.);
RUN;

/*----------------------------- Summary statistics -------------------------------------*/
PROC MEANS DATA=har.shn0 MAXDEC=2; 
	VAR age group nsh_bl--cesd_yr23 anydiag_yrs;
RUN;
PROC FREQ DATA=har.shn0; 
	TABLES group white female anydiag_status;
RUN;

* Examine bivariate relationship;
PROC CORR DATA=har.shn0 PLOTS(MAXPOINTS=NONE)=MATRIX(HIST);
	VAR anydiag_yrs nsh_bl nsh_yr08 nsh_yr23 seq_bl seq_yr08 seq_yr23 age group white female;
RUN;
PROC SGSCATTER DATA=har.shn0;
	MATRIX anydiag_yrs nsh_bl nsh_yr08 nsh_yr23 seq_bl seq_yr08 seq_yr23 age group white female/ DIAGONAL=(HISTOGRAM KERNEL);
RUN;
	
/*----------------------------- KM estimator -------------------------------------*/
PROC LIFETEST DATA=har.shn0 METHOD=KM PLOTS=ALL;
	TIME anydiag_yrs*anydiag_status(0);
RUN;
PROC LIFETEST DATA=har.shn0 METHOD=KM PLOTS=S;
	TIME anydiag_yrs*anydiag_status(0);
	STRATA /GROUP=group;
RUN;
PROC LIFETEST DATA=har.shn0 METHOD=KM PLOTS=S;
	TIME anydiag_yrs*anydiag_status(0);
	STRATA /GROUP=white;
RUN;
PROC LIFETEST DATA=har.shn0 METHOD=KM PLOTS=S;
	TIME anydiag_yrs*anydiag_status(0);
	STRATA /GROUP=female;
RUN;
PROC LIFETEST DATA=har.shn1 METHOD=KM PLOTS=S;
	TIME anydiag_yrs*anydiag_status(0);
	STRATA /GROUP=age_group;
RUN;

/*----------------------------- Handle Missing Data -------------------------------------*/
* shn2: removed subjects with more than 7 NA nsh/seq, and more than 5 NA cesd;
DATA har.shn2;
	SET har.shn1;
	
	ARRAY nsh_vars {*} nsh_:;
	missing_nsh = 0;
	DO i = 1 TO DIM(nsh_vars);
		IF MISSING(nsh_vars[i]) THEN missing_nsh+1;
	END;
	ARRAY seq_vars {*} seq_:;
	missing_seq = 0;
	DO i = 1 TO DIM(seq_vars);
		IF MISSING(seq_vars[i]) THEN missing_seq+1;
	END;
	ARRAY cesd_vars {*} cesd_:;
	missing_cesd = 0;
	DO i = 1 TO DIM(cesd_vars);
		IF MISSING(cesd_vars[i]) THEN missing_cesd+1;
	END;
	
	IF (missing_nsh<=7) AND (missing_seq<=7) AND (missing_cesd<=5);
	DROP i;
RUN;

* Handle Baseline Missing;
DATA har.shn3;
	SET har.shn2;
	IF MISSING(nsh_b) THEN DO;
		ARRAY nsh_vars{*} nsh_yr01 nsh_yr03 nsh_yr04 nsh_yr05 nsh_yr06 nsh_yr07 nsh_yr08 nsh_yr09 nsh_yr10 nsh_yr23;
		DO i=1 to DIM(nsh_vars);
			IF nsh_vars[i] NE . THEN DO;
				nsh_bl = nsh_vars[i];
				LEAVE;
			END;
		END;
	END;
	IF MISSING(seq_bl) THEN DO;
		ARRAY seq_vars{*} seq_yr01 seq_yr03 seq_yr04 seq_yr05 seq_yr06 seq_yr07 seq_yr08 seq_yr09 seq_yr10 seq_yr23;
		DO i=1 to DIM(seq_vars);
			IF seq_vars[i] NE . THEN DO;
				seq_bl = seq_vars[i];
				LEAVE;
			END;
		END;
	END;
	IF MISSING(cesd_bl) THEN DO;
		ARRAY cesd_vars{*} cesd_yr01 cesd_yr05 cesd_yr06 cesd_yr07 cesd_yr09 cesd_yr10 cesd_yr11 cesd_yr23;
		DO i=1 to DIM(cesd_vars);
			IF cesd_vars[i] NE . THEN DO;
				cesd_bl = cesd_vars[i];
				LEAVE;
			END;
		END;
	END;
RUN;
	
* Use the last value carried forward method for other missing time-varying covariates;
DATA har.shn3;
	SET har.shn3;
	ARRAY nsh_vars{*} nsh_bl nsh_yr01 nsh_yr03 nsh_yr04 nsh_yr05 nsh_yr06 nsh_yr07 nsh_yr08 nsh_yr09 nsh_yr10 nsh_yr23;
	ARRAY seq_vars{*} seq_bl seq_yr01 seq_yr03 seq_yr04 seq_yr05 seq_yr06 seq_yr07 seq_yr08 seq_yr09 seq_yr10 seq_yr23;
	ARRAY cesd_vars{*} cesd_bl cesd_yr01 cesd_yr05 cesd_yr06 cesd_yr07 cesd_yr09 cesd_yr10 cesd_yr11 cesd_yr23;
	DO i = 2 to DIM(nsh_vars);
		IF MISSING(nsh_vars[i]) THEN nsh_vars[i]=nsh_vars[i-1];
	END;
	DO i = 2 to DIM(seq_vars);
		IF MISSING(seq_vars[i]) THEN seq_vars[i]=seq_vars[i-1];
	END;
	DO i = 2 to DIM(cesd_vars);
		IF MISSING(cesd_vars[i]) THEN cesd_vars[i]=cesd_vars[i-1];
	END;
	DROP i;
RUN;

/*--------------------- Data process for Cox regression ------------------------*/
* 1. Use the last value carried forward method to fill all values between bl and yr23 for the time-varying cov ;
DATA har.shn4_filled REPLACE;
    SET har.shn4;
    ARRAY nsh[0:23] nsh_0-nsh_23;
    ARRAY seq[0:23] seq_0-seq_23;
    ARRAY cesd[0:23] cesd_0-cesd_23;

    DO i = 1 TO 23;
        IF MISSING(nsh[i]) THEN nsh[i] = nsh[i-1];
        IF MISSING(seq[i]) THEN seq[i] = seq[i-1];
        IF MISSING(cesd[i]) THEN cesd[i] = cesd[i-1];
    END;
    drop i; 
RUN;

DATA har.shn4_filled;
	RETAIN caseid nsh_0-nsh_23 seq_0-seq_23 cesd_0-cesd_23; /* to retain an order*/
	SET har.shn4_filled;
RUN;

	
/*--------------------- Cox regression with age_group as ordinal ------------------------*/
* Note: LRT shows no significant difference between the cox model with age_age as ordinal and multinomial;
DATA har.shn4_filled_ord_age;
    SET har.shn4_filled;
    IF 20 <= age < 30 THEN age_group_ord = 1;
    ELSE IF 30 <= age < 40 THEN age_group_ord = 2;
    ELSE IF 40 <= age < 50 THEN age_group_ord = 3;
    ELSE IF 50 <= age <= 70 THEN age_group_ord = 4;
RUN;

* Final model 1;
PROC PHREG DATA=har.shn4_filled_ord_age;
	CLASS group(REF=FIRST);
	MODEL anydiag_yrs*anydiag_status(0)= nsh_val seq_val age_group_ord group/TIES=EFRON RL;
		ARRAY nsh_[*] nsh_0-nsh_23;
		ARRAY seq_[*] seq_0-seq_23;
		nsh_val = nsh_[anydiag_yrs];
		seq_val = seq_[anydiag_yrs];
RUN;
* Final model 3;
PROC PHREG DATA=har.shn4_filled_ord_age;
	CLASS group(REF=FIRST);
	MODEL anydiag_yrs*anydiag_status(0)= nsh_val seq_val cesd_val age_group_ord group/TIES=EFRON RL;
		ARRAY nsh_[*] nsh_0-nsh_23;
		ARRAY seq_[*] seq_0-seq_23;
		ARRAY cesd_[*] cesd_0-cesd_23;
		nsh_val = nsh_[anydiag_yrs];
		seq_val = seq_[anydiag_yrs];
		cesd_val = cesd_[anydiag_yrs];
RUN;

* Check for proportional hazard assumption: no significant time interaction;
PROC PHREG DATA=har.shn4_filled_ord_age;
	CLASS group(REF=FIRST);
	MODEL anydiag_yrs*anydiag_status(0)= nsh_val seq_val nsh_time age_group_ord group/TIES=EFRON RL;
		ARRAY nsh_[*] nsh_0-nsh_23;
		ARRAY seq_[*] seq_0-seq_23;
		nsh_val = nsh_[anydiag_yrs];
		seq_val = seq_[anydiag_yrs];
		nsh_time = nsh_val*anydiag_yrs;
RUN;
PROC PHREG DATA=har.shn4_filled_ord_age;
	CLASS group(REF=FIRST);
	MODEL anydiag_yrs*anydiag_status(0)= nsh_val seq_val seq_time age_group_ord group/TIES=EFRON RL;
		ARRAY nsh_[*] nsh_0-nsh_23;
		ARRAY seq_[*] seq_0-seq_23;
		nsh_val = nsh_[anydiag_yrs];
		seq_val = seq_[anydiag_yrs];
		seq_time = seq_val*anydiag_yrs;
RUN;
PROC PHREG DATA=har.shn4_filled_ord_age;
	CLASS group(REF=FIRST);
	MODEL anydiag_yrs*anydiag_status(0)= nsh_val seq_val age_group_ord age_time group/TIES=EFRON RL;
		ARRAY nsh_[*] nsh_0-nsh_23;
		ARRAY seq_[*] seq_0-seq_23;
		nsh_val = nsh_[anydiag_yrs];
		seq_val = seq_[anydiag_yrs];
		age_time = age_group_ord*anydiag_yrs;
RUN;
PROC PHREG DATA=har.shn4_filled_ord_age;
	CLASS group(REF=FIRST);
	MODEL anydiag_yrs*anydiag_status(0)= nsh_val seq_val age_group_ord group group_time/TIES=EFRON RL;
		ARRAY nsh_[*] nsh_0-nsh_23;
		ARRAY seq_[*] seq_0-seq_23;
		nsh_val = nsh_[anydiag_yrs];
		seq_val = seq_[anydiag_yrs];
		group_time = group*anydiag_yrs;
RUN;


/*-------------------------------------- MEM for CESD: age as ordinal -------------------------------------*/
/************ Import long data from R ***************/
FILENAME REFFILE '/home/u63463818/BSTT 550_Biostatistical_investigations/shn1_long.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=har.shn1_long;
	GETNAMES=YES;
RUN;

DATA har.shn1_long;
    SET har.shn1_long;
    nsh_num = INPUT(nsh, 8.); /* THM: wrong nsh = INPUT(nsh, 8.) */
    seq_num = INPUT(seq, 8.);
    cesd_num = INPUT(cesd, 8.);
    DROP nsh seq cesd var1;
    /* Rename new numeric variables */
    RENAME nsh_num = nsh seq_num = seq cesd_num = cesd;
RUN;
PROC CONTENTS DATA=har.shn1_long; RUN;

/************ Data exploration ***************/
PROC MEANS DATA=har.shn1 MAXDEC=2;
	VAR cesd_0--cesd_23;
RUN;

PROC CORR DATA = har.shn1;
	VAR cesd_0--cesd_23;
RUN;
PROC SGSCATTER DATA=har.shn1;
	MATRIX cesd_0--cesd_23/ DIAGONAL=(HISTOGRAM KERNEL);
RUN;

PROC CORR DATA = har.shn1;
	VAR cesd_0 cesd_1 cesd_6 cesd_10 cesd_23;
RUN;
PROC SGSCATTER DATA=har.shn1;
	MATRIX cesd_0 cesd_1 cesd_6 cesd_10 cesd_23/ DIAGONAL=(HISTOGRAM KERNEL);
RUN;

* spaghetti plot ;
PROC SGPLOT NOAUTOLEGEND DATA=har.shn1_long;
	* observed trends;
	SERIES X=time Y=cesd / GROUP=caseid LIneattrs=(THICKNESS=1);
    *overall linear ;
    REG X=time Y=cesd / NOMARKERS LINEATTRS = (COLOR=BLUE PATTERN=1 THICKNESS=3);
    *overall spline;
    PBSPLINE X=time Y=cesd / NOMARKERS LINEATTRS = (COLOR=RED THICKNESS=3);	
RUN;

/************ Data processing ***************/
* Get ordinal age_group;
DATA har.shn1_long;
	SET har.shn1_long;
	IF 20 <= age < 30 THEN age_group_ord = 1;
    ELSE IF 30 <= age < 40 THEN age_group_ord = 2;
    ELSE IF 40 <= age < 50 THEN age_group_ord = 3;
    ELSE IF 50 <= age <= 70 THEN age_group_ord = 4;
RUN;

* For the decomposition of the between- and within- effect of NSH and SEQ;
DATA;
	SET har.shn1_long;
PROC SORT;
	BY caseid;
PROC MEANS MEAN NOPRINT;
	VAR nsh seq;
	CLASS caseid;
	OUTPUT OUT=har.shn1_mean_nsh_seq (drop=_TYPE_ _FREQ_) MEAN=mean_nsh mean_seq;
RUN;

DATA har.shn1_mean_nsh_seq;
	SET har.shn1_mean_nsh_seq;
	IF caseid=. THEN DELETE;
RUN;
DATA har.shn1_long1;
	MERGE har.shn1_long har.shn1_mean_nsh_seq;
	BY caseid;
	dev_nsh = nsh - mean_nsh;
	dev_seq = seq - mean_seq;
RUN;

/************ model selection(age as ordinal): var-cov ***************/
DATA har.shn1_long1;
	SET har.shn1_long1;
	time_c = time;
RUN;

* Full model;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=UN R RCORR;
RUN;

*************** CPM *************;
* Note: TOEPHs can't coverge;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=AR(1) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=ARH(1) R RCORR;
RUN;

PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=ARMA(1,1) R RCORR; /* final model*/
RUN;

PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=TOEP(5) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=TOEP(7) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=TOEP(9) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=TOEP(11) R RCORR;
RUN;

*********** Random intercept ***********;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept/SUBJECT=caseid TYPE=UN G GCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept/SUBJECT=caseid TYPE=UN G GCORR;
	REPEATED time_c/ SUBJECT=caseid TYPE=AR(1) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept/SUBJECT=caseid TYPE=UN G GCORR;
	REPEATED time_c/ SUBJECT=caseid TYPE=ARMA(1,1) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept/SUBJECT=caseid TYPE=UN G GCORR;
	REPEATED time_c/ SUBJECT=caseid TYPE=TOEP(4) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept/SUBJECT=caseid TYPE=UN G GCORR;
	REPEATED time_c/ SUBJECT=caseid TYPE=TOEP(6) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept/SUBJECT=caseid TYPE=UN G GCORR;
	REPEATED time_c/ SUBJECT=caseid TYPE=TOEP(8) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept/SUBJECT=caseid TYPE=UN G GCORR;
	REPEATED time_c/ SUBJECT=caseid TYPE=TOEP(10) R RCORR;
RUN;

************** Random trend ***********;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept time/SUBJECT=caseid TYPE=UN G GCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept time/SUBJECT=caseid TYPE=UN G GCORR;
	REPEATED time_c/ SUBJECT=caseid TYPE=AR(1) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept time/SUBJECT=caseid TYPE=UN G GCORR;
	REPEATED time_c/ SUBJECT=caseid TYPE=ARMA(1,1) R RCORR;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept time/SUBJECT=caseid TYPE=UN G GCORR;
	REPEATED time_c/ SUBJECT=caseid TYPE=TOEP(2) R RCORR;
RUN;

************** Random quadratic ***********;
PROC MIXED DATA=har.shn1_long1 METHOD=ML;
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	RANDOM intercept time time*time/SUBJECT=caseid TYPE=UN G GCORR;
RUN;

/************ model selection(age as ordinal): fixed effects ***************/
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time seq*time*time group white female age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=ARMA(1,1) R RCORR; 
RUN;

PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time group white female age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=ARMA(1,1) R RCORR; 
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time nsh*time*time seq seq*time group white age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=ARMA(1,1) R RCORR; 
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time time*time nsh nsh*time seq seq*time group white age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=ARMA(1,1) R RCORR; 
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time nsh nsh*time seq seq*time group white age_group_ord/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=ARMA(1,1) R RCORR; 
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML; 
	CLASS caseid time_c group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time nsh seq nsh*time seq*time age_group_ord group/s;
	REPEATED time_c/ SUBJECT=caseid TYPE=ARMA(1,1) R RCORR; 
RUN;

/************ Decompose the between & within-subject effect of time-varying cov (age as ordinal) ***************/
PROC MIXED DATA=har.shn1_long1 METHOD=ML;
	CLASS caseid group(REF=FIRST) white(REF=FIRST) female(REF=FIRST) age_group(REF=FIRST);
	MODEL cesd = time mean_nsh dev_nsh dev_nsh*time mean_seq dev_seq dev_seq*time group white age_group/S;
	RANDOM intercept time time*time/SUBJECT=caseid TYPE=UN;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML;
	CLASS caseid group(REF=FIRST) white(REF=FIRST) female(REF=FIRST) age_group(REF=FIRST);
	MODEL cesd = time mean_nsh dev_nsh mean_nsh*time dev_nsh*time mean_seq dev_seq mean_seq*time dev_seq*time group white age_group/S;
	RANDOM intercept time time*time/SUBJECT=caseid TYPE=UN;
RUN;


PROC MIXED DATA=har.shn1_long1 METHOD=ML;
	CLASS caseid group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time mean_nsh mean_seq mean_nsh*time mean_seq*time dev_nsh dev_seq dev_nsh*time dev_seq*time group white age_group_ord/S;
	RANDOM intercept time time*time/SUBJECT=caseid TYPE=UN;
RUN;
* backward selection (force to keep group and age_group);
PROC MIXED DATA=har.shn1_long1 METHOD=ML;
	CLASS caseid group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time mean_nsh mean_seq mean_nsh*time mean_seq*time dev_nsh dev_seq dev_nsh*time dev_seq*time age_group_ord group/S;
	RANDOM intercept time time*time/SUBJECT=caseid TYPE=UN;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML;
	CLASS caseid group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time mean_nsh mean_seq mean_nsh*time dev_nsh dev_seq dev_nsh*time dev_seq*time age_group_ord group/S;
	RANDOM intercept time time*time/SUBJECT=caseid TYPE=UN;
RUN;
PROC MIXED DATA=har.shn1_long1 METHOD=ML;
	CLASS caseid group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time mean_nsh mean_seq mean_nsh*time dev_nsh dev_seq dev_seq*time age_group_ord group/S;
	RANDOM intercept time time*time/SUBJECT=caseid TYPE=UN;
RUN;

* Exclude group and age_group;
PROC MIXED DATA=har.shn1_long1 METHOD=ML;
	CLASS caseid group(REF=FIRST) white(REF=FIRST) female(REF=FIRST);
	MODEL cesd = time mean_nsh mean_seq mean_nsh*time mean_seq*time dev_nsh dev_seq dev_nsh*time dev_seq*time/S;
	RANDOM intercept time time*time/SUBJECT=caseid TYPE=UN;
RUN;
