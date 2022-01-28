/**********************************************/
/* Longitudinal 718 - Section 01 Introduction */
/* Graphs */
/**********************************************/
/* Homework 1 Sample Code */
/**********************************************/
%let todaysdate=%sysfunc(today(),yymmddn8.);
/* put the output in an .rtf file in local directory */
ODS RTF FILE="Homeworkl_&todaysdate..rtf" STYLE=PearlJ;
ODS GRAPHICS ON;
/**********************************/
/* An example of generating data */
/* for a longitudinal model for */
/* 40 people with 9 time points. */
/* using random effects */
/* */
/**********************************/
/* Model E(Y_1ij) = (beta01 + gamma_0i)+ (beta11+gamma1i)*t + error_ij
*/
/* E(Y_2ij) = (beta02 + gamma_0i)+ (beta12+gamma1i)*t + error_ij
*/
/* where i is nested in group (k=1, 2) and */
/* t=0, 2, 4, 6, 8, 10, 12, 14, 16 */
/* (gamma0i, gamma1i)' ~ N(0, Sigma) */
/* Sigma = [v1 cov] */
/* [cov v2 ] */
/* error_ij ~ N(0, s_squared) */
/* input the values of parameters you need */
/* to set. Then generate the random */
/* slope and intercept for each person (i) */
/* within each group (k=1, 2) */
/********************************************/
* Generate the random gamma vectors using *
* proc SIMNORMAL *;
/********************************************/



**************/
* Create the variance/covariance matrix and mean vector;
* This is in the same format as a SAS PROC CORR;
DATA cov_mean (TYPE=COV);
 INPUT _TYPE_ $ 1-4 _NAME_ $ 5-7 g0 g1;
 DATALINES;
COV g0 4 -0.2
COV g1 -0.2 0.08
MEAN . 0 0
;
run;
/* take a look at this dataset */
TITLE1 "Variance-Covariance Matrix of the Random Effects";
PROC PRINT DATA=cov_mean;
VAR _ALL_;
run;
/* Use that covariance matrix and mean vector */
/* to generate gamma vectors for FIRST group */
/* Generate extra in case intercept is below */
/* 30 or >=40,which means that must be excluded */
/* from this group. */
PROC SIMNORMAL DATA=cov_mean SEED=2022 /*pick your own random
number generator seed*/
 OUT=grp1_REs NUMREAL=40;
VAR g0 g1;
run;
/* take a look at what you generated */
TITLE1 "Random Sample of 40 from Bivariate Normal";
TITLE2 "First 10 observations";
PROC PRINT DATA=grp1_REs (obs=10);
 VAR _all_;
 run;
/* See how close the estimated variances are to truth*/
PROC CORR DATA= grp1_REs plots=matrix(histogram);
 VAR g0 g1;
 run;
/* now add those to preset intercepts/slopes */
/* in the FIRSTo groups,and add random error of */
/* measurement. */
DATA grp1_bmi exclusions; SET grp1_REs;
 ID=Rnum; /* Use Rnum as the study ID */
 SEED=2022; /*for random measurment error */
 intercept=31;
 slope = -0.45;
 s = 0.98; /*random error standard deviation */
 /* check inclusion criteria for group 1*/
 /* at week 0 */
 initial_bmi = intercept+g0;
 IF (30 <= initial_bmi < 40)
 THEN DO;
 do week=0 to 16 by 2;
 error =RAND('Normal', 0, s);
 bmi=initial_bmi + (slope+g1)*week + error;
 OUTPUT grp1_bmi;
 end;
 END;
 ELSE output Exclusions;
run;

/* see exclusions */
/* to many? */
PROC PRINT DATA=exclusions;
run;
/************************************/
/* repeat steps for group 2 changing*/
/* slope and intercept. */
/************************************/
PROC SIMNORMAL DATA=cov_mean SEED=2022 /*pick your own random
number generator seed*/
 OUT=grp2_REs NUMREAL=40;
VAR g0 g1;
run;
DATA grp2_bmi exclusions2; SET grp2_REs;
 ID = Rnum + 50;
 SEED=2021; /*for random measurment error */
 intercept=41;
 slope = -0.55;
 s = 0.98; /*random error standard deviation */
 /* check inclusion criteria for group 1*/
 /* at week 0 */
 initial_bmi = intercept+g0;
 IF (40 <= initial_bmi)
 THEN DO;
 do week=0 to 16 by 2;
 error =RAND('Normal', 0, s);
 bmi=initial_bmi + (slope+g1)*week + error;
 OUTPUT grp2_bmi;
 end;
 END;
 ELSE output Exclusions2;
run;
proc print data=exclusions2;
run;
/*************************************/
/* Put them together, keeping only */
/* the first 20 people in each group */
/*************************************/
DATA bmi; SET grp1_bmi(obs=180 in=in1)
 grp2_bmi(obs=180 in=in2);
 IF in1 then group=1;
 ELSE if in2 then group=2;
 LABEL bmi='BMI';
 LABEL week='Week';
 LABEL group='Study Arm';
 run;
*** Summarize Data ****;
*** create nicer labels for groups ***;
PROC FORMAT;
 VALUE grpfmt 1="Obese Group"
 2="Morbidly Obese Group";
 run;
TITLE1 "Summary Statistics on BMI over Time";
PROC MEANS N MEAN STD VAR Min Median Max DATA=bmi MAXDEC=2;
 CLASS group week;
 VAR bmi;
 FORMAT group grpfmt.;
 OUTPUT out=bmi_means MEAN=Ave_BMI STD=Std_BMI; /* for plotting*/
 run;
/* done using a different procedure */
PROC TABULATE DATA=bmi;
 CLASS group week;
 VAR bmi;
 TABLE (week ALL='Over Time'),
 (group ALL='Overall')*bmi*(N*f =4.0 MEAN*F=6.2 STD*F=6.2 Min*F=6.2
Median*F=6.2 max*f=6.2);
 FORMAT group grpfmt.;
run;

*** Plot Means in Each Group ****;
PROC SGPLOT DATA=bmi_means;
WHERE group NE . and week NE .; /* These are overall averages */
SERIES x=week Y=ave_bmi / group=group groupLC=group;
 /* separates by group and uses a different line color for
each*/
XAXIS OFFSETMIN= 0.1 OFFSETMAX=0.1 LABEL="Study Week" VALUES=(0 to 16
by 2);
YAXIS LABEL="Body Mass Index Means" MIN=20 MAX = 50;
run;
*** Spaghetti Plots ****;
PROC SORT DATA=bmi; by id week; run;
PROC SGPLOT DATA=bmi NOAUTOLEGEND;
SERIES x=week Y=bmi / group=ID groupLC=group Name="plot";
 /* separates by group and uses a different line color for
each*/
REG x=week y=bmi / group=group lineattrs=(color="Black") NOMARKERS;
XAXIS OFFSETMIN= 0.1 OFFSETMAX=0.1 LABEL="Study Week" VALUES=(0 to 16
by 2);
YAXIS LABEL="Body Mass Index Means" MIN=10 MAX=50 ;
LEGENDitem type=line name='Obese' / label="Obese" lineattrs=
(color='Blue');
LEGENDitem type=line name="Morbidly Obese" /LABEL="Morbidly Obese"
lineattrs=(color="DarkRed");
KEYLEGEND "Obese" "Morbidly Obese";
run;
/**********************/
/* Case 1 Missingness */
/* Question 2c */
/**********************/
DATA case1; SET bmi;
 SEED=2022;
 u = RAND('Uniform', 0, 1);
 If week >= 4 and U <.2 THEN delete;
 run;
TITLE "Case 1 Missingness";
PROC TABULATE DATA=case1;
 CLASS group week;
 VAR bmi;
 TABLE (week ALL='Over Time'),
 (group ALL='Overall')*bmi*(N*f =4.0 MEAN*F=6.2 STD*F=6.2 Min*F=6.2
Median*F=6.2 max*f=6.2);
 FORMAT group grpfmt.;
run;
PROC MEANS N MEAN STD VAR Min Median Max DATA=case1 NOPRINT MAXDEC=2;
 CLASS group week;
 VAR bmi;
 FORMAT group grpfmt.;
 OUTPUT out=case1_means MEAN=Ave_BMI STD=Std_BMI; /* for plotting*/
 run;

*** Plot Means in Each Group for Case 1 data ****;
PROC SGPLOT DATA=case1_means;
 WHERE group NE . AND week NE .; /* These are overall averages */
 SERIES x=week Y=ave_bmi / group=group groupLC=group;
 /* separates by group and uses a different line color for
each*/
 XAXIS OFFSETMIN= 0.1 OFFSETMAX=0.1 LABEL="Study Week" VALUES=(0
to 16 by 2);
 YAXIS LABEL="Body Mass Index Means" MIN=20 MAX = 50;
 run;
*** Spaghetti Plots for case 1 ****;
PROC SORT DATA=case1; by id week; run;
PROC SGPLOT DATA=case1 NOAUTOLEGEND;
 SERIES x=week Y=bmi / group=ID groupLC=group Name="plot";
 /* separates by group and uses a different line color for
each*/
 XAXIS OFFSETMIN= 0.1 OFFSETMAX=0.1 LABEL="Study Week" VALUES=(0
to 16 by 2);
 YAXIS LABEL="Body Mass Index Means" MIN=10 MAX=50 ;
 REG X=week Y=bmi / group=group NOMARKERS lineattrs=
(color="Black");
 LEGENDitem type=line name='Obese' / label="Obese" lineattrs=
(color='Blue');
 LEGENDitem type=line name="Morbidly Obese" /LABEL="Morbidly
Obese" lineattrs=(color="DarkRed");
 KEYLEGEND "Obese" "Morbidly Obese";
 run;
/**********************/
/* Case 2 Missingness */
/* Question 2d */
/**********************/
PROC SORT DATA=bmi; by id; run;
DATA case2_part1; set bmi;
 Seed = 2022;
 IF (week > =4)
 THEN if ranuni(seed)<=.223 THEN bmi=.;
 ELSE; /* do nothing*/
drop seed;
run;
PROC SORT DATA=case2_part1; by id week; run;
DATA case2; SET case2_part1;
BY id week;
RETAIN firstmiss;
 if (week = 0 or week = 2) THEN firstmiss=.;
 ELSE /*week >= 4*/
 if bmi =. AND firstmiss=.
 THEN firstmiss=week;
 ELSE
 IF (firstmiss >. and week > firstmiss)
 THEN bmi =.;
run;

TITLE "Case 2 Missingness";
PROC TABULATE DATA=case2;
 CLASS group week;
 VAR bmi;
 TABLE (week ALL='Over Time'),
 (group ALL='Overall')*bmi*(N*f =4.0 MEAN*F=6.2 STD*F=6.2 Min*F=6.2
Median*F=6.2 max*f=6.2);
 FORMAT group grpfmt.;
run;
PROC MEANS N MEAN STD VAR Min Median Max DATA=case2 NOPRINT MAXDEC=2;
 CLASS group week;
 VAR bmi;
 FORMAT group grpfmt.;
 OUTPUT out=case2_means MEAN=Ave_BMI STD=Std_BMI; /* for plotting*/
 run;

*** Plot Means in Each Group for Case 2 data ****;
PROC SGPLOT DATA=case2_means;
 WHERE group NE . AND week NE .; /* These are overall averages */
 SERIES x=week Y=ave_bmi / group=group groupLC=group;
 /* separates by group and uses a different line color for
each*/
 XAXIS OFFSETMIN= 0.1 OFFSETMAX=0.1 LABEL="Study Week" VALUES=(0
to 16 by 2);
 YAXIS LABEL="Body Mass Index Means" MIN=20 MAX = 50;
 run;
*** Spaghetti Plots for case 2 ****;
PROC SORT DATA=case2; by id week; run;
PROC SGPLOT DATA=case1 NOAUTOLEGEND;
 SERIES x=week Y=bmi / group=ID groupLC=group Name="plot";
 /* separates by group and uses a different line color for
each*/
 REG x=week Y=bmi / group=group LINEATTRS=(color="Black")
NOMARKERS;
 XAXIS OFFSETMIN= 0.1 OFFSETMAX=0.1 LABEL="Study Week" VALUES=(0
to 16 by 2);
 YAXIS LABEL="Body Mass Index Means" MIN=10 MAX=50 ;
 LEGENDitem type=line name='Obese' / label="Obese" lineattrs=
(color='Blue');
 LEGENDitem type=line name="Morbidly Obese" /LABEL="Morbidly
Obese" lineattrs=(color="DarkRed");
 KEYLEGEND "Obese" "Morbidly Obese";
 run;

/**********************/
/* Case 3 Missingness */
/* Question 2e */
/**********************/
PROC SORT DATA=bmi; by id week; run;
DATA case3_part1; set bmi;
 Seed = 2022;
 IF (week > =4)
 THEN If group = 1 and ranuni(seed) <=0.045 THEN bmi=.;
 ELSE IF group = 2 and ranuni(seed) <=0.2235 THEN bmi=.;
drop seed;
run;
DATA case3; SET case3_part1;
BY id week;
RETAIN firstmiss;
 if (week = 0 or week = 2) THEN firstmiss=.;
 ELSE /*week >= 4*/
 IF bmi =. AND firstmiss=.
THEN firstmiss=week;
ELSE;
IF (firstmiss >. and week > firstmiss)
THEN bmi =.;
run;
TITLE "Case 3 Missingness";
PROC TABULATE DATA=case3;
 CLASS group week;
 VAR bmi;
 TABLE (week ALL='Over Time'),
 (group ALL='Overall')*bmi*(N*f =4.0 MEAN*F=6.2 STD*F=6.2 Min*F=6.2
Median*F=6.2 max*f=6.2);
 FORMAT group grpfmt.;
run;
PROC MEANS N MEAN STD VAR Min Median Max DATA=case3 NOPRINT MAXDEC=2;
 CLASS group week;
 VAR bmi;
 FORMAT group grpfmt.;
 OUTPUT out=case3_means MEAN=Ave_BMI STD=Std_BMI; /* for plotting*/
 run;
*** Plot Means in Each Group for Case 3 data ****;
PROC SGPLOT DATA=case3_means;
 WHERE group NE . AND week NE .; /* These are overall averages */
 SERIES x=week Y=ave_bmi / group=group groupLC=group;
/* separates by group and uses a different line color for
each*/
 XAXIS OFFSETMIN= 0.1 OFFSETMAX=0.1 LABEL="Study Week" VALUES=(0
to 16 by 2);
 YAXIS LABEL="Body Mass Index Means" MIN=20 MAX = 50;
 run;
*** Spaghetti Plots for case 3 ****;
PROC SORT DATA=case3; by id week; run;
PROC SGPLOT DATA=case1 NOAUTOLEGEND;
 SERIES x=week Y=bmi / group=ID groupLC=group Name="plot";
/* separates by group and uses a different line color for
each*/
 REG X=week Y=bmi / group=group NOMARKERS LINEATTRS=
(COLOR='Black');
 XAXIS OFFSETMIN= 0.1 OFFSETMAX=0.1 LABEL="Study Week" VALUES=(0
to 16 by 2);
 YAXIS LABEL="Body Mass Index Means" MIN=10 MAX=50 ;
 LEGENDitem type=line name='Obese' / label="Obese" lineattrs=
(color='Blue');
 LEGENDitem type=line name="Morbidly Obese" /LABEL="Morbidly
Obese" lineattrs=(color="DarkRed");
 KEYLEGEND "Obese" "Morbidly Obese";
 run; Don't worry about the
LEGENDITEM keyword being red.
That just means that the
editor doesn't recognize it,
but it works.
**** Compare lines across cases ******;
DATA all; SET bmi_means (in=in1)
case1_means (in=in2)
case2_means (in=in3)
case3_means (in=in4);
WHERE group NE . and week ne .;
miss_type = 1*in1 + 2*in2 + 3*in3 + 4*in4;
LABEL miss_type = "Type";
run;
/* create better labels for types of missiness */
PROC FORMAT;
 VALUE typefmt 1="Complete Data"
2="Intermittent Missingness"
3="Drop-out Missingness"
4="Differential Dropout Missingness";
 run;
TITLE1 "Comparison of Types of Missingness";
PROC SGPANEL DATA=all;
 PANELBY miss_type / columns=4;
 SERIES x=week Y=ave_bmi / group=group groupLC=group;
 COLAXIS OFFSETMIN= 0.1 OFFSETMAX=0.1 LABEL="Study Week" VALUES=(0
to 16 by 2);
 ROWAXIS LABEL="Body Mass Index Means" MIN=20 MAX = 50;
 FORMAT miss_type typefmt.;
 run;
/* Shown differently */
PROC SGPANEL data=all;
 PANELBY group / columns=2;
 SERIES X=week Y=ave_bmi / group=miss_type groupLC=miss_type;
 COLAXIS OFFSETMIN= 0.1 OFFSETMAX=0.1 LABEL="Study Week" VALUES=(0
to 16 by 2);
 ROWAXIS LABEL="Body Mass Index Means" MIN=20 MAX = 50;
 FORMAT miss_type typefmt.;
 run;