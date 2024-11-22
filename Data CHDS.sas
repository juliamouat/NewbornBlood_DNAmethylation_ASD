/* Open up our lab library for ReCHARGE */
libname recharge '/home/u63604083/ReCHARGE/';

/* load data for CHDS*/

FILENAME REFFILE '/home/u63604083/ReCHARGE/CHDS_F1_and_F2.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	replace
	OUT=recharge.chds;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=recharge.chds; RUN;

data chds_labeled_data;
    set recharge.chds;

    /* Adding labels to variables */
    label
        F1MOMAGE = "Maternal Age (Years)"
        F1FATHERAGE = "Paternal Age (Years)"
        F2BirthYear = "Child Year of Birth"
        F2SEX = "Child's Sex"
        mom_race_new = "Maternal Race"
        dad_race_new = "Paternal Race"
        Mom_edu_new = "Maternal Education"
        Dad_edu_new = "Paternal Education"
        Prenatalinsurance = "Insurance"
        F2Aut = "Diagnosis";
run;



%macro descriptive_stats(data, group_var, continuous_vars, categorical_vars, out_file);
    ods listing close;
    ods excel file=Descriptive_stats options(sheet_interval="none");

    %put Generating descriptive statistics and p-values;

    %let i = 1;
    
    /* Continuous variables */
    %do %while (%scan(&continuous_vars, &i) ne );
        %let var = %scan(&continuous_vars, &i);

        /* Summary statistics */
        proc means data=&data mean std median min max;
            var &var;
            class &group_var;
            ods output Summary=summary_&var;
        run;

        /* T-test */
        proc ttest data=&data;
            class &group_var;
            var &var;
            ods output TTests=ttest_&var;
        run;

        /* Export to Excel */
        ods excel options(sheet_name="&var (Cont)" sheet_interval="none");
        proc print data=summary_&var noobs label; 
            title "Summary Statistics for &var"; 
        run;

        proc print data=ttest_&var noobs label;
            title "T-Test for &var"; 
        run;

        %let i = %eval(&i + 1);
    %end;

    %let i = 1;

    /* Categorical variables */
    %do %while (%scan(&categorical_vars, &i) ne );
        %let var = %scan(&categorical_vars, &i);

        /* Frequency table */
	proc freq data=&data;
   	 tables &var * &group_var / chisq fisher;
    	ods output CrossTabFreqs=freq_&var ChiSq=chisq_&var FisherExact=fisher_&var;
	run;


        /* Export to Excel */
        ods excel options(sheet_name="&var (Cat)" sheet_interval="none");
        proc print data=freq_&var noobs label;
            title "Frequency Table for &var";
        run;

        proc print data=chisq_&var noobs label;
            title "Chi-Square Test for &var"; 
        run;

        %let i = %eval(&i + 1);
    %end;

    ods excel close;
    ods listing;
%mend descriptive_stats;

%let continuous_vars = F1MOMAGE F1FATHERAGE F2BirthYear;
%let categorical_vars = F2SEX mom_race_new dad_race_new Mom_edu_new 
						Dad_edu_new Prenatalinsurance;

%descriptive_stats(
    data=chds_labeled_data,
    group_var=F2Aut,
    continuous_vars=&continuous_vars,
    categorical_vars=&categorical_vars,
    out_file="Descriptive_Stats.xlsx"
);
