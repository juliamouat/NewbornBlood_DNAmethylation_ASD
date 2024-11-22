/* Open up our lab library for ReCHARGE */
libname recharge '/home/u63604083/ReCHARGE/';

/* upload data */
FILENAME REFFILE '/home/u63604083/ReCHARGE/combined_df_children_parents.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	replace
	OUT = recharge.Jules_data;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=recharge.Jules_data; RUN;


data labeled_data;
    set recharge.Jules_data;

    /* Adding labels to variables */
    label
        AgeMomYrs = "Maternal Age (Years)"
        AgeDadYrs = "Paternal Age (Years)"
        YOB = "Child Year of Birth"
        correctsex = "Child's Sex"
        MomRace_4cat = "Maternal Race"
        DadRace_4cat = "Paternal Race"
        MomEdu_4cat = "Maternal Education"
        DadEdu_4cat = "Paternal Education"
        PaymentDelivery = "Insurance"
        OwnHome = "Home Ownership"
        dx2 = "Diagnosis";
run;

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
            tables &var * &group_var / chisq;
            ods output CrossTabFreqs=freq_&var ChiSq=chisq_&var;
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

%let continuous_vars = AgeMomYrs AgeDadYrs YOB;
%let categorical_vars = correctsex MomRace_4cat DadRace_4cat MomEdu_4cat 
						DadEdu_4cat PaymentDelivery OwnHome;

%descriptive_stats(
    data=labeled_data,
    group_var=dx2,
    continuous_vars=&continuous_vars,
    categorical_vars=&categorical_vars,
    out_file="Descriptive_Stats.xlsx"
);



%macro descriptive_stats(data, group_var, continuous_vars, categorical_vars, out_file);
    %let cont_file = %sysfunc(tranwrd(&out_file, .csv, _cont.csv));
    %let cat_file = %sysfunc(tranwrd(&out_file, .csv, _cat.csv));

    /* Initialize empty datasets for results */
    data continuous_results categorical_results;
        length Variable $50 Statistic $50 Value $50;
        stop;
    run;

    /* Continuous variables */
    %let i = 1;
    %do %while (%scan(&continuous_vars, &i) ne );
        %let var = %scan(&continuous_vars, &i);

        proc means data=&data mean std median min max;
            class &group_var;
            var &var;
            ods output Summary=summary_&var;
        run;

        data summary_&var;
            set summary_&var;
            length Variable $50 Statistic $50 Value $50;
            Variable = "&var";
        run;

        proc append base=continuous_results data=summary_&var force; run;

        %let i = %eval(&i + 1);
    %end;

    /* Categorical variables */
    %let i = 1;
    %do %while (%scan(&categorical_vars, &i) ne );
        %let var = %scan(&categorical_vars, &i);

        proc freq data=&data;
            tables &var * &group_var / chisq;
            ods output CrossTabFreqs=freq_&var ChiSq=chisq_&var;
        run;

        data freq_&var;
            set freq_&var;
            length Variable $50 Statistic $50 Value $50;
            Variable = "&var";
        run;

        proc append base=categorical_results data=freq_&var force; run;

        %let i = %eval(&i + 1);
    %end;

    /* Export Continuous Results to CSV */
    proc export data=continuous_results
        outfile="&cont_file"
        dbms=csv
        replace;
    run;

    /* Export Categorical Results to CSV */
    proc export data=categorical_results
        outfile="&cat_file"
        dbms=csv
        replace;
    run;

    %put NOTE: Continuous results saved to &cont_file;
    %put NOTE: Categorical results saved to &cat_file;
%mend descriptive_stats;

%let continuous_vars = AgeMomYrs AgeDadYrs YOB;
%let categorical_vars = correctsex MomRace_4cat DadRace_4cat MomEdu_4cat DadEdu_4cat PaymentDelivery OwnHome;

%descriptive_stats(
    data=labeled_data,
    group_var=dx2,
    continuous_vars=&continuous_vars,
    categorical_vars=&categorical_vars,
    out_file="/home/u63604083/ReCHARGE/Descriptive_Stats.csv"
);



%macro descriptive_stats(data, group_var, continuous_vars, categorical_vars, out_base);
    %let excel_file = &out_base..xlsx;
    %let cont_csv_file = &out_base._cont.csv;
    %let cat_csv_file = &out_base._cat.csv;

    /* Initialize empty datasets for results */
    data continuous_results categorical_results;
        length Variable $50 Statistic $50 Value $50;
        stop;
    run;

    /* Continuous Variables */
    %let i = 1;
    %do %while (%scan(&continuous_vars, &i) ne );
        %let var = %scan(&continuous_vars, &i);

        proc means data=&data mean std median min max;
            class &group_var;
            var &var;
            ods output Summary=summary_&var;
        run;

        data summary_&var;
            set summary_&var;
            length Variable $50 Statistic $50 Value $50;
            Variable = "&var";
        run;

        proc append base=continuous_results data=summary_&var force; run;

        %let i = %eval(&i + 1);
    %end;

    /* Categorical Variables */
    %let i = 1;
    %do %while (%scan(&categorical_vars, &i) ne );
        %let var = %scan(&categorical_vars, &i);

        proc freq data=&data;
            tables &var * &group_var / chisq;
            ods output CrossTabFreqs=freq_&var;
        run;

        data freq_&var;
            set freq_&var;
            length Variable $50 Statistic $50 Value $50;
            Variable = "&var";
        run;

        proc append base=categorical_results data=freq_&var force; run;

        %let i = %eval(&i + 1);
    %end;

    /* Save to Excel */
    ods excel file="&excel_file" options(sheet_interval="none");

    /* Continuous Results to Excel */
    proc print data=continuous_results noobs;
        title "Continuous Variables Summary";
    run;

    /* Categorical Results to Excel */
    proc print data=categorical_results noobs;
        title "Categorical Variables Summary";
    run;

    ods excel close;

    /* Save Continuous Results to CSV */
    proc export data=continuous_results
        outfile="&cont_csv_file"
        dbms=csv
        replace;
    run;

    /* Save Categorical Results to CSV */
    proc export data=categorical_results
        outfile="&cat_csv_file"
        dbms=csv
        replace;
    run;

    %put NOTE: Excel file saved to &excel_file;
    %put NOTE: Continuous results saved to &cont_csv_file;
    %put NOTE: Categorical results saved to &cat_csv_file;
%mend descriptive_stats;


%let continuous_vars = AgeMomYrs AgeDadYrs YOB;
%let categorical_vars = correctsex MomRace_4cat DadRace_4cat MomEdu_4cat DadEdu_4cat PaymentDelivery OwnHome;

%descriptive_stats(
    data=labeled_data,
    group_var=dx2,
    continuous_vars=&continuous_vars,
    categorical_vars=&categorical_vars,
    out_base=/home/u63604083/ReCHARGE/Descriptive_Stats
);

ODS RTF file="/home/u63604083/ReCHARGE/Descriptive_Stats.rtf"

%macro descriptive_stats(data, group_var, continuous_vars, categorical_vars, out_file);
    ods listing close;
    ods excel file="&out_file" options(sheet_interval="none");

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
            tables &var * &group_var / chisq;
            ods output CrossTabFreqs=freq_&var ChiSq=chisq_&var;
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


%let continuous_vars = AgeMomYrs AgeDadYrs YOB;
%let categorical_vars = correctsex MomRace_4cat DadRace_4cat MomEdu_4cat DadEdu_4cat PaymentDelivery OwnHome;

%descriptive_stats(
    data=labeled_data,
    group_var=dx2,
    continuous_vars=&continuous_vars,
    categorical_vars=&categorical_vars,
    out_file="/home/u63604083/ReCHARGE/Descriptive_Stats.xlsx"
);

ODS RTF close
