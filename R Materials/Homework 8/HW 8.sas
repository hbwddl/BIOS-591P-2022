*BIOS 591P Spring 2022
*HW 8 Answer Key Code;

*file import wizard used to import the data
 into temporary data set named 'titanic';

*proc contents for imported data set;
PROC CONTENTS DATA=WORK.titanic;
RUN;

*Create a formats for survived and sex;
PROC FORMAT;
  VALUE survfmt 0 ='Died'
                1 ='Survived';
  VALUE agefmt  0 = '< 15 Yrs Old'
                1 = '>= 15 Yrs Old';
  VALUE sexfmt  0 ='Female'
                1 ='Male';
RUN;

*create binary variable for age, and associate the the formats with the variables;
DATA titanic;
  SET titanic;
  Passenger_age = (Age >= 15); *Create dummary variable, = 1 for age >= 15, = 0 for any other age;
  if Age = . then Passenger_Age = .; *if age is missing, the passenge_age is set to missing;
  FORMAT Survived survfmt.;
  FORMAT Passenger_age agefmt.;
  FORMAT Sex sexfmt.;
RUN;

*simple descriptive statistics for each variable;
PROC FREQ DATA=WORK.titanic;
  TABLES Survived TicketClass Sex Passenger_Age;
RUN;

*bivariate analyses;
PROC FREQ DATA=WORK.titanic;
  TABLES (TicketClass Sex Passenger_Age)*Survived / CHISQ ODDSRATIO(CL=WALD) ;
RUN;

*Compare avg ages for 3 ticket classes (one-way ANOVA);
PROC LOGISTIC DATA=WORK.titanic PLOTS=ROC; *roc option produces ROC curve;
  CLASS TicketClass (PARAM=REF REF='3');
  MODEL Survived(Event='Survived') = Sex Passenger_Age TicketClass;
  ODDSRATIO Ticketclass / DIFF=ALL; * ORs comparing ticket classes (pairwise);
RUN;
