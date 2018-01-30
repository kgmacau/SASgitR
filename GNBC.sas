

/*This macro is a generalization of the NBC macro written by Vadim Pliner,
  which the whole framework and discrete input variables are originally written by Valim.
  I just added the part 5.1 - input continuous variables and modified part 6
  In future work, I will add kernel estimation method into this macro.
  For further reference, please read the original paper titled "A SAS Macro for Naive Bayes Classfication"*/
%macro GNBC(train=, score=, nclass=, target=, inputVarD=, inputVarC=);
       /*Part 1: 
         Check whether all the macro parameters are specified*/
       %let error=0;
	   %if %length(&train.)=0 %then %do;
	       %put ERROR: Value for macro parameter TRAIN is missing;
		   %let error=1;
	   %end;
	   %if %length(&score) = 0 %then %do;
           %put ERROR: Value for macro parameter SCORE is missing ;
           %let error=1;
       %end;
       %if %length(&nclass) = 0 %then %do;
           %put ERROR: Value for macro parameter NCLASS is missing ;
           %let error=1;
       %end;
       %if %length(&target) = 0 %then %do;
           %put ERROR: Value for macro parameter TARGET is missing ;
           %let error=1;
       %end;
       %if %length(&inputVarD) = 0 and %length(&inputVarC) = 0 %then %do;
           %put ERROR: Values for macro parameters inputVarD and inputVarD are missing ;
           %let error=1;
      %end;
      %if &error=1 %then %goto finish;
	  /* Part 2:
	     Check if data sets "train" and "score" exist; */
	  %if %sysfunc(exist(&train))=0 %then %do;
	      %put ERROR: data set &train does not exist;
	      %let error=1;
	  %end;
	  %if %sysfunc(exist(&score))=0 %then %do;
	      %put ERROR: data set &score does not exist;
	      %let error=1;
	  %end;
	  %if &error=1 %then %goto finish;
	  
      /* Part 3: Compute the number of features */
	     /*3.1 Compute the number of discreate features specified in "inputVarD"*/
	  %let nvard=0;
      %do %while (%length(%scan(&inputVarD, &nvard.+1))>0);
	      %let nvard=%eval(&nvard+1);	  
      %end;
	  %put &nvard.;
	     /*3.2 Compute the number of continuous features specified in "inputVarC"*/
	  %let nvarc=0;
	  %do %while (%length(%scan(&inputVarC, &nvarc.+1))>0);
	      %let nvarc=%eval(&nvarc+1);	  
      %end;
	  %put &nvarc.;
      /* Part 4:
	     Calculating the prior probabilities and counts for all classes, i.e. P(Y=c) & #{Y=c} for all c=1,...,C. 
	      The results are stored in macro variables Prior1,..., PriorC and Count1,..., CountC, respectively*/
      proc freq data=&train noprint;
	       tables &target. / out=_priors_;
	  run;
	  %do k=1 % to &nclass;
	      proc sql noprint;
		       select percent, count into :Prior&k, :Count&k
			   from _priors_
			   where &target=&k;
		  quit;
	  %end;
	  /* Part 5: Compute condifional probability*/
	      /* 5.1 if discreate variable, Compute the conditional probabilities */
	  %if &nvard.>0 %then %do;
	      data discds;
				       set &score;				 
		  run;
		  %do i=1 %to &nvard.;
		      %let vard=%scan(&inputVarD, &i);
			  %do j=1 %to &nclass;
			      proc freq data=&train noprint;
				       tables &vard / out=_&vard.&j (drop=count) missing;
					   where &target=&j.;
				  run;
			  %end;
			  data _&vard.;
			       merge %do k=1 %to &nclass.;
				             _&vard.&k (rename=(percent=percent&k))
						 %end;;
				   by &vard;
				   %do k=1 %to &nclass;
				       if missing(percent&k) then percent&k=0;
				   %end;
			  run;

			  proc sql noprint;
			       create table discds 
				   as select a.*
				      %do k=1 %to &nclass;
				           , b.percent&k as percent&k._&vard
				      %end;
				   from discds as a left join _&vard as b
				   on a.&vard=b.&vard;
			  quit;
		
	      %end;
	  %end;
	      /* 5.2 if continuous variable, compute the conditional probability density value assuming to be normal distribution */
	  %if &nvarc.>0 %then %do;
	      data contds;
			 set &score;				 
		  run;
		  %do i=1 %to &nvarc.;
		      %let varc=%scan(&inputVarC, &i);

			  proc sort data=&train.; by &target.; run;
	          proc means data=&train. noprint;
	               var &varc.;
		           by &target.;
		           output out=_&varc. mean=mean_&varc std=std_&varc;
	          run;

			  proc transpose data=_&varc. out=_mean_&varc prefix=mean_&varc;
                   var  mean_&varc ;
				   id   &target.;
			  run;
				  
			  proc transpose data=_&varc. out=_std_&varc prefix=std_&varc;
                   var std_&varc;
				   id   &target.;
			  run;

			  data _&varc.;
			       merge _mean_&varc _std_&varc;
			  run;

			  data contds;
			       set contds;
                   ord=1;
			  run;

			  data _&varc.;
			       set _&varc.;
				   ord=1;
			  run;

			  proc sort data=contds; by ord; run;

			  proc sort data=_&varc.; by ord; run;

			  data contds;
			       merge _&varc. contds;
				   by ord;
				   /* calculate the conditional probability of different variables at different levels of outcome variable*/
				   %do k=1 %to &nclass;
				       pi=constant("pi");
				       prob&k._&varc=(1/sqrt(2*pi*(std_&varc&k**2)))*exp(-(&varc.-mean_&varc&k)**2/2*(std_&varc&k**2));
				   %end;
			  run;

	      %end;
	  %end;

      /*Combine the dataset containing discrete input variables and the dataset containing continuouts variables*/
	  %if &nvard.>0 and &nvarc.>0 %then %do;
	      proc sort data=discds; by &target. _all_; 
		  run;
	      data discds;
		       set discds;
			   i=_n_;
		  run;
		  proc sort data=contds; by &target.  _all_; 
		  run;
		  data contds;
		       set contds;
			   i=_n_;
		  run;
		  proc sort data=discds; by i; 
		  run;
		  proc sort data=contds; by i;
		  run;

		  data &score.;
		       merge discds contds;
			   by i;
		  run;
	  %end;
	  %else %if &nvard.>0 %then %do;
	      data &score.;
		       set discds;
		  run;
	  %end;
      %else %if &nvarc.>0 %then %do;
	      data &score.;
		       set contds;
		  run;
	  %end;


	  /* Part 6: 
	      The last data step does all multiplications to obtain the final NB classification.
	      Actually, instead of multiplications it does summations of logarithms to calculate the logarithm of the product
	      and to avoid a potential floating-point underflow resulting from multiplying lots of probabilities, 
	      which are between 0 and 1. A new variable _class_ (class number) is added to the data set &score. */  
      data &score ;
           set  &score.;
		   maxprob=0;
		   %do k=1 %to &nclass;
		   	       product=log(&&prior&k);

				   %if &nvard.>0 %then %do;
			           array varsd&k (&nvard)
				       %do i=1 %to &nvard.; percent&k._%scan(&inputVard, &i) %end;;
					   do L=1 to &nvard;
					      if varsd&k(L)>0 then product=product+log(varsd&k(L));
						  else product=product+log(0.5)-log(&&count&k);
					   end;
				   %end;

				   %if &nvarc.>0 %then %do;
					   array varsc&k (&nvarc)
				       %do j=1 %to &nvarc.; prob&k._%scan(&inputVarc, &j) %end;;
	                   do C=1 to &nvarC;
					      if varsc&k(C)>0 then product=product*varsc&k(C);
						  else product=product;
					   end;
					%end;

				   if product>maxprob then do;
				      maxprob=product;
					  _class_=&k;
				   end;
		   %end;
		run;

	    %put &nclass.;
		%finish:;      	       
%mend GNBC;


/*Example: the outcome variable is age, the input variables are discrete height and weight*/
data class;
     set sashelp.class;
	 agen=age-10;
run;

proc rank data=class groups=5 out=class1;
     var  height weight;
	 ranks  decile_height decile_weight;
run;

data class2;
     set class1;
run;

%GNBC(train=class1, score=class2, nclass=6, target=agen, inputVarD=decile_height, inputVarC=weight );

%macro misclass_rate(score=, target=);
proc freq data=&score;
     tables &target*_class_ / out=_freq_out_ noprint;
run;
data misclass;
     set _freq_out_ end=last;
     if &target^=_class_ then misclass+percent;
run;
%mend;

%misclass_rate(score=class2, target=agen);

