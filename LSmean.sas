data twoway;
      input Treatment Block y @@;
      datalines;
   1 1 17   1 1 28   1 1 19   1 1 21   1 1 19
   1 2 43   1 2 30   1 2 39   1 2 44   1 2 44
   1 3 16
   2 1 21   2 1 21   2 1 24   2 1 25
   2 2 39   2 2 45   2 2 42   2 2 47
   2 3 19   2 3 22   2 3 16
   3 1 22   3 1 30   3 1 33   3 1 31
   3 2 46
   3 3 26   3 3 31   3 3 26   3 3 33   3 3 29   3 3 25
   ;


   title "Unbalanced Two-way Design";
   ods select ModelANOVA Means LSMeans;
    
   proc glm data=twoway;
      class Treatment Block;
      model y = Treatment|Block;
      means Treatment;
      lsmeans Treatment;
   run;
    
   ods select all;

   proc sort data=twoway; by treatment block; run;
   
   
   data sum;
        set twoway;
		by treatment block;
		retain Sumtrt Sumblk Ntrt Nblk 0;
		Sumtrt=Sumtrt+y;
		Sumblk=Sumblk+y;
		Ntrt=Ntrt+1;
        Nblk=Nblk+1;
		if first.treatment then do;
           Sumtrt=y;
		   Ntrt=1;
		end;
		if first.block then do;
           Sumblk=y;
		   Nblk=1;
		end;
		if last.treatment then meantrt=Sumtrt/Ntrt;
		if last.Block then meanblk=Sumblk/Nblk;
	run;

	data lssum;
	    set sum(where=(~missing(meanblk)));
		by treatment block;
		retain blksum 0;		
		if first.treatment then blksum=meanblk;
		else blksum=blksum+meanblk;
		if last.treatment then lsmean=blksum/block;
	run;


		    
