


%* Below Macro is used to calculate how many bottles of beer one can drink in maximum 
   when he/she has x bottles of beer initially;

%Macro MaxBottleBeer(x);
    data Calculation_Process;
	   length Beer Bottle_Remained Lid_Remained initialValue 8;
	   initialValue=&x.;
	   retain Beer Bottle_Remained Lid_Remained &x.;	   
	   do until(Bottle_Remained<2 and Lid_Remained<4);
	      if Bottle_Remained>=2 then do;
	         Beer=Beer + floor(Bottle_Remained/2) ;
		     Lid_Remained=Lid_Remained + floor(Bottle_Remained/2);
		     Bottle_Remained=floor(Bottle_Remained/2) + mod(Bottle_Remained, 2);
			 output;
	      end;
	      if Lid_Remained>=4 then do;
	         Beer=Beer + floor(Lid_Remained/4) ;
		     Bottle_Remained=Bottle_Remained  + floor(Lid_Remained/4);
		     Lid_Remained=floor(Lid_Remained/4) + mod(Lid_Remained, 4);		
             output; 
	      end;
	   end;	  
	   put "The maximum number of beer you can drink is " Beer= ;
	   put "The remained number of null bottle is " Bottle_Remained=;
	   put "The remained number of Lid is " Lid_Remained=;
	run;
%mend;
%MaxBottleBeer(x=5);

