**********************************************************************************************************************************
* Below is a classical example of simpson paradox in medical area in which the study was undertaken by Charig on a historical 
        comparion of success rates in removing kidney stones.
* Original Reference Paper: Confounding and Simpson's paradox - https://www.ncbi.nlm.nih.gov/pubmed/7804052

* Treatment 1 (TRT1): Open surgery (1972 - 1980)
* Treatment 2 (TRT2): Percutaneous nephrolithotomy (1980 - 1985)

* l2: Stone diameter <  2 cm
* g2: Stone diameter >= 2 cm

* TRT1_l2_s, TRT1_l2_n, TRT1_l2_r: The successful, total number and successful rate of patients using TRT1 for stones of < 2 cm
* TRT1_s, TRT1_n, TRT1_r: The successful, total number and successful rate of patients using TRT1 whatever the length of stones
* For the meannings of other parameters, please make reference to the above

*****
*****
Findings:
1. TRT 1 - Open surgery is more effective than trt 2 - Percutaneous nephrolithotomy in removing the stones of < 2 cm:  93% vs 87%
2. TRT 1 - Open surgery is more effective than trt 2 - Percutaneous nephrolithotomy in removing the stones of >= 2 cm: 73% vs 69%
3. However, once combining the patients with stones of < 2 cm and stones of >= 2 cm, the result reverses, 
            Open surgery is less effective than Percutaneous nephrolithotomy in removing the stones: 78% vs 83%
4. Investigating the reasons, we can easily find that the ratio of stone < 2 cm and stone >= 2 cm in two treatment groups are totally
     reverse, 87/263 vs 270/80. So as to keep the balance between two treatment groups, we adjust the ratio via tentatively multiplying
      3 for both the successful, total number of patients using TRT1 
      for stones of < 2 cm and patients using TRT2 for stones of >= 2 cm. thus, we get the adjusted result, that is:
      Open surgery is more effective than Percutaneous nephrolithotomy in removing the stones 83% vs 78%;
****
****
Discussion:
1. As a whole, the patient numbers in two treatment groups are equal: 87 + 263 vs 270 + 80. 
               However, the internal structure are totally different which leads to unbalance between two groups: 
               more patients with stones >= 2 cm in group Open surgery and more patients with stones < 2 cm in another group.
               thus, it is not rational to make a conclusion that Open surgery is less effective Percutaneous nephrolithotomy
                      by directly using this unblanced data..
           
2. As two separative categories, stone < 2 cm and stone >= 2 cm, there also exist unblances between two groups, ie., sample size. 
               For stone < 2 cm,  the patient numbers in two groups are unbalanced: 87  vs 270.
               For stone >= 2 cm, the patient numbers in two groups are unbalanced: 263 vs 80. 
               thus, it is also not rational to make a conclusion that Open surgery is more effective Percutaneous nephrolithotomy
                     for both two categories - stone < 2 cm and stone >= 2 cm by directly using this unblanced data.
3. Asssume that the data after multiplying 3 in above finds 4 is the real data, in this case, we can directly analyze the data 
               both in a whole combining approach or in the separative category because the data is balanced both in sample size 
               and in internal structure.
**********************************************************************************************************************************;
data simpson_paradox;
     length TRT1_l2_r TRT2_l2_r TRT1_g2_r TRT2_g2_r TRT1_r TRT2_r TRT1_r_adj TRT2_r_adj $10;
     TRT1_l2_s=81; TRT1_l2_n=87; 
	 TRT2_l2_s=234; TRT2_l2_n=270; 
	 TRT1_g2_s=192; TRT1_g2_n=263;
	 TRT2_g2_s=55; TRT2_g2_n=80; 

	 /* compare the rate between TRT1 and TRT2 for stones of  < 2 cm*/
	 TRT1_l2_r=put(TRT1_l2_s/TRT1_l2_n, 4.2);
	 TRT2_l2_r=put(TRT2_l2_s/TRT2_l2_n, 4.2);

	 /* compare the rate between TRT1 and TRT2 for stones of  >= 2 cm*/
	 TRT1_g2_r=put(TRT1_g2_s/TRT1_g2_n, 4.2);
	 TRT2_g2_r=put(TRT2_g2_s/TRT2_g2_n, 4.2);

	 /* compare the rate between TRT1 and TRT2 for all stones*/
	 TRT1_r=put((TRT1_g2_s+TRT1_l2_s)/(TRT1_g2_n+TRT1_l2_n), 4.2);
	 TRT2_r=put((TRT2_g2_s+TRT2_l2_s)/(TRT2_g2_n+TRT2_l2_n), 4.2);

	 /* compare the rate between TRT1 and TRT2 for all stones based on the adjusted number of patients*/
	 TRT1_r_adj=put((TRT1_g2_s+3*TRT1_l2_s)/(TRT1_g2_n+3*TRT1_l2_n), 4.2);
	 TRT2_r_adj=put((3*TRT2_g2_s+TRT2_l2_s)/(3*TRT2_g2_n+TRT2_l2_n), 4.2);

	 put TRT1_l2_r=  TRT2_l2_r=;
	 put TRT1_g2_r=  TRT2_g2_r=;
	 put TRT1_r=     TRT2_r=;
	 put TRT1_r_adj= TRT2_r_adj=;
run;





