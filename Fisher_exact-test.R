setwd("/path/Fisher_test")
Data = read.table("data1.txt", skip=1, header=F,sep="\t")

Data$Fisher.p   = NA
Data$Odds.ratio = NA

for(i in 1:length(Data$V1)){

Data$Fisher.p[i]   = fisher.test(matrix(c(Data$V2[i],Data$V3[i],Data$V4[i],Data$V5[i]), nrow=2))$p.value

Data$Odds.ratio[i] = fisher.test(matrix(c(Data$V2[i],Data$V3[i],Data$V4[i],Data$V5[i]), nrow=2))$estimate
}

write.table(Data,"results_swap.txt",sep ="\t")

'''
For Fisher test, organize the data in below format:

Test	Observed	Total-Observed	Expected	Total-Expected
Test1	609	16921	75	17455
Test2	550	16980	64	17466
Test3	588	16942	87	17443
Test4	561	18637	73	19125

Example-
> SNP_enrich <-
+     matrix(c(609,16921,75,17455),
+            nrow = 2,
+            dimnames = list(SNP = c("GWAS", "Non-GWAS"),
+                            Type = c("Test_Peaks", "Random_peaks")))
> SNP_enrich
          Type
SNP        Test_Peaks Random_peaks
GWAS         609     75
Non-GWAS   16921  17455
> fisher.test(SNP_enrich, alternative = "two.sided")

	Fisher's Exact Test for Count Data

data:  SNP_enrich
p-value < 2.2e-16
alternative hypothesis: true odds ratio is not equal to 1
95 percent confidence interval:
  6.573238 10.803852
sample estimates:
odds ratio 
  8.375773 
'''
