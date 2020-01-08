c=as.matrix(read.delim("/LM_in_28k.txt", sep="\t",header=T, row.names = 1))
m=matrix(ncol=5) # no. of column(basically designing matrix for output)

for(i in 4:nrow(c))
{ 
   PMI = rownames(c)[3]
   gene_variable = rownames(c)[i]
   G = summary(lm((as.numeric(c[3,]))~(as.numeric(c[i,]))))$coefficients[2, 4]
   R = summary(lm((as.numeric(c[3,]))~(as.numeric(c[i,]))))$r.squared
   RD = summary(lm((as.numeric(c[3,]))~(as.numeric(c[i,]))))$adj.r.squared	
   f = summary(lm((as.numeric(c[3,]))~(as.numeric(c[i,]))))$fstatistic
   p <- pf(f[1],f[2],f[3],lower.tail=F)
   attributes(p) <- NULL
   ele = c(gene_variable, G, R, RD, p)
   m = rbind(m,c(gene_variable, G, R, RD, p))
}
write.table (m,file = "Age-Exp.tsv", sep = "	")
#1PMI
#2Gender
#3Age
