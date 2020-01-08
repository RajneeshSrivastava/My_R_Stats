library (maigesPack)
c=as.matrix(read.delim("/cRBP.txt", sep="\t",header=T, row.names = 1))
m=matrix(ncol=4) # no. of column(basically designing matrix for output)

for(i in 2:nrow(c))
{ 
   gene_constant = rownames(c)[1]
   gene_variable = rownames(c)[i]
   pvalue = bootstrapMI (as.numeric(c[1,]),as.numeric(c[i,]),bRep=100)
   MIvalue = MI(as.numeric(c[1,]),as.numeric(c[i,]))
   ele = c(gene_constant,gene_variable,MIvalue,pvalue)
   m = rbind(m,c(gene_constant,gene_variable,MIvalue,pvalue))
}
write.table (m,file = "Test.tsv", sep = "	")
