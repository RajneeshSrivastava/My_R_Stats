c=as.matrix(read.delim("/Input.txt", sep="\t",header=T, row.names = 1))
m=matrix(ncol=4) # no. of column(basically designing matrix for output)

for(i in 2:nrow(c))
{ 
   gene_constant = rownames(c)[1]
   gene_variable = rownames(c)[i]
   pvalue = cor.test(as.numeric(c[1,]),as.numeric(c[i,]), method = "spearman",use="pairwise.complete.obs")$p.value
   corvalue = cor(as.numeric(c[1,]),as.numeric(c[i,]), method = "spearman",use="pairwise.complete.obs")
   ele = c(gene_constant,gene_variable,corvalue,pvalue)
   m = rbind(m,c(gene_constant,gene_variable,corvalue,pvalue))
}
write.table (m,file = "Morbid_All_Genes.tsv", sep = "	")
