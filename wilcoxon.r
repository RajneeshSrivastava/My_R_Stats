v=as.matrix(read.table("./VFS/Values.txt",sep="\t",header=F))
m=matrix(nrow=40,ncol=2)
for(i in 1:nrow(v)){
	  m[i,2]=wilcox.test(as.numeric(v[i,2:15]),as.numeric(v[i,16:29]))$p.value
	  m[i,1]=v[i,1]
                   }
write.table(m,"Test_pval.txt", quote=FALSE, row.names = FALSE, col.names = FALSE, sep ="\t")
