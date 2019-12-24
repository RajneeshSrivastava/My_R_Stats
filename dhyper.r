y=as.matrix(read.table("INPUT.txt",sep="\t",header=T))
m=matrix(nrow=22,ncol=2)
for(i in 1:nrow(y))
{
	m[i,2]=dhyper(as.numeric(y[i,2]),as.numeric(y[i,3]),as.numeric(y[i,4]),as.numeric(y[i,5]))
	m[i,1]=y[i,1]
}
write.table(m,"Test_pval.txt", quote=FALSE, row.names = FALSE, col.names = FALSE, sep ="\t")