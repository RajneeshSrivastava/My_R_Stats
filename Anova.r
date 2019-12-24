y=as.data.frame(read.table("/home/rsrivast/R-Anova/Input.txt",sep="\t", header=T))
m=matrix(nrow=1,ncol=55344)
for(i in 2:ncol(y))
{
    m[1,i]=summary(aov(y[,i] ~ PH, data=y))[[1]][[1,"Pr(>F)"]]
}
write.table(m,"/home/rsrivast/R-Anova/Output.txt",sep="\t")