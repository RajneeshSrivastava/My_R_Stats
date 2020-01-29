#This code can be used to generate a scatter plot between two variables and display linear rigression line along with its basic statistics

lm_eqn <- function(m){
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(pval)~"="~p*","~~italic(r)^2~"="~r2, 
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2),
                          p = format(summary(fit)$coefficients[2,4], digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 2)))
    as.character(as.expression(eq));                 
}

#File format
#Col1	Col2	Col3
#RowName1	x1	y1
#RowName2	x2	y2

fit <- lm(Y ~ X, data = mydata)
MyPlot=ggplot(mydata, aes(x=X, y=X.1)) +
    labs(x = "My X-axis", y ="My Y-axis")+
    geom_point(size=2.5, shape=19,color = "yellowgreen")+
    geom_text(label=rownames(mydata),fontface="bold",size = 3.0,color="orange",hjust=0.25, vjust=1.5)+
    geom_smooth(method="lm", se=TRUE, level=0.5)+
    geom_text(x=7.5,y=2.0,size = 3.0, label = lm_eqn(fit), parse = TRUE)
	