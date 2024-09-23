#Task 1
getwd()

# Task 2
spruce.df=myread("SPRUCE.csv")#MS pg478
head(spruce.df)

# Task 3 
## Make a scatter plot of the data 
quartz()
plot(Height~BHDiameter , bg ="Blue", pch =21,cex= 1.2,xlim=c(0,max(BHDiameter)*1.1), ylim=c(0,max(Height)*1.1), main = "Correlation Between Spruce Height and BH Diameter", spruce.df)

### o	Does there appear to be a straight line relationship?
###  Yes there appears to be a straight line relationship because the scatterplots are increasing

library(s20x)
quartz()
layout(matrix(1:3 ,nrow =1, ncol = 3,byrow = TRUE))
trendscatter(Height~BHDiameter, f=0.5, spruce.df)
trendscatter(Height~BHDiameter, f=0.6, spruce.df)
trendscatter(Height~BHDiameter, f=0.7, spruce.df)

spruce.lm = lm(Height~BHDiameter,spruce.df)
#summary(spruce.lm)

abline(spruce.lm)

### o	Comment on the graph, is a straight line appropriate? 
### Yes, this straight line is appropriate for this graph because the dots closely follow the line.

# Task 4
quartz()
layout(matrix(1:4 ,nrow =2, ncol = 2,byrow = TRUE))
layout.show(4)

plot(BHDiameter~Height, bg = "blue", pch = 21, cex = 1.2, xlim = c(0, max(BHDiameter)), ylim = c(0, max(Height)), main = "Fitted Line",spruce.df)
abline(spruce.lm)


plot(BHDiameter~Height, bg = "blue", pch = 21, cex = 1.2, xlim = c(0, max(BHDiameter)), ylim = c(0, max(Height)), main = "Fitted Line",spruce.df)
abline(spruce.lm)
segments(spruce.df$BHDiameter, spruce.df$Height, spruce.df$BHDiameter, fitted(spruce.lm), col = "blue", lty = 2)


