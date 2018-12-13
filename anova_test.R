#testing two-way anova
library(FSA)
library(ggplot2)
library(multcompView)
library(lsmeans)
library(grid)
library(nlme)
library(lme4)
library(lmerTest)
library(rcompanion)
library(Rmisc)

CS_df = cbind(input.y.df, rep("CS", dim(input.y.df)[1]))
colnames(CS_df) = c("Value", "Sessions", "Genotype")

SUN1_df = cbind(input.y.df, rep("SUN1", dim(input.y.df)[1]))
colnames(SUN1_df) = c("Value", "Sessions", "Genotype")

test_df = rbind(CS_df, SUN1_df)

sum_df = summarySE(test_df, measurevar = "Value", groupvars = c("Sessions", "Genotype"))

pd = position_dodge(.2)
g = ggplot(sum_df, aes(x=Sessions, y=Value, color=Genotype))

g + geom_errorbar(aes(ymin=Value-se, ymax=Value+se), width=.2, size=0.7, position=pd) +
    geom_point(shape=15, size=4, position=pd) +
    theme_bw() +
    theme(axis.title.y = element_text(vjust=1.8), 
          axis.title.x = element_text(vjust=-0.5),
          axis.title = element_text(face="bold")) +
    scale_color_manual(values=c("black", "blue"))


boxplot(Value~Sessions:Genotype, data=test_df)


model = lm(Value ~ Sessions + Genotype + Sessions : Genotype, data = test_df)
library(car)
Anova(model, type="II")

aa = aov(Value ~ Sessions + Genotype + Sessions : Genotype, data = test_df)

summary(model)

hist(residuals(model), col="darkgray")
plot(fitted(model), residuals(model))