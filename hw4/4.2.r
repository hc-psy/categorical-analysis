crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat",header=T)
fitqt <- glm(y ~ color, 
            family=binomial(link=logit),
            data=crabs)
crabs$color <- factor(crabs$color)
fitql <- glm(y ~ color, 
           family=binomial(link=logit),
           contrasts=list(color=contr.treatment(4,base=4,contrasts=TRUE)),
           data=crabs)
summary(fitql)
summary(fitqt)

#classification tables
pihat.qt <- vector(length=173)
for (i in 1:173){
  pihat.qt[i] <-  predict.glm(update(fitqt, subset=-i),
                           newdata=crabs[i,], type="response")
}
yhat.qt <- as.numeric(pihat.qt > 0.50)
confusionqt <- table(crabs$y, yhat.qt)
confusionqt

pihat.ql <- vector(length=173)
for (i in 1:173){
  pihat.ql[i] <-  predict.glm(update(fitql, subset=-i),
                              newdata=crabs[i,], type="response")
}
yhat.ql <- as.numeric(pihat.ql > 0.50)
confusionql <- table(crabs$y, yhat.ql)
confusionql

#ROC
rocplotqt <- roc(y ~ fitted(fitqt), data=crabs)
rocplotql <- roc(y ~ fitted(fitql), data=crabs)
a <- auc(rocplotqt)
b <- auc(rocplotql)
signif(a,10)
signif(b,10)

#naive way of power
cor(crabs$y,fitted(fitql))
cor(crabs$y,fitted(fitqt))

#which is fitted well
anova(fitql, fitqt, test="LRT")

#two predictive variables
fit2 <- glm(y ~ color+weight, 
             family=binomial(link=logit),
             data=crabs)
beta.glm(fit2, x = TRUE, y = TRUE)
