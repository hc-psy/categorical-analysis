---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
plot(cars)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.


```{r}
stud<-read.table("http://users.stat.ufl.edu/~aa/cat/data/Students.dat",header=T)
```

```{r}
fit1 <- glm(abor ~ 1, family = binomial, data = stud)
summary(fit1)
```
```{r}
fit2 <- glm(abor ~ ideol, family = binomial, data = stud)
summary(fit2)
anova(fit2,fit1,test="Chisq")
```

```{r}
fit3 <- glm(abor ~ relig, family = binomial, data = stud)
summary(fit3)
anova(fit3,fit1,test="Chisq")
```

```{r}
fit4 <- glm(abor ~ news, family = binomial, data = stud)
summary(fit4)
anova(fit4,fit1,test="Chisq")
```

```{r}
fit5 <- glm(abor ~ hsgpa, family = binomial, data = stud)
summary(fit5)
anova(fit5,fit1,test="Chisq")
```

```{r}
fit6 <- glm(abor ~ factor(gender), family = binomial, data = stud)
summary(fit6)
anova(fit6,fit1,test="Chisq")
```

```{r}
fit7 <- glm(abor ~ ideol+relig+news, family = binomial, data = stud)
summary(fit7)
anova(fit7,fit2,test="Chisq")
anova(fit7,fit3,test="Chisq")
anova(fit7,fit4,test="Chisq")
```

```{r}
fit8 <- glm(abor ~ ideol+relig+news+gender+hsgpa, family = binomial, data = stud)
summary(fit8)
anova(fit8,fit7,test="Chisq")
```

```{r}
fit9 <- glm(abor ~ ideol+relig+news+ideol*relig+ideol*news+relig*news, family = binomial, data = stud)
summary(fit9)
anova(fit9,fit7,test="Chisq")
```

```{r}
fit7$deviance; fit7$df.residual
1 - pchisq(fit7$deviance, fit7$df.residual)
```

```{r}
fit10 <- glm(abor ~ gender+age+hsgpa+cogpa+dhome+dres+tv+sport+news+aids+veg+ideol+relig+affirm, family = binomial, data = stud)
stepAIC(fit10)
```

```{r}
fit11 <- glm(veg ~ gender+age+hsgpa+cogpa+dhome+dres+tv+sport+news+aids+abor+ideol+relig+affirm, family = binomial, data = stud)
summary(fit11)
anova(fit11,glm(veg ~ 1, family = binomial, data = stud),test="Chisq")
1 - pchisq(50.725-26.645, 59-45)
```

```{r}
for (x in colnames(stud)){
  print(x)
  print(summary(glm(veg ~ stud[,x], family = binomial, data = stud)))
}

```

```{r}
vr <- c(1,1,0,0)
dr <- c(1,0,1,0)
yes <- c(53,11,0,4)
no <- c(414,37,16,139)
fit31 <- glm(yes/(no+yes)~vr+dr, weights = (no+yes), family = binomial)
summary(fit31)
cbind(rstandard(fit31,type="pearson"), residuals(fit31,type="pearson"), rstandard(fit31,type="deviance"),residuals(fit31,type="deviance"))
```

```{r}
lungs<-read.table("http://users.stat.ufl.edu/~aa/cat/data/Lungs.dat",header=T)
lungs
```


```{r}
l <- as.matrix(lungs)
```

```{r}
odds.ratio <-  function(x){
  return ((x[1,1]*x[2,2])/(x[1,2]*x[2,1]))
}
x = matrix(c(0,0,0,0),nrow=2,ncol=2)
for (i in seq(0,4)){
  a<-matrix(strtoi(l[(1+2*i):(2+2*i),3:4]),nrow=2,ncol=2)
  for (j in seq(2)){
    for (k in seq(2)){
      x[j,k] = x[j,k]+a[j,k]
    }
  }
}
print(x)
print(odds.ratio(x))


```

```{r}
fit40=glm(Yes/(Yes+No)~factor(City),family=binomial(link=logit),data=lungs,weights=(Yes+No))
summary(fit40)
1-pchisq(fit40$deviance,fit40$df.residual)
cbind(rstandard(fit40,type="pearson"), residuals(fit40,type="pearson"), rstandard(fit40,type="deviance"),residuals(fit40,type="deviance"))
```



```{r}
fit41=glm(Yes/(Yes+No)~factor(City)+Smoking,family=binomial(link=logit),data=lungs,weights=(Yes+No))
summary(fit41)
1-pchisq(fit41$deviance,fit41$df.residual)
cbind(rstandard(fit41,type="pearson"), residuals(fit41,type="pearson"), rstandard(fit41,type="deviance"),residuals(fit41,type="deviance"))
```

```{r}
LX<-data.frame(LI=c('H','H','H','H','L','L','L','L'),sex=c('f','f','m','m','f','f','m','m'),OP=c(0,1,0,1,0,1,0,1),yes=c(3,2,4,1,5,3,5,6),no=c(0,0,0,0,0,2,4,11))
fit51 <- glm(yes/(yes+no)~factor(LI),family=binomial(link=logit),data=LX,weights=(yes+no))
summary(fit51)
Anova(fit51)
confintModel(fit51, objective="ordinaryDeviance", method="zoom", endpoint.tolerance = 1e-08)
fit52 <- glm(yes/(yes+no)~factor(sex),family=binomial(link=logit),data=LX,weights=(yes+no))
summary(fit52)
fit53 <- glm(yes/(yes+no)~OP,family=binomial(link=logit),data=LX,weights=(yes+no))
summary(fit53)
fit54 <- glm(yes/(yes+no)~factor(LI)+factor(sex)+OP,family=binomial(link=logit),data=LX,weights=(yes+no))
summary(fit54)
Anova(fit54)
confintModel(fit54, objective="ordinaryDeviance", method="zoom", endpoint.tolerance = 1e-08)
```

```{r}
# > LX<-data.frame(LI=c('H','H','H','H','L','L','L','L'),sex=c('f','f','m','m','f','f','m','m'),OP=c(0,1,0,1,0,1,0,1),yes=c(3,2,4,1,5,3,5,6),no=c(0,0,0,0,0,2,4,11))
# > fit51 <- glm(yes/(yes+no)~factor(LI),family=binomial(link=logit),data=LX,weights=(yes+no))
# > summary(fit51)
# 
# Call:
# glm(formula = yes/(yes + no) ~ factor(LI), family = binomial(link = logit), 
#     data = LX, weights = (yes + no))
# 
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -1.44956   0.00008   0.00012   0.20659   2.52800  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)
# (Intercept)    20.06    4357.04   0.005    0.996
# factor(LI)L   -19.95    4357.04  -0.005    0.996
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 19.4327  on 7  degrees of freedom
# Residual deviance:  8.6256  on 6  degrees of freedom
# AIC: 20.671
# 
# Number of Fisher Scoring iterations: 18
# 
# > Anova(fit51)
# Analysis of Deviance Table (Type II tests)
# 
# Response: yes/(yes + no)
#            LR Chisq Df Pr(>Chisq)   
# factor(LI)   10.807  1   0.001011 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > confintModel(fit51, objective="ordinaryDeviance", method="zoom", endpoint.tolerance = 1e-08)
# Preliminary iteration .. Done
# 
# Profiling for parameter (Intercept) ... Done
# Profiling for parameter factor(LI)L ... Done
# Zooming for parameter (Intercept) ...
# Zooming for parameter factor(LI)L ...
#                Lower     Upper
# (Intercept) 1.552307       Inf
# factor(LI)L     -Inf -1.346829
# attr(,"fitted object")
# fit51
# > fit52 <- glm(yes/(yes+no)~factor(sex),family=binomial(link=logit),data=LX,weights=(yes+no))
# > summary(fit52)
# 
# Call:
# glm(formula = yes/(yes + no) ~ factor(sex), family = binomial(link = logit), 
#     data = LX, weights = (yes + no))
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.4792  -0.1607   0.8416   1.1617   2.3003  
# 
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)  
# (Intercept)    1.8718     0.7595   2.464   0.0137 *
# factor(sex)m  -1.8073     0.8403  -2.151   0.0315 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 19.433  on 7  degrees of freedom
# Residual deviance: 13.553  on 6  degrees of freedom
# AIC: 25.598
# 
# Number of Fisher Scoring iterations: 4
# 
# > fit53 <- glm(yes/(yes+no)~OP,family=binomial(link=logit),data=LX,weights=(yes+no))
# > summary(fit53)
# 
# Call:
# glm(formula = yes/(yes + no) ~ OP, family = binomial(link = logit), 
#     data = LX, weights = (yes + no))
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -1.7360   0.1389   1.1688   1.3385   1.7134  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)   
# (Intercept)   1.4469     0.5557   2.604  0.00922 **
# OP           -1.5270     0.6849  -2.230  0.02578 * 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 19.433  on 7  degrees of freedom
# Residual deviance: 13.898  on 6  degrees of freedom
# AIC: 25.943
# 
# Number of Fisher Scoring iterations: 4
# 
# > fit54 <- glm(yes/(yes+no)~factor(LI)+factor(sex)+OP,family=binomial(link=logit),data=LX,weights=(yes+no))
# > summary(fit54)
# 
# Call:
# glm(formula = yes/(yes + no) ~ factor(LI) + factor(sex) + OP, 
#     family = binomial(link = logit), data = LX, weights = (yes + 
#         no))
# 
# Deviance Residuals: 
#        1         2         3         4         5         6         7         8  
#  0.00002   0.00003   0.00005   0.00005   1.07088  -0.51727  -0.36813   0.27912  
# 
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)  
# (Intercept)     23.4920 11084.3781   0.002   0.9983  
# factor(LI)L    -21.3842 11084.3781  -0.002   0.9985  
# factor(sex)m    -1.6362     0.9123  -1.794   0.0729 .
# OP              -1.2204     0.7712  -1.582   0.1135  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 19.4327  on 7  degrees of freedom
# Residual deviance:  1.6278  on 4  degrees of freedom
# AIC: 17.673
# 
# Number of Fisher Scoring iterations: 20
# 
# > Anova(fit54)
# Analysis of Deviance Table (Type II tests)
# 
# Response: yes/(yes + no)
#             LR Chisq Df Pr(>Chisq)   
# factor(LI)    6.9149  1   0.008548 **
# factor(sex)   3.7210  1   0.053731 . 
# OP            2.6362  1   0.104451   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# > confintModel(fit54, objective="ordinaryDeviance", method="zoom", endpoint.tolerance = 1e-08)
# Preliminary iteration .... Done
# 
# Profiling for parameter (Intercept) ... Done
# Profiling for parameter factor(LI)L ... Done
# Profiling for parameter factor(sex)m ... Done
# Profiling for parameter OP ... Done
# Zooming for parameter (Intercept) ...
# Zooming for parameter factor(LI)L ...
# Zooming for parameter factor(sex)m ...
# Zooming for parameter OP ...
#                  Lower       Upper
# (Intercept)   2.679308         Inf
# factor(LI)L       -Inf -0.80598077
# factor(sex)m -3.699682  0.02505359
# OP           -2.827147  0.24901300
# attr(,"fitted object")
# fit54
```

