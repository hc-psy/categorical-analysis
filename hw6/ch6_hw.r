
## CH 6 HW

## used libraries 

```{r}
library(VGAM);library(MASS)
```

## dataset

```{r}
life <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Afterlife.dat", header=TRUE)
lung <- data.frame(therapy=c("seq","seq","alt","alt"), ##欄位名稱=欄位值
                  gender=c("M","F","M","F"),
                  progDisease=c(28,4,41,12),
                  responseNo=c(45,12,44,7),
                  responsePartial=c(29,5,20,3),
                  responseComplete=c(26,2,20,1))
```

## 6.4

```{r}
fit640 <- vglm(cbind(yes,undecided,no) ~ gender, family=multinomial(refLevel="no"), data=life)
summary(fit640)
```


```{r}
fit641 <- vglm(cbind(yes,undecided,no) ~ gender, family=multinomial(refLevel="undecided"), data=life)
summary(fit641)
```

## 6.14

```{r}
#adjacent-categoriy ordinal logits
f6140 <-vglm(cbind(yes,undecided,no) ~ gender, family=acat(parallel=T,reverse=T), data=life)
summary(f6140)
```

```{r}
#adjacent-categoriy ordinal logits
f6140 <-vglm(cbind(yes,undecided,no) ~ gender, family=acat(parallel=T,reverse=T), data=life)
summary(f6140)
```


```{r}
#seq ordinal logits
f6141 <-vglm(cbind(yes,undecided,no) ~ gender, family=sratio(parallel=T), data=life)
summary(f6141)

```

## 6.8

```{r}
# only null
fit680 <- vglm(cbind(responseNo,responsePartial,responseComplete) ~ 1,
               family=cumulative(parallel=TRUE),
               data=lung)
summary(fit680)
```




```{r}
# only therapy effect
fit681 <- vglm(cbind(responseNo,responsePartial,responseComplete) ~ therapy,
               family=cumulative(parallel=TRUE),
               data=lung)
summary(fit681)
lrtest(fit681,fit680)
```

```{r}
# only gender effect
fit682 <- vglm(cbind(responseNo,responsePartial,responseComplete) ~ gender,
               family=cumulative(parallel=TRUE),
               data=lung)
summary(fit682)
lrtest(fit682,fit680)
```


```{r}
# only prog effect
fit683 <- vglm(cbind(responseNo,responsePartial,responseComplete) ~ progDisease,
               family=cumulative(parallel=TRUE),
               data=lung)
summary(fit683)
lrtest(fit683,fit680)
```

```{r}
# all IV
fit684 <- vglm(cbind(responseNo,responsePartial,responseComplete) ~ therapy + gender + progDisease,
               family=cumulative(parallel=TRUE),
               data=lung)
summary(fit684)
lrtest(fit684,fit681)
lrtest(fit684,fit682)
lrtest(fit684,fit683)
```




```{r}
# therapy with gender
fit685 <- vglm(cbind(responseNo,responsePartial,responseComplete) ~ therapy + gender,
               family=cumulative(parallel=TRUE),
               data=lung)
summary(fit685)

```

```{r}
# therapy + gender + interaction
fit686 <- vglm(cbind(responseNo,responsePartial,responseComplete) ~ therapy + gender + therapy:gender,
               family=cumulative(parallel=TRUE),
               data=lung)
lrtest(fit686,fit685)

```


```{r}
# all IV
fit684 <- vglm(cbind(responseNo,responsePartial,responseComplete) ~ therapy + gender + progDisease,
               family=cumulative(parallel=TRUE),
               data=lung)
# all IV with their interactions
fit685 <- vglm(cbind(responseNo,responsePartial,responseComplete) ~ therapy + gender + progDisease+ therapy:gender,
               family=cumulative(parallel=TRUE),
               data=lung)
summary(fit684);summary(fit685)
lrtest(fit685,fit684)
```


























