LI <- c(8,8,10,10,12,12,12,14,14,14,16,16,16,18,20,20,20,22,22,24,26,28,32,34,38,38,38)
y <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,1,1,0,1,1,1,0)
fit1 <- glm(y ~ LI, family = binomial)
plot(seq(-100,100,0.5),
     predict.glm(fit1,
                 data.frame(LI = seq(-100,100,0.5)),
                 type = "response"),
     type = "l",
     xlab = "LI or x",
     ylab = expression(pi*(x)),
     col = transparent("#392F5A", trans.val = .1))
abline(v = 0,
       lty = 2)
rect(xleft = min(LI),
     ybottom = -1,
     xright = max(LI),
     ytop = 1.1,
     col = transparent("#F4D06F", trans.val = .5),
     border = NA)
rect(xleft = quantile(LI,.25),
     ybottom = -1,
     xright = quantile(LI,.75),
     ytop = 1.1,
     col = transparent("#A0CCDA", trans.val = .7),
     border = NA)
points(LI,
       y,
       pch = 16,
       col = transparent("#ED6A5A", trans.val = .5))
points(x=26,
       y=0.5,
       pch = 16,
       col = transparent("#ACA885", trans.val = .1))