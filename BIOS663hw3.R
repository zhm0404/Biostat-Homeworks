
fit<-lm(y~DNTT,data=ds)
summary(fit)
plot(ds$DNTT,ds$y)

plot(ds$CD19,ds$y)

fit1<-lm(y~1,data=ds)
fit2<-lm(y~DNTT,data=ds)
anova(fit1,fit2)
anova(fit2,fit1)

fit3<-lm(y~HPS4+ZNF423+DNTT,data=ds)
fit4<-lm(y~HPS4+ZNF423,data=ds)
anova(fit4,fit3)

fit<-lm(y~HPS4+ZNF423+EBF1+CD19+DNTT+MYO5C+CCN2+INTS3+CCDC50,data=ds)
summary(fit)

ds2<-read.table("leuk_noisy_pred.tsv",header = TRUE,sep = "\t")
ds2

fit<-lm(y~EBF1,data=ds2)
summary(fit)