remotes::install_github("DoseResponse/drcData")
remotes::install_github("DoseResponse/medrc")
library("medrc")
# https://doseresponse.github.io/medrc/articles/medrc.html#hierarchical-dose-response-models
data(vinclozolin)

m1 <- metadrm(effect ~ conc,
              data=vinclozolin,
              fct=LL.3(),
              ind=exper,
              struct="UN")
summary(m1)

m2 <- medrm(effect ~ conc, data=vinclozolin,
            random=b + d + e ~ 1|exper,
            fct=LL.3(), start=c(0.5, 2000, 0.05))
summary(m2)

ED(m1, respLev=c(15, 50, 85))

plot(m2, logx=TRUE, ndose=25, ranef=TRUE) +
  theme_bw()


data(spinach)
spinach$CURVE <- as.factor(spinach$CURVE)

# meta analysis approach
sm1 <- metadrm(SLOPE ~ DOSE,
               data=spinach,
               fct=LL.3(),
               ind=CURVE,
               cid2=HERBICIDE,
               struct="UN")
summary(sm1)

### nlme
sm2 <- medrm(SLOPE ~ DOSE,
             curveid=b + d + e ~ HERBICIDE,
             data=spinach,
             fct=LL.3(),
             random = b + d + e ~ 1|CURVE,
             start=c(0.5, 1, 1.5, 1.5, 1.5, 0.3))
summary(sm2)

cmat <- rbind(c(1, 1),
              c(2, 2),
              c(3, 3))

# comparing effective dose levels for meta analysis
EDcomp(sm1,
       percVec=c(15, 50, 85),
       percMat=cmat,
       interval="fieller")

pdata <- spinach %>%
  group_by(CURVE, HERBICIDE) %>%
  tidyr::expand(DOSE=exp(seq(-5, 5, length=50)))

pdata$SLOPEind <- predict(sm2, newdata=pdata)
pdata$SLOPE <- predict(sm2, newdata=pdata, level=0)

ggplot(spinach, aes(x=log(DOSE), y=SLOPE,
                    colour=HERBICIDE, group=CURVE, shape=HERBICIDE)) +
  geom_point() +
  geom_line(data=pdata) +
  geom_line(data=pdata, aes(y=SLOPEind), linetype=2) +
  theme_bw() +
  scale_x_continuous("DOSE",
                     breaks=log(c(0.01, 0.1, 1, 10, 100)),
                     labels=c(0.01, 0.1, 1, 10, 100))
