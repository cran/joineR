## ----heart.valve_data---------------------------------------------------------
library(joineR)
data(heart.valve)

## ----heart.valve_dims---------------------------------------------------------
dim(heart.valve)
names(heart.valve)

## ----heart.valve_covars-------------------------------------------------------
heart.valve.cov <- UniqueVariables(
  heart.valve, 
  c("emergenc", "age", "sex"),
  id.col = "num")

## ----heart.valve_covars_samp--------------------------------------------------
heart.valve.cov[11:15, ]

## ----heart.valve_covars_unique------------------------------------------------
heart.valve.cov <- UniqueVariables(
  heart.valve, 
  c(2, 3, 5, 6, 12:25), 
  id.col = "num")

dim(heart.valve.cov)

## ----liver_data---------------------------------------------------------------
data(liver)
dim(liver)
names(liver)

## ----liver_sample-------------------------------------------------------------
liver[liver$id %in% 29:30, ]

## ----mental_data--------------------------------------------------------------
data(mental)

## ----mental_sample------------------------------------------------------------
mental[1:5, ]

## ----mental_dropouts----------------------------------------------------------
table(mental$cens[is.na(mental$Y.t8)])

## ----epileptic_data-----------------------------------------------------------
data(epileptic)
epileptic[1:3, ]

## ----mental_unbalanced--------------------------------------------------------
mental.unbalanced <- to.unbalanced(mental, id.col = 1,
                                   times = c(0, 1, 2, 4, 6, 8),
                                   Y.col = 2:7, other.col = 8:11)
names(mental.unbalanced)
names(mental.unbalanced)[3] <- "Y"

## ----mental_balanced----------------------------------------------------------
mental.balanced <- to.balanced(mental.unbalanced, id.col = 1,
                               time.col = 2,
                               Y.col = 3, other.col = 4:7)
dim(mental.balanced)
names(mental.balanced)

## ----epileptic_balanced-------------------------------------------------------
epileptic.balanced <- to.balanced(epileptic,
                                  id.col = 1, time.col = 3,
                                  Y.col = 2, other.col = 4:12)
dim(epileptic.balanced)
sum(is.na(epileptic.balanced))

## ----mental_summarybal--------------------------------------------------------
summarybal(mental, Y.col = 2:7, times = c(0, 1, 2, 4, 6, 8), 
           na.rm = TRUE)

## ----liver_jm-----------------------------------------------------------------
liver.long <- liver[, 1:3]
liver.surv <- UniqueVariables(liver, var.col = c("survival", "cens"), 
                              id.col = "id")
liver.baseline <- UniqueVariables(liver, var.col = 4,
                                  id.col = "id")

liver.jd <- jointdata(longitudinal = liver.long,
                      survival = liver.surv, 
                      baseline = liver.baseline,
                      id.col = "id",
                      time.col = "time")

## ----heart.valve_jd-----------------------------------------------------------
heart.surv <- UniqueVariables(heart.valve, var.col = c("fuyrs", "status"),
                              id.col = "num")
heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
heart.jd <- jointdata(longitudinal = heart.long, 
                      survival = heart.surv,
                      id.col = "num",
                      time.col = "time")

## ----heart.valve_jd_summary---------------------------------------------------
summary(heart.jd)

## ----heart.valve_subset-------------------------------------------------------
take <- heart.jd$survival$num[heart.jd$survival$status == 0]
heart.jd.cens <- subset(heart.jd, take)

## ----heart.valve_sample-------------------------------------------------------
set.seed(94561)
heart.jd.sample <- sample.jointdata(heart.jd, size = 10)

## ----heart.valve_jointplot, fig.width=7, fig.height=6.5-----------------------
plot(heart.jd)

## ----heart.valve_plot_grad, fig.width=7, fig.height=4.5-----------------------
par(mfrow = c(1, 2))
plot(heart.jd.cens, Y.col = 4, main = "gradient: censored")
take <- heart.jd$survival$num[heart.jd$survival$status == 1]
heart.jd.uncens <- subset(heart.jd, take)
plot(heart.jd.uncens, Y.col = 4, main = "gradient: failed")

## ----heart.valve_plot_lvmi, fig.width=7, fig.height=4.5-----------------------
jointplot(heart.jd, 
          Y.col = "log.lvmi", Cens.col = "status", 
          lag = 5, 
          col1 = "black", col2 = "gray", ylab = "log(lvmi)")

## ----mental_resids------------------------------------------------------------
y <- as.matrix(mental[, 2:7]) 
# converts mental from list format to numeric matrix format
means <- matrix(0, 3, 6)

for (trt in 1:3) {
   ysub <- y[mental$treat == trt, ]
   means[trt,] <- apply(ysub, 2, mean, na.rm = TRUE)
}

residuals <- matrix(0, 150, 6)

for (i in 1:150) {
   residuals[i,] <- y[i,] - means[mental$treat[i], ]
}

V <- cov(residuals, use = "pairwise")
R <- cor(residuals, use = "pairwise")
round(cbind(diag(V), R), 3)

## ----mental_variogram---------------------------------------------------------
vgm <- variogram(indv = mental.unbalanced[, 1], 
                 time = mental.unbalanced[, 2], 
                 Y = mental.unbalanced[, 3])
vgm$sigma2

## ----mental_variogram_plot, fig.width=7, fig.height=4.5-----------------------
par(mfrow = c(1, 3))
plot(vgm$svar[, 1], vgm$svar[, 2], 
     pch = 19, cex = 0.5, 
     xlab = "u", ylab = "V(u)")
plot(vgm, points = FALSE, ylim = c(0, 200)) 
plot(vgm)

## ----mental_jm1---------------------------------------------------------------
mental.long <- mental.unbalanced[, 1:3]
mental.surv <- UniqueVariables(mental.unbalanced, 
                               6:7, id.col = 1)
mental.baseline <- UniqueVariables(mental.unbalanced,
                                   4, id.col = 1)
mental.jd <- jointdata(mental.long, 
                       mental.surv,
                       mental.baseline,
                       id.col = "id", 
                       time.col = "time")

## ----mental_jm2---------------------------------------------------------------
model.jointrandom <- joint(mental.jd, Y ~ 1 + time + treat, 
                           Surv(surv.time, cens.ind) ~ treat,
                           model = "int")
names(model.jointrandom)

## ----mental_jm_summary--------------------------------------------------------
summary(model.jointrandom)

## ----mental_jm_boot-----------------------------------------------------------
model.jointrandom.se <- jointSE(model.jointrandom, n.boot = 100)
model.jointrandom.se

## ----liver_jm1, eval=FALSE----------------------------------------------------
#  model.jointrandom.liver <- joint(
#    liver.jd,
#    prothrombin ~ treatment * time + I(time == 0) * treatment,
#    Surv(survival, cens) ~ treatment,
#    max.it = 1000)

## ----liver_jm2, eval=FALSE----------------------------------------------------
#  model.jointrandom.liver.sep <- joint(
#    liver.jd,
#    prothrombin ~ treatment * time + I(time == 0) * treatment,
#    Surv(survival, cens) ~ treatment,
#    sepassoc = TRUE)

## ----liver_jm_loglik, eval=FALSE----------------------------------------------
#  model.jointrandom.liver.sep$loglik$jointlhood - model.jointrandom.liver$loglik$jointlhood

## ----heart.valve_jointdata----------------------------------------------------
heart.grad <- heart.valve[!is.na(heart.valve$grad), ]
heart.grad.long <- heart.grad[, c(1, 4, 7)]
heart.grad.surv <- UniqueVariables(heart.grad,
                                   var.col = c("fuyrs", "status"),
                                   id.col = "num")
heart.grad.base <- UniqueVariables(heart.grad, 
                                   var.col = 2:3,
                                   id.col = "num")
heart.grad.jd <- jointdata(longitudinal = heart.grad.long,
                           survival = heart.grad.surv,
                           baseline = heart.grad.base,
                           id.col = "num",
                           time.col = "time")

## ----heart.valve_jm-----------------------------------------------------------
model.jointrandom.heart <- joint(heart.grad.jd,
                                 grad ~ age + sex,
                                 Surv(fuyrs, status) ~ age + sex,
                                 model = "int")
summary(model.jointrandom.heart)

