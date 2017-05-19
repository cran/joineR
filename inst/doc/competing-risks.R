## ----epileptic_data------------------------------------------------------
library(joineR)
data(epileptic)
head(epileptic)

## ----interaction_data----------------------------------------------------
epileptic$interaction <- with(epileptic, time * (treat == "LTG"))

## ----jointdata, fig.width=7, fig.height=4--------------------------------
longitudinal <- epileptic[, c(1:3, 13)]
survival <- UniqueVariables(epileptic, c(4, 6), "id")
baseline <- UniqueVariables(epileptic, "treat", "id")
data <- jointdata(longitudinal = longitudinal,
                  survival = survival,
                  baseline = baseline,
                  id.col = "id",
                  time.col = "time")

summary(data)
jointplot(data, Y.col = "dose", Cens.col = "with.status2")

## ----jointmodel, cache=TRUE----------------------------------------------
fit2 <- joint(data = data,
              long.formula = dose ~ time + treat + interaction,
              surv.formula = Surv(with.time, with.status2) ~ treat,
              longsep = FALSE, survsep = FALSE,
              gpt = 3)

summary(fit2)

## ----jointmodel_ses, cache=TRUE, eval=FALSE------------------------------
#  fit2.se <- jointSE(fit2, n.boot = 100)
#  fit2.se

