# install required packages, if not installed already
list.of.packages <- c("survival", "survminer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)


# load packages
library("survival")
library("survminer")

# load lung cancer data
data("lung")
head(lung)
# meta info abt data: 
# inst: Institution code
# time: Survival time in days
# status: censoring status 1=censored, 2=dead
# age: Age in years
# sex: Male=1 Female=2
# ph.ecog: ECOG performance score (0=good 5=dead)
#          Score indicating the patients living abilities
# ph.karno: Karnofsky performance score (bad=0-good=100) rated by physician
# pat.karno: Karnofsky performance score as rated by patient
# meal.cal: Calories consumed at meals
# wt.loss: Weight loss in last six months

# compute multivariate cox model
res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)
res.cox
# p-value: if p-value < 0.05, then var is significant
# p-values for all tests are significant => model is significant
summary(res.cox)

# visualization
ggsurvplot(fit = survfit(res.cox, data = lung), color="#2E9FDF", 
           ggtheme = theme_minimal())


