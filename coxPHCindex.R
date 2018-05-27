# install required packages, if not installed already
list.of.packages <- c("survival", "survminer","pec", "prodlim", "randomForestSRC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)


# load packages
library("survival")
library("survminer")
library("prodlim")
library("randomForestSRC")
library("pec")

# load lung cancer data
data("lung")
head(lung)

# recode status variable 
lung$status <- lung$status-1

# Delete rows with missing values
lung <- na.omit(lung)

# split data into training and testing
## 80% of the sample size
smp_size <- floor(0.8 * nrow(lung))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(lung)), size = smp_size)

# training and testing data
train.lung <- lung[train_ind, ]
test.lung <- lung[-train_ind, ]

# which covaritaes should we use? 
fit <- coxph(Surv(time, status)~sex+age+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss, data=lung)
summary(fit)
# use covariates with high significance 
# sex, ph.ecog, ph.karno

# compute multivariate cox model
res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno, 
                 data = train.lung)
res.cox
# p-value: if p-value < 0.05, then var is significant
# p-values for all tests are significant => model is significant
summary(res.cox)

# visualization
ggsurvplot(fit = survfit(res.cox, data = train.lung), palette="#2E9FDF", 
             ggtheme = theme_minimal())


# create more cox ph models for comparison
cox.ph2 <- coxph(Surv(time, status)~age+meal.cal+wt.loss, data=train.lung)
cox.ph3 <- coxph(Surv(time, status)~sex+age+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss, data=train.lung)

# create model with 
# Random Forests For Survival, Regression, And Classification (RF-SRC)
forest1 <- rfsrc(Surv(time,status)~age + sex + ph.ecog + ph.karno,data=train.lung,ntree=100,forest=T)
forest2 <- rfsrc(Surv(time,status)~age+meal.cal+wt.loss,data=train.lung,ntree=100,forest=T)
forest3 <- rfsrc(Surv(time,status)~sex+age+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss,data=train.lung,ntree=100,forest=T)



# how good is the model? 
# calculate concordance index
ApparrentCindex  <- pec::cindex(list("cPH ~ age, sex, ph.ecog, ph.karno"=res.cox, 
                                     "cPH ~ age, meal, wt.loss"=cox.ph2, 
                                     "cPH ~ sex, age, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss"=cox.ph3),
                                formula=Surv(time,status)~age + sex + ph.ecog + ph.karno,
                                data=test.lung,
                                eval.times=seq(1,1022,1))

print(ApparrentCindex)
plot(ApparrentCindex)


# how good is the model? 
# calculate concordance index
ApparrentCindex  <- pec::cindex(list("rf ~ age, sex, ph.ecog, ph.karno"=forest1, 
                                     "rf ~ age, meal, wt.loss"=forest2, 
                                     "rf ~ sex, age, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss"=forest3),
                                formula=Surv(time,status)~age + sex + ph.ecog + ph.karno,
                                data=test.lung,
                                eval.times=seq(1,1022,1))

print(ApparrentCindex)
plot(ApparrentCindex)









