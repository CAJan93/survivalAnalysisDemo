# import packages
list.of.packages <- c("car", "survival", "flexsurv", "KMsurv", "e1071", "rms", "survAUC", "pec")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

myCindex <- function(predicted, actual) {
  if (length(predicted) != length(actual)) {
    print("length do not match!")
    return()
  }
  sum <- 0
  comparissons <- 0
  for(i in seq(1, length(predicted))) {
    if (i < length(predicted)) {
      for(j in seq(i + 1, length(predicted))) {
        comparissons = comparissons + 1
        if (predicted[i] < predicted[j]) {
          if (actual[i] < actual[j]){
            sum = sum + 1
          }
        }
        if (predicted[i] > predicted[j]) {
          if (actual[i] > actual[j]){
            sum = sum + 1
          }
        }
      }
    }
  }
  return(sum / comparissons)
}

# get data
data("kidtran")
kidtran = na.omit(kidtran) # remove rows with missing values
n = nrow(kidtran)
kidtran <- kidtran[sample(n),] # shuffle row-wise
kidtran.train = kidtran[1:(n * 0.8),]
kidtran.test = kidtran[(n * 0.8):n,]

kidtransurv <- Surv(kidtran.train$time, kidtran.train$delta)


library(flexsurv)
plot(flexsurvreg(kidtransurv~1, dist="exp")) # bad 
plot(flexsurvreg(kidtransurv~1, dist="weibull")) # good
plot(flexsurvreg(kidtransurv~1, dist="gamma")) # good
plot(flexsurvreg(kidtransurv~1, dist="gengamma")) # ok
plot(flexsurvreg(kidtransurv~1, dist="genf")) # bad
plot(flexsurvreg(kidtransurv~1, dist="lnorm")) # ok 
plot(flexsurvreg(kidtransurv~1, dist="gompertz")) # ok

# booth weibull and gamma distributions seem to fit good 
aftmodel  <- survreg(Surv(time, delta) ~ gender + race + age, dist = "weibull", data = kidtran.train)

flexAFT<-flexsurvreg(kidtransurv~kidtran.train$gender+kidtran.train$race+kidtran.train$age, dist = "weibull")
plot(flexAFT, type="survival")

# now predict values with aftmodel
# why does this predict on the old data, not the new data? 
predicted <- predict(aftmodel, newdata = kidtran.test)
# remove all censored data and only use observed time
actual <- kidtran.test[kidtran.test$delta != 0,]$time
predicted <- predicted[kidtran.test$delta != 0]
# remove elements, where predicted is NA
actual <- actual[!is.na(predicted)]
predicted <- predicted[!is.na(predicted)]

print(myCindex(predicted = as.vector(predicted), actual = actual)) # 0.4927536

# uno c-index, does not consider the underlying distribution
predicted <- predict(aftmodel, newdata = kidtran.test)
kidtransurv.test <- Surv(kidtran.test$time, kidtran.test$delta)
# survAUC::UnoC(Surv.rsp = flexAFT, Surv.rsp.new = flexAFT, predicted ) # 0.5286549
survAUC::UnoC(Surv.rsp = kidtransurv, Surv.rsp.new = kidtransurv.test, predicted ) # 0.5286549
