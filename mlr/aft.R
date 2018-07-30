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

# telco_churn = read.csv("telco_customer_churn.csv")  # read csv file
telco_churn = read.csv("https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-Telco-Customer-Churn.csv")

telco_churn = na.omit(telco_churn) # remove rows with missing values
colnames(telco_churn)[6] <- "time"
n = nrow(telco_churn)
telco_churn <- telco_churn[sample(n),] # shuffle row-wise
#telco_churn.train = sample(n, size = 0.8*n) 
#telco_churn.test = setdiff(1:n, telco_churn.train)
telco_churn.train_seq = seq(round(n*0.8))
telco_churn.test_seq  = seq(from = round(n*0.8) + 1, to = n)
#telco_churn.train = telco_churn[1:round(telco_churn.train_seq),]
#telco_churn.test = telco_churn[round(n*0.8) + 1 : n,]
telco_churn$Churn <- as.character(telco_churn$Churn)
telco_churn$Churn[telco_churn$Churn == "Yes"] <- T
telco_churn$Churn[telco_churn$Churn == "No"] <- F
telco_churn$Churn <- as.logical(telco_churn$Churn)

telco_churn_cor = telco_churn

# have a look at the correlations between user attributes
#telco_churn_cor = telco_churn[which(telco_churn$Churn == T),]
telco_churn_cor$PhoneService <- as.character(telco_churn_cor$PhoneService)
telco_churn_cor$PhoneService[telco_churn_cor$PhoneService == "Yes"] <- 1
telco_churn_cor$PhoneService[telco_churn_cor$PhoneService == "No"] <- 0
telco_churn_cor$PhoneService <- as.numeric(telco_churn_cor$PhoneService)

telco_churn_cor$MultipleLines <- as.character(telco_churn_cor$MultipleLines)
telco_churn_cor$MultipleLines[telco_churn_cor$MultipleLines == "Yes"] <- 2
telco_churn_cor$MultipleLines[telco_churn_cor$MultipleLines == "No"] <- 1
telco_churn_cor$MultipleLines[telco_churn_cor$MultipleLines == "No phone service"] <- 0
telco_churn_cor$MultipleLines <- as.numeric(telco_churn_cor$MultipleLines)

telco_churn_cor$OnlineSecurity <- as.character(telco_churn_cor$OnlineSecurity)
telco_churn_cor$OnlineSecurity[telco_churn_cor$OnlineSecurity == "Yes"] <- 2
telco_churn_cor$OnlineSecurity[telco_churn_cor$OnlineSecurity == "No"] <- 1
telco_churn_cor$OnlineSecurity[telco_churn_cor$OnlineSecurity == "No internet service"] <- 0
telco_churn_cor$OnlineSecurity <- as.numeric(telco_churn_cor$OnlineSecurity)

telco_churn_cor$OnlineBackup <- as.character(telco_churn_cor$OnlineBackup)
telco_churn_cor$OnlineBackup[telco_churn_cor$OnlineBackup == "Yes"] <- 2
telco_churn_cor$OnlineBackup[telco_churn_cor$OnlineBackup == "No"] <- 1
telco_churn_cor$OnlineBackup[telco_churn_cor$OnlineBackup == "No internet service"] <- 0
telco_churn_cor$OnlineBackup <- as.numeric(telco_churn_cor$OnlineBackup)

telco_churn_cor$DeviceProtection <- as.character(telco_churn_cor$DeviceProtection)
telco_churn_cor$DeviceProtection[telco_churn_cor$DeviceProtection == "Yes"] <- 2
telco_churn_cor$DeviceProtection[telco_churn_cor$DeviceProtection == "No"] <- 1
telco_churn_cor$DeviceProtection[telco_churn_cor$DeviceProtection == "No internet service"] <- 0
telco_churn_cor$DeviceProtection <- as.numeric(telco_churn_cor$DeviceProtection)

telco_churn_cor$StreamingTV <- as.character(telco_churn_cor$StreamingTV)
telco_churn_cor$StreamingTV[telco_churn_cor$StreamingTV == "Yes"] <- 1
telco_churn_cor$StreamingTV[telco_churn_cor$StreamingTV == "No"] <- 0
telco_churn_cor$StreamingTV[telco_churn_cor$StreamingTV == "No internet service"] <- 0
telco_churn_cor$StreamingTV <- as.numeric(telco_churn_cor$StreamingTV)

telco_churn_cor$StreamingMovies <- as.character(telco_churn_cor$StreamingMovies)
telco_churn_cor$StreamingMovies[telco_churn_cor$StreamingMovies == "Yes"] <- 2
telco_churn_cor$StreamingMovies[telco_churn_cor$StreamingMovies == "No"] <- 1
telco_churn_cor$StreamingMovies[telco_churn_cor$StreamingMovies == "No internet service"] <- 0
telco_churn_cor$StreamingMovies <- as.numeric(telco_churn_cor$StreamingMovies)

telco_churn_cor$TechSupport <- as.character(telco_churn_cor$TechSupport)
telco_churn_cor$TechSupport[telco_churn_cor$TechSupport == "Yes"] <- 2
telco_churn_cor$TechSupport[telco_churn_cor$TechSupport == "No"] <- 1
telco_churn_cor$TechSupport[telco_churn_cor$TechSupport == "No internet service"] <- 0
telco_churn_cor$TechSupport <- as.numeric(telco_churn_cor$TechSupport)

telco_churn_cor$InternetService <- as.character(telco_churn_cor$InternetService)
telco_churn_cor$InternetService[telco_churn_cor$InternetService == "Fiber optic"] <- 2
telco_churn_cor$InternetService[telco_churn_cor$InternetService == "DSL"] <- 1
telco_churn_cor$InternetService[telco_churn_cor$InternetService == "No"] <- 0
telco_churn_cor$InternetService <- as.numeric(telco_churn_cor$InternetService)


telco_churn_cor$PaperlessBilling <- as.character(telco_churn_cor$PaperlessBilling)
telco_churn_cor$PaperlessBilling[telco_churn_cor$PaperlessBilling == "Yes"] <- 1
telco_churn_cor$PaperlessBilling[telco_churn_cor$PaperlessBilling == "No"] <- 0
telco_churn_cor$PaperlessBilling <- as.numeric(telco_churn_cor$PaperlessBilling)


telco_churn_cor$Partner <- as.character(telco_churn_cor$Partner)
telco_churn_cor$Partner[telco_churn_cor$Partner == "Yes"] <- 1
telco_churn_cor$Partner[telco_churn_cor$Partner == "No"] <- 0
telco_churn_cor$Partner <- as.numeric(telco_churn_cor$Partner)

telco_churn_cor$Dependents <- as.character(telco_churn_cor$Dependents)
telco_churn_cor$Dependents[telco_churn_cor$Dependents == "Yes"] <- 1
telco_churn_cor$Dependents[telco_churn_cor$Dependents == "No"] <- 0
telco_churn_cor$Dependents <- as.numeric(telco_churn_cor$Dependents)

telco_churn_cor$Contract <- as.character(telco_churn_cor$Contract)
telco_churn_cor$Contract[telco_churn_cor$Contract == "One year"] <- 1
telco_churn_cor$Contract[telco_churn_cor$Contract == "Two year"] <- 2
telco_churn_cor$Contract[telco_churn_cor$Contract == "Month-to-month"] <- 0
telco_churn_cor$Contract <- as.numeric(telco_churn_cor$Contract)

fit <- Surv(telco_churn_cor$time, telco_churn_cor$Churn)

library(flexsurv)
plot(flexsurvreg(fit~1, dist="weibull"), main = "weibull", ylab="S(t)", xlab="t")  # good
plot(flexsurvreg(fit~1, dist="exponential"), main = "exponential", ylab="S(t)", xlab="t") # bad
plot(flexsurvreg(fit~1, dist="lognormal"), main = "lognormal", ylab="S(t)", xlab="t") # ok

plot(flexsurvreg(fit~1, dist="gengamma"), main = "gengamma", ylab="S(t)", xlab="t") # ok
plot(flexsurvreg(fit~1, dist="genf"), main = "genf", ylab="S(t)", xlab="t") # ok
plot(flexsurvreg(fit~1, dist="llogis"), main = "llogis", ylab="S(t)", xlab="t") # ok
plot(flexsurvreg(fit~1, dist="gompertz"), main = "gompertz", ylab="S(t)", xlab="t") # ok




telco_churn_cor$Churn <- as.logical(telco_churn_cor$Churn)

# booth weibull and lognormal distributions seem to fit good 
aftmodelWeibul  <- survreg(Surv(time, Churn) ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, dist = "weibull", data = telco_churn_cor[telco_churn.train_seq,])
predicted <- predict(aftmodelWeibul, newdata = telco_churn_cor[telco_churn.test_seq,])
actual    <- telco_churn_cor[telco_churn.test_seq,]$time
predicted <- as.vector(predicted)
print(myCindex(predicted = predicted, actual = actual)) # 0.7545345 # 0.7604157 # 0.75653 # 0.7495361 # 0.7521168 # 0.7493649 # 0.7608156 # 0.7427811 # 0.7625509 # 0.7467842


aftmodelln   <- survreg(Surv(time, Churn) ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges, dist = "lognormal", data = telco_churn_cor[telco_churn.train_seq,])
predicted <- predict(aftmodelln, newdata = telco_churn_cor[telco_churn.test_seq,])
actual    <- telco_churn_cor[telco_churn.test_seq,]$time
predicted <- as.vector(predicted)
print(myCindex(predicted = predicted, actual = actual)) # 0.7622482 # 0.768333 # 0.7628415 # 0.7572691 # 0.7595177 # 0.7561949 # 0.7687329 # 0.751408 # 0.7698456 # 0.7566343

