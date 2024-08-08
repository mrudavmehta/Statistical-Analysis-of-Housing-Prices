install.packages("psych")
install.packages("caTools")
install.packages("car")
install.packages("MASS")


hp=read.csv("Housing Prices.csv") #reading the CSV file


head(hp) #to view the 1st 5 rows
library("psych")
describe(hp) #descriptive stats
summary(hp) #data summarization
attach(hp)


library(caTools)
set.seed(101)
split=sample.split(ï..Price,SplitRatio = 0.70) #train-test split
train_data<-subset(hp,split==T)
test_data<-subset(hp,split==F)


model1 = lm(ï..Price~.,train_data) #building the model
model1
summary(model1)



plot(model1) #4 graphs should appear





library(car)
ncvTest(model1) #NCV Test



vif(model1) #VIF



library(MASS)
model2=stepAIC(model1,direction = 'both') #Stepwise regression


library(car)
vif(model2) #VIF of new model


summary(model2) 


library(car)
ncvTest(model2) #NCV Test of new model (after AIC)


plot(model2) #4 graphs


anova(model2) #ANOVA Table


pred = predict(model2)
res= residuals(model2)
mse = mean(res^2)  #Mean Squared Error
mse
rmse = sqrt(mse) #Root of MSE
rmse
plot(train_data$Y,pred) #graph of training data and its prediction
abline(a=0, b=1)


infIndexPlot(model2, vars=c("Cook", "Studentized", "Bonf", "hat"),id=TRUE, grid=TRUE, main="Diagnostic Plots")


p = predict(model2,test_data)
r = r = abs(test_data$Y-p) 
mse_test = mean(r^2) 
mse_test
rmse_test = sqrt(mean(r^2))
rmse_test
plot(test_data$Y,p)
abline(a=0, b=1)


