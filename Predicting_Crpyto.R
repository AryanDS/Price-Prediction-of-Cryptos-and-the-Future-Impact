#SDM Project final code!

#Importing libraries

library(data.table)
library(ggplot2)
library(readr)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(caret)
library(lattice)
library(Metrics)

#Importing correlation data files of the cryptocurrencies
#Update the data file path below.
bit_eth=fread("/bitcoin_eth_correlation_utf8.csv")
head(bit_eth)

bit_doge=fread("/bitcoin_doge_correlation_utf8.csv")
head(bit_doge)

eth_doge=fread("/eth_dogo_correlation_utf8.csv")
head(eth_doge)


#Importing bitcoin, ethereum, dogecoin data files.
#Update the data file path below.

data_bit=fread("/bitcoin_utf8.csv")
head(data_bit)

data_eth=fread("/data_eth_utf8.csv")
head(data_eth)

data_doge = fread("/data_doge_utf8.csv")
head(data_doge)

#Investigating the summary of the data imported
summary(data_bit)

summary(data_eth)

summary(data_doge)

#plotting line graph to analyse the trend
plot(data_bit$Time,data_bit$`BTC / USD Denominated Closing Price`, "l")

plot(data_doge$Time,data_doge$`DOGE / USD Denominated Closing Price`,"l")

plot(data_eth$Time,data_eth$`ETH / USD Denominated Closing Price`,"l")

#we see that there could be a possible correlation between the prices of the cryptocurrencies

summary(bit_eth)

summary(bit_doge)

summary(eth_doge)

#There are empty columns in the data, so we drop them

data_dummy= bit_eth[,c(1,2)]
data_dummy
str(data_dummy)
bit_eth=data_dummy

bit_doge=bit_doge[,c(1,2)]
head(bit_doge)

eth_doge=eth_doge[,c(1,2)]
head(eth_doge)

#Plotting the correlation data between bitcoin and dogecoin 
ggplot(bit_doge, aes(x = bit_doge$Time, y = bit_doge$`Pearson: BTC-DOGE / PriceUSD`,color="#5CB85C")) +
  geom_line()

#We would now create a new data frame and merge all the correlation data frames!


data_corr = cbind(bit_doge, bit_eth$`Pearson: BTC-ETH / PriceUSD`, eth_doge$`Pearson: DOGE-ETH / PriceUSD`)
head(data_corr)

#Changing the column names 
colnames(data_corr)[3]= "bit_eth_cor"
colnames(data_corr)[4]= "doge-eth_cor"
colnames(data_corr)[2]= "bit_doge_cor"
head(data_corr)

#Plotting a correlation plot between all three cryptocurrecies

ggplot(data_corr, aes(x=data_corr$Time))+
  geom_line(aes(y=data_corr$bit_doge_cor), color="#00abff")+
  geom_line(aes(y=data_corr$bit_eth_cor), color="black", linetype='twodash')+
  geom_line(aes(y=data_corr$`doge-eth_cor`), color="red", linetype="dotted")


#This graph helped us to determine the correlation between the crypto currencies!

install.packages("lubridate")
library(lubridate)

#Filtering the data from 2016 on the correlation
data_filtered = filter(data_corr, data_corr$Time >= ymd(20160101))
data_filtered


#Now we need to create a new data frame with the individual cryptocurrencies.

head(data_bit)
head(data_doge)
head(data_eth)

#We would be choosing the starting data point from 1 Jan 2016 
#For Bitcoin
bit_check = data_bit
head(bit_check)

bit_check= filter(bit_check, bit_check$Time >= ymd(20160101))
head(bit_check)

data_bit=bit_check
head(data_bit)

#Ethereum
bit_check=data_eth
head(bit_check)


bit_check= filter(bit_check, bit_check$Time >= ymd(20160101))
head(bit_check)

data_eth=bit_check
head(data_eth)

#Dogecoin
bit_check=data_doge
head(bit_check)

bit_check= filter(bit_check, bit_check$Time >= ymd(20160101))
head(bit_check)

data_doge=bit_check
head(data_doge)


#Importing S&P500 Index fund data
#Update the data file path below.
sp500= fread("/S&P500.csv")
head(sp500)

#Importing AMD stock price data
#Update the data file path below.
amd= fread("/AMD.csv")
head(amd)

#Importing NVDA stock price data
#Update the data file path below.
nvda= fread("/NVDA.csv")
head(nvda)


#Analyzing the data
summary(sp500)

summary(amd)

summary(nvda)

#Creating a new data frame and merging the required data for our analysis


data=cbind(data_bit, data_eth$`ETH / USD Denominated Closing Price`, data_doge$`DOGE / USD Denominated Closing Price`)
head(data)


#Changing column names 
#colnames(data_corr)[3]= "bit_eth_cor"

colnames(data)[2]="Bitcoin"

colnames(data)[3]="Ethereum"

colnames(data)[4]="Dogecoin"

head(data)

#Now we would merge S&P500, NVDA, AMD with the data frame!
data_check=data

data_check= cbind(sp500$`S&P500`, nvda$Close, amd$Close)
head(data_check)


data= cbind(data, data_check)
head(data)

#Changing the column names 
colnames(data)[5]="S&P500"

colnames(data)[6]="NVDA"

colnames(data)[7]="AMD"

head(data)
#Merging the correlation data as well

data= cbind(data, bit_eth$`Pearson: BTC-ETH / PriceUSD`, bit_doge$`Pearson: BTC-DOGE / PriceUSD`, eth_doge$`Pearson: DOGE-ETH / PriceUSD`)
head(data)

data=data[,c(1:7)]
head(data)

#Changing the column names 
colnames(data)[8]="Bit_Eth"

colnames(data)[9]="Bit_Doge"

colnames(data)[10]="Eth_Doge"

head(data)

#Removing extra rows created by R
#creating dummmy variables and copying the data

data_dum_check= data

head(data_dum_check)

#removing extra rows

data_dum_check = data_dum_check[-c(2131:2779)]

head(data_dum_check)

data_dum_check
data = data_dum_check

summary(data)

#Now we would perform EDA on the data!

#Bitcoin vs other independent variables plots!

ggplot(data, aes(x=Bitcoin)) + 
  geom_line(aes(y = `S&P500`), color = "#00abff") + 
  geom_line(aes(y = NVDA), color="orange",linetype ="twodash")+
  geom_line(aes(y= AMD), color="red", linetype="dotted")

#S&P500 vs bitcoin
ggplot(data=data, 
       aes(x=`S&P500`,y=Bitcoin))+
  geom_point(colour='green')+geom_smooth(method="lm")+geom_point(colour='black')

#AMD vs Bitcoin 
ggplot(data=data, 
       aes(x=AMD,y=Bitcoin))+
  geom_point(colour='brown')+geom_smooth(method="lm")

#NVDA vs Bitcoin
ggplot(data=data, 
       aes(x=NVDA,y=Bitcoin))+
  geom_point(colour='red')+geom_smooth(method="lm")


#Ethereum vs other independent variables!


ggplot(data, aes(x=Ethereum)) + 
  geom_line(aes(y = `S&P500`), color = "#00abff") + 
  geom_line(aes(y = NVDA), color="orange",linetype ="twodash")+
  geom_line(aes(y= AMD), color="red", linetype="dotted")


#S&P500 vs Ethereum 
ggplot(data=data, 
       aes(x=`S&P500`,y=Ethereum))+
  geom_point(colour='green')+geom_smooth(method="lm")+geom_point(colour='black')

#AMD vs Ethereum
ggplot(data=data, 
       aes(x=AMD,y=Ethereum))+
  geom_point(colour='brown')+geom_smooth(method="lm")

#NVDA vs Ethereum
ggplot(data=data, 
       aes(x=NVDA,y=Ethereum))+
  geom_point(colour='red')+geom_smooth(method="lm")


#DogeCoin vs other independent variables!

ggplot(data, aes(x=Dogecoin)) + 
  geom_line(aes(y = `S&P500`), color = "#00abff") + 
  geom_line(aes(y = NVDA), color="orange",linetype ="twodash")+
  geom_line(aes(y= AMD), color="red", linetype="dotted")

#S&P500 vs Dogecoin
ggplot(data=data, 
       aes(x=`S&P500`,y=Dogecoin))+
  geom_point(colour='green')+geom_smooth(method="lm")+geom_point(colour='black')

#AMD vs Dogecoin 
ggplot(data=data, 
       aes(x=AMD,y=Dogecoin))+
  geom_point(colour='brown')+geom_smooth(method="lm")

#NVDA vs Dogecoin
ggplot(data=data, 
       aes(x=NVDA,y=Dogecoin))+
  geom_point(colour='red')+geom_smooth(method="lm")


#For forward analyzes we use the ggpairs function from the ggplot library!
#bitcoin vs the independent variables (S&P500, AMD, NVDA)
ggpairs(data=data, columns =c(2,5,6,7))


#Ethereum vs the independent variables (S&P500, AMD, NVDA)
ggpairs(data=data, columns =c(3,5,6,7))

#Dogecoin vs the independent variables (S&P500, AMD, NVDA)
ggpairs(data=data, columns =c(4,5,6,7))


#Analyzing structure of the data

str(data)

#Checking for missing values

summary(data)

p = function(x){
  sum(is.na(x))/length(x)*100
}

apply(data, 2,p)

#There are no missing values in the data.

#We will now move forward and implement ML models on the data with Bitcoin, Ethereum, Dogecoin being the 
#independent variables.

#Multiple Linear Regression 
#Without splitting the data into training and test set!

#Bitcoin
multi_reg_bit = lm(formula = Bitcoin~`S&P500`+NVDA+AMD, data= data)
#Multiple regression equation!
multi_reg_bit
#Summary of the model object!
summary(multi_reg_bit)


#Ethereum
multi_reg_Eth =  lm(formula = Ethereum~`S&P500`+NVDA+AMD, data= data)
#Multiple regression equation!
multi_reg_Eth
#Summary of the model object!
summary(multi_reg_Eth)


#Dogecoin
multi_reg_Dog = lm(formula = Dogecoin~`S&P500`+NVDA+AMD, data= data)
#Multiple regression equation!
multi_reg_Dog
#Summary of the model object!
summary(multi_reg_Dog)


#We didn't get a good r^2 value for Dogecoin.
#We will check whether we can use feature scaling to improve the R^2 value.

#We will add a new column in the data which would have scaled values of Dogecoin, S&P500, NVDA, AMD values as well.

#Creating another data object 
data_doge_FS= data

#Feature Scaling 
#Dogecoin
data_doge_FS$Doge_FS = scale(data_doge_FS$Dogecoin)
head(data_doge_FS)

#S&P500, NVDA, AMD.
data_doge_FS$`S&P500` = scale(data_doge_FS$`S&P500`)

data_doge_FS$NVDA = scale(data_doge_FS$NVDA)

data_doge_FS$AMD = scale(data_doge_FS$AMD)

head(data_doge_FS)

#feature scaling plot 

ggplot(data=data_doge_FS)+
  geom_point(aes(x=Time, y=Dogecoin), color='red')+
  geom_point(aes(x=Time, y=data_doge_FS$Doge_FS), color='Blue')+
  ggtitle("Actual Dogecoin values(Red) vs Predicted Dogecoin values(Blue)")

#We notice that with feature scaling we get better predictions.

#Now that we have scaled the independent and dependent variables.
#We will use Multiple linear regression.

#Dogecoin
multi_reg_Dog = lm(formula = Doge_FS~`S&P500`+NVDA+AMD, data= data_doge_FS)
#Multiple regression equation!
multi_reg_Dog
#Summary of the model object!
summary(multi_reg_Dog)

#We did not notice a change in the R^2 value even after feature scaling. 

#We would be using the original data for further ML algos. "data".

#Duplicating data object.

data_d= data
head(data_d)

#Now, we will predict the Bitcoin,Ethereum,Dogecoin values using the predict function
#We will then compare the predicted values with the actual values using the confusion matrix.

#Bitcoin
data_d$predicted_bit_mlr= predict(multi_reg_bit, newdata= data_d)

#Ethereum
data_d$predicted_eth_mlr= predict(multi_reg_Eth, newdata= data_d)

#Dogecoin
data_d$predicted_doge_mlr= predict(multi_reg_Dog, newdata= data_d)

head(data_d)

#Plotting the actual vs predicted values to understand how the model worked!
#Bitcoin
ggplot(data=data_d)+
  geom_point(aes(x=Time, y=Bitcoin), color='red')+
  geom_point(aes(x=Time, y=data_d$predicted_bit_mlr), color='Blue')+
  ggtitle("Actual Bitcoin values(Red) vs Predicted Bitcoin values(Blue)")

#Ethereum
ggplot(data=data_d)+
  geom_point(aes(x=Time, y=Ethereum), color='red')+
  geom_point(aes(x=Time, y=data_d$predicted_eth_mlr), color='Blue')+
  ggtitle("Actual Ethereum values(Red) vs Predicted Ethereum values(Blue)")

#Dogecoin
ggplot(data=data_d)+
  geom_point(aes(x=Time, y=Dogecoin), color='red')+
  geom_point(aes(x=Time, y=data_d$predicted_doge_mlr), color='Blue')+
  ggtitle("Actual Dogecoin values(Red) vs Predicted Dogecoin values(Blue)")


#In order to evaluate the model for different crpytocurrecies, we would be using Mean Square Error (MSE) 
#Mean Absoute Error(MAE). We would also use the R^2 value for the same.

#Mean Absolute Values
#Bitcoin
mae(data_d$Bitcoin, predict(multi_reg_bit))

#Ethereum
mae(data_d$Ethereum, predict(multi_reg_Eth))

#Dogecoin
mae(data_d$Dogecoin, predict(multi_reg_Dog))


#Mean Square Error
#Bitcoin
mean((data_d$Bitcoin- data_d$predicted_bit_mlr)^2)

#Ethereum
mean((data_d$Ethereum- data_d$predicted_eth_mlr)^2)

#Dogecoin
mean((data_d$Dogecoin- data_d$predicted_doge_mlr)^2)





#We will use other ML techniques now!


#creating new data frame for the predicted values of Boosting, Random Forest.
#data_predicted


#Boosting

#Bitcoin
set.seed(123)
training= createDataPartition(y=data_d$Bitcoin, p=0.75, list=FALSE)

b_train_set_boost=data_d[training,]
b_test_set_boost=data_d[-training,]

data_predicted=b_test_set_boost
head(data_predicted)


head(b_train_set_boost)
head(b_test_set_boost)

#Creating model object
boost_model_bitcoin=train(data=b_train_set_boost, Bitcoin~`S&P500`+NVDA+AMD, method='gbm', verbose=FALSE)
boost_model_bitcoin
boost_model_bitcoin$finalModel
  
#Predicting bitcoin values using the boosting object!

data_predicted$predicted_bit_boost=predict(boost_model_bitcoin, newdata=b_test_set_boost)
head(data_predicted)



#Ethereum
set.seed(123)
training= createDataPartition(y=data_d$Ethereum, p=0.75, list=FALSE)

e_train_set_boost=data[training,]
e_test_set_boost=data[-training,]

head(e_train_set_boost)
head(e_test_set_boost)

head(data_predicted)


#Creating model object
boost_model_ethereum=train(data=e_train_set_boost, Ethereum~`S&P500`+NVDA+AMD, method='gbm', verbose=FALSE)
boost_model_ethereum
boost_model_ethereum$finalModel

#Predicting Ethereum values using boosting object

data_predicted$predicted_ethe_boost=predict(boost_model_ethereum, newdata=e_test_set_boost)
head(data_predicted)


#Dogecoin
set.seed(123)
training= createDataPartition(y=data_d$Dogecoin, p=0.75, list=FALSE)


d_train_set_boost=data[training,]
d_test_set_boost=data[-training,]

head(d_train_set_boost)
head(d_test_set_boost)
head(data_predicted)

#Creating model object
boost_model_dogecoin=train(data=d_train_set_boost, Dogecoin~`S&P500`+NVDA+AMD, method='gbm', verbose=FALSE)
boost_model_dogecoin
boost_model_dogecoin$finalModel

#Predicting Dogecoin values using boosting object

data_predicted$predicted_doge_boost=predict(boost_model_dogecoin, newdata=d_test_set_boost)
head(data_predicted)



#Plotting the actual vs predicted values to understand how the model worked!
#Bitcoin
ggplot(data=data_predicted)+
  geom_point(aes(x=Time, y=Bitcoin), color='red')+
  geom_point(aes(x=Time, y=data_predicted$predicted_bit_boost), color='Blue')+
  ggtitle("Actual Bitcoin values(Red) vs Predicted Bitcoin values(Blue)")

#Ethereum
ggplot(data=data_predicted)+
  geom_point(aes(x=Time, y=Ethereum), color='red')+
  geom_point(aes(x=Time, y=data_predicted$predicted_ethe_boost), color='Blue')+
  ggtitle("Actual Ethereum values(Red) vs Predicted Ethereum values(Blue)")

#Dogecoin
ggplot(data=data_predicted)+
  geom_point(aes(x=Time, y=Dogecoin), color='red')+
  geom_point(aes(x=Time, y=data_predicted$predicted_doge_boost), color='Blue')+
  ggtitle("Actual Dogecoin values(Red) vs Predicted Dogecoin values(Blue)")


#Evaluating how well boosting performed by checking the R^2 value, MAE, MSE of
#the model above.

#Bitcoin
summary(boost_model_bitcoin)
boost_model_bitcoin

#Ethereum
summary(boost_model_ethereum)
boost_model_ethereum

#Dogecoin 
summary(boost_model_dogecoin)
boost_model_dogecoin



#Random Forest 
#Bitcoin
set.seed(123)
training= createDataPartition(y=data_d$Bitcoin, p=0.75, list=FALSE)

b_train_set_RF=data[training,]
b_test_set_RF=data[-training,]

head(b_train_set_RF)
head(b_test_set_RF)
head(data_predicted)

#Model object creation
RF_model_bitcoin= train(data=b_train_set_RF,Bitcoin~`S&P500`+NVDA+AMD, method='rf', prox=TRUE)
RF_model_bitcoin
RF_model_bitcoin$finalModel
summary(RF_model_bitcoin$finalModel)

#Predicting bitcoin values using the boosting object!

data_predicted$predicted_bit_RF=predict(RF_model_bitcoin, newdata=b_test_set_RF)
head(data_predicted)


#Ethereum
set.seed(123)
training= createDataPartition(y=data_d$Ethereum, p=0.75, list=FALSE)

e_train_set_RF=data[training,]
e_test_set_RF=data[-training,]

head(e_train_set_RF)
head(e_test_set_RF)
head(data_predicted)

#Model object creation
RF_model_ethe= train(data=e_train_set_RF,Ethereum~`S&P500`+NVDA+AMD, method='rf', prox=TRUE)
RF_model_ethe
RF_model_ethe$finalModel
summary(RF_model_ethe$finalModel)

#Predicting bitcoin values using the boosting object!

data_predicted$predicted_ethe_RF=predict(RF_model_ethe, newdata=e_test_set_RF)
head(data_predicted)


#Dogecoin
set.seed(123)
training= createDataPartition(y=data_d$Dogecoin, p=0.75, list=FALSE)

d_train_set_RF=data[training,]
d_test_set_RF=data[-training,]

head(d_train_set_RF)
head(d_test_set_RF)
head(data_predicted)

#Model object creation
RF_model_doge= train(data=d_train_set_RF,Dogecoin~`S&P500`+NVDA+AMD, method='rf', prox=TRUE)
RF_model_doge
RF_model_doge$finalModel
summary(RF_model_doge$finalModel)

#Predicting bitcoin values using the boosting object!

data_predicted$predicted_doge_RF=predict(RF_model_doge, newdata=d_test_set_RF)
head(data_predicted)


#Plotting the actual vs predicted values to understand how the model worked!

#Bitcoin
ggplot(data=data_predicted)+
  geom_point(aes(x=Time, y=Bitcoin), color='red')+
  geom_point(aes(x=Time, y=data_predicted$predicted_bit_RF), color='Blue')+
  ggtitle("Actual Dogecoin values(Red) vs Predicted Dogecoin values(Blue)")

#Ethereum
ggplot(data=data_predicted)+
  geom_point(aes(x=Time, y=Ethereum), color='red')+
  geom_point(aes(x=Time, y=data_predicted$predicted_ethe_RF), color='Blue')+
  ggtitle("Actual Dogecoin values(Red) vs Predicted Dogecoin values(Blue)")

#Dogecoin
ggplot(data=data_predicted)+
  geom_point(aes(x=Time, y=Dogecoin), color='red')+
  geom_point(aes(x=Time, y=data_predicted$predicted_doge_RF), color='Blue')+
  ggtitle("Actual Dogecoin values(Red) vs Predicted Dogecoin values(Blue)")

#Evaluating how well boosting performed by checking the R^2 value, MAE, MSE of
#the model above.

#Bitcoin
summary(RF_model_bitcoin)
RF_model_bitcoin

#Ethereum
summary(RF_model_ethe)
RF_model_ethe

#Dogecoin 
summary(RF_model_doge)
RF_model_doge









