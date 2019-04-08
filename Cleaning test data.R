library('ggplot2')
library('forecast')
library('tseries')
library(stringr)

setwd("D://Data Analytics/Project 3/Flight_Ticket_Participant_Datasets")
library(readxl)
data = read_excel("Test_set.xlsx")

table(is.na(data))
check_na <- data[rowSums(is.na(data)) > 0,]
check_na

ad<- data[complete.cases(data$Route),]

table(is.na(ad))
check_na <- data[rowSums(is.na(ad)) > 0,]
check_na

str(ad)

library(data.table)



ad$Date_of_Journey = as.Date(ad$Date_of_Journey, "%d/%m/%Y")
(ad$Dep_Time <- as.ITime(ad$Dep_Time))
hours<-sapply(strsplit(ad$Duration," "),function(x) x[1])
mins<-sapply(strsplit(ad$Duration," "),function(x) x[2])
hours<-sapply(strsplit(hours,"h"),function(x) x[1])
mins<-sapply(strsplit(mins,"m"),function(x) x[1])
hours[is.na(hours)]=0
mins[is.na(mins)]=0

hours <- gsub(",","",hours)
hours=as.numeric(hours)
mins <- gsub(",","",mins)
mins=as.numeric(mins)

dur=(hours*60)+mins

ad$dur=dur


adggplot(ad, aes(Date_of_Journey, Price)) + geom_line() + scale_x_date('Date')  + ylab("Price") +
  xlab("")

ad$Day<-weekdays(ad$Date_of_Journey)


ad$Total_Stops<-sapply(strsplit(ad$Total_Stops," "),function(x) x[1])
x = grep("non-stop",ad$Total_Stops)
ad$Total_Stops[x]=0
ad$Total_Stops=as.integer(ad$Total_Stops)

#Giving priority to Time
prompt = 0

t1 = "00:00"
t1 = as.ITime(t1)
t1_h = hour(t1)
t1_m = minute(t1)

t2 = "04:59"
t2 = as.ITime(t2)
t2_h = hour(t2)
t2_m = minute(t2)

t3 = "05:00"
t3 = as.ITime(t3)
t3_h = hour(t3)
t3_m = minute(t3)

t4 = "11:59"
t4 = as.ITime(t4)
t4_h = hour(t4)
t4_m = minute(t4)

t5 = "12:00"
t5 = as.ITime(t5)
t5_h = hour(t5)
t5_m = minute(t5)

t6 = "20:59"
t6 = as.ITime(t6)
t6_h = hour(t6)
t6_m = minute(t6)

t7 = "21:00"
t7 = as.ITime(t7)
t7_h = hour(t7)
t7_m = minute(t7)

t8 = "23:59"
t8 = as.ITime(t8)
t8_h = hour(t8)
t8_m = minute(t8)

ad$hour = hour(ad$Dep_Time)
ad$minute = minute(ad$Dep_Time)

ad$dep_value[ad$hour >= t1_h & ad$hour <= t2_h & ad$minute >= t1_m & ad$minute <= t2_m] = 4
ad$dep_value[ad$hour >= t3_h & ad$hour <= t4_h & ad$minute >= t3_m & ad$minute <= t4_m] = 1
ad$dep_value[ad$hour >= t5_h & ad$hour <= t6_h & ad$minute >= t5_m & ad$minute <= t6_m] = 2
ad$dep_value[ad$hour >= t7_h & ad$hour <= t8_h & ad$minute >= t7_m & ad$minute <= t8_m] = 3

#Giving priority to Date of departure

ad$d = format(ad$Date_of_Journey,"%d")
ad$d = gsub('0','',ad$d)
ad$d = as.numeric(ad$d)
ad$m = month(ad$Date_of_Journey)
ad$m = gsub('0','',ad$m)
ad$m = as.numeric(ad$m)

ad$depdate_value[ad$d >= 1 & ad$d <= 14 & ad$m == 3] = 5
ad$depdate_value[ad$d >= 15 & ad$d <= 31 & ad$m == 3] = 2
ad$depdate_value[ad$m == 4] = 1
ad$depdate_value[ad$m == 5] = 3
ad$depdate_value[ad$m == 6] = 2



#converting dates to factors

library(dummies)
ad<-cbind(ad,dummy(ad$Airline,sep="_"))
ad<-cbind(ad,dummy(ad$Source,sep="_S"))
ad<-cbind(ad,dummy(ad$Destination,sep="_D"))
ad<-cbind(ad,dummy(ad$Additional_Info,sep="_AI"))
ad<-cbind(ad,dummy(ad$Day,sep="_"))

colnames(ad)= names(ad) <- gsub(" ", "_", names(ad))


targetdata = ad[,-c(1,2,3,4,5,6,7,8,10,12,13,14,16,17)]

write.csv(targetdata, "pred.csv", row.names = FALSE)

#univariate analysis


library(moments)
library(diptest)

hist(targetdata$Total_Stops,col="blue")
plot(density(targetdata$Total_Stops),col="blue")
skewness(targetdata$Total_Stops)

hist(targetdata$dur,col="blue")
plot(density(targetdata$dur),col="blue")
skewness(targetdata$dur)

plot(targetdata$Date_of_Journey,targetdata$Price,col=c("green","red"),
     main="Qual Vs. Fees",font=10)

plot(targetdata$Dep_Time,targetdata$Price,col=c("green","red"),
     main="Qual Vs. Fees",font=10)

plot(targetdata$Total_Stops,targetdata$Price,col=c("green","red"),
     main="Qual Vs. Fees",font=10)

plot(targetdata$dur,targetdata$Price,col=c("green","red"),
     main="Qual Vs. Fees",font=10)

colnames(targetdata) = gsub('-','_',colnames(targetdata))

# CREATE TRAIN & TEST DATA
a <- sample(nrow(targetdata),nrow(targetdata)*0.7)
train <- targetdata[a,]
test <- targetdata[-a,]





#LM

ntrain = train[,-c(1,2,5)]

model1 = lm(ntrain$Price ~ .,data = ntrain)
summary(model1)

# CREATE THE MODEL ON TRAIN DATA RANDOM FOREST
library(randomForest)
set.seed(222)
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)

model2 = randomForest(ntrain$Price ~ .,data = ntrain,method="rf", ntree=1000, mtry=29, importance=TRUE, Proximity=TRUE, na.action=na.exclude)
print(model2)

library(caret)
p1=predict(model2, ntrain)



plot(p1, col=c("green","red"))

ntrain$pred=p1
ntrain<- ntrain[complete.cases(ntrain$pred),]

actual <- ntrain$Price
predicted <- ntrain$pred


rss <- sum((predicted - actual) ^ 2)  ## residual sum of squares
tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

R2 <- 1 - (sum((actual-predicted)^2)/sum((actual-mean(actual))^2))

R#FORECAST

library(forecast)

model3=ts(train$Price, frequency=12)
plot.ts(model3)

p1=auto.arima(model3)
summary(p1)

q=forecast(p1, h=20)


