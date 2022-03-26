library(dplyr)
library(readxl)
library(caret)
library(rpart)
library(rpart.plot)


dat63_1<-read_excel("D:\\2.64\\Shiny\\63-1.xlsx")
re61_1<-read_excel("D:\\2.64\\Shiny\\report 61-1.xlsx")
re61_2<-read_excel("D:\\2.64\\Shiny\\report 61-2.xlsx")
re61_3<-read_excel("D:\\2.64\\Shiny\\report 62-2.xlsx")
glimpse(dat63_1)
glimpse(re61_1)
glimpse(re61_2)
glimpse(re61_3)

dat1<-dat63_1[c(5,9:15)]
dat2<-re61_1[c(2:9)]
dat3<-re61_2[c(5,9:15)]
dat4<-re61_3[c(2:8,10)]

model1<-rbind(dat1, dat2, dat3, dat4)
summary(model1)
model1[is.na(model1)] = 0
names(model1)[1]<-paste("midtrem")
#Recode
train.dat<-model1%>%mutate(result2=if_else(total>=70,"1","0"))

train.dat$result2<-factor(train.dat$result2, labels = c("fail", "pass"))

#Decision tree 
set.seed(123)
fit.rpart<-rpart(result2~midtrem, data=train.dat[-8],
                 method="class")
rpart.plot(fit.rpart, roundint=F,type=1)

train.dat$result2<-as.factor(train.dat$result2)

##evaluation modle
pred.class<-predict(fit.rpart,newdata=train.dat,type="class")
caret::confusionMatrix(pred.class, train.dat$result2, positive="1")

saveRDS(fit.rpart, "D:\\2.64\\Shiny\\modeltree.RDS")

