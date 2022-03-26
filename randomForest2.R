library(dplyr)
library(readxl)
library("mice")
library(caret)
library(randomForest)


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

model<-rbind(dat1, dat2, dat3, dat4)
glimpse(model)
summary(model)

model[is.na(model)] = 0
summary(model)

names(model)[1]<-paste("midtrem")
##train.dat Recode
train.dat<-model%>%mutate(result1=if_else(total<55,0,
                                          if_else(total>=55 & total<75,1,
                                                  if_else(total<85,2,3))))
table(train.dat$result1)


train.dat$result1<-factor(train.dat$result1, labels = c("fail", "pass","good", "verygood"))



#RANDOM FOREST
fit.rf1<-randomForest(result1~total, data=train.dat)
fit.rf1
#testing data
pred.rf1<-predict(fit.rf1, train.dat, type = "prob")



saveRDS(fit.rf1, "D:\\2.64\\Shiny\\modelRan3.RDS")
