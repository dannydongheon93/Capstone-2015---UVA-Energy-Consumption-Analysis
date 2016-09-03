# pre-labelling for supervised learning
building<-read.csv("Buildinginfo_clean.csv")
building$buildingName<-building$BLDG
building2<-read.csv("forClustering.csv")
building3<-read.csv("cleaned+energy+parameters.csv")
building4<-merge(building,building2,by="buildingName")
building5<-merge(building4,building3, by="buildingName")
names(building5)
building6<-building5[,c("buildingName","Correlation_Lag1","Correlation_Lag2","Correlation_Lag3","Correlation_TEMP", "FLOORS", "NET_SF", "PRIMARY_USE","ar1", "ar2","ma1", "mu", "omega", "alpha1", "beta1")]
sapply(building6, class)
building6$NET_SF[which(building6$NET_SF=="NULL")]<-0
building6$NET_SF<-as.character(building6$NET_SF)
building6$NET_SF<-as.numeric(building6$NET_SF)
building6$sfwithfloor<-building6$NET_SF*building6$FLOORS
building6$PRIMARY_USE<-as.character(building6$PRIMARY_USE)
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Hardscape")]<-"Landscape"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Sports Field")]<-"Landscape"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Garden")]<-"Landscape"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Classroom")]<-"Academics"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Research")]<-"Academics"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Library")]<-"Academics"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Dining")]<-"Student Life"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Parking Garage")]<-"Student Life"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Athletic")]<-"Student Life"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Parking Lot")]<-"Student Life"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="None")]<-"Utility"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Office")]<-"Utility"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Support")]<-"Utility"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Public Service")]<-"Utility"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Patient Care")]<-"Utility"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Storage")]<-"Utility"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Utilities")]<-"Utility"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="Child Care")]<-"Utility"
building6$PRIMARY_USE[which(building6$PRIMARY_USE=="TBD")]<-"Utility"
plot(building6$sfwithfloor)
which(building6$sfwithfloor==max(building6$sfwithfloor))

#linear model 
building6$PRIMARY_USE<-as.factor(building6$PRIMARY_USE)
levels(building6$PRIMARY_USE)
building6$PRIMARY_USE<-as.numeric(building6$PRIMARY_USE)
fit<-lm(PRIMARY_USE~Correlation_Lag1+Correlation_Lag2+Correlation_Lag3+Correlation_TEMP+ar1+ar2+ma1+mu+omega+alpha1+beta1+sfwithfloor,data=building6)
summary(fit)
fit<-lm(PRIMARY_USE~Correlation_TEMP+ar1+ma1,data=building6)
building6$predicted<-round(predict(fit, newdata=building6))
length(which(building6$PRIMARY_USE==building6$predicted))


#logistic regression - academics
building6$academics<-0
building6$academics[which(building6$PRIMARY_USE=="Academics")]<-1
x<-1:244
ind<-sample(x, 122, replace=FALSE)
train<-building6[ind,]
test<-building6[-ind,]
fit2<-glm(academics~Correlation_Lag3+Correlation_TEMP+sfwithfloor,data=train, family=binomial)
summary(fit2)
test$predicted<-predict(fit2,newdata=test)
test$rounded<-round(test$predicted)
summary(test$rounded)
test$rounded2<-0
test$rounded2[which(test$rounded>0)]<-1
length(which(test$academics==test$rounded2))/122

#decision tree - academcis
library(rpart)
fit<-rpart(academics~Correlation_Lag3+Correlation_TEMP+sfwithfloor, method="class",data=train)
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit, uniform=TRUE, main="classification for Academics")
text(fit,use.n=TRUE,all=TRUE,cex=.8)
train$academics
test$tree<-predict(fit,newdata=test)
test$tree2<-0
test$tree2[which(test$tree[,1]<0.5)]<-1
length(which(test$academics==test$tree2))/122

#logistic - housing
building6$housing<-0
building6$housing[which(building6$PRIMARY_USE=="Housing")]<-1
train2<-building6[ind,]
test2<-building6[-ind,]
fit3<-glm(housing~Correlation_Lag1+Correlation_TEMP+sfwithfloor+ar2,data=train2, family=binomial)
summary(fit3)
test2$predicted<-predict(fit3,newdata=test2)
test2$rounded<-round(test2$predicted)
summary(test2$rounded)
test2$rounded2<-0
test2$rounded2[which(test2$rounded>0)]<-1
length(which(test2$housing==test2$rounded2))/122

#tree - housing
fit4<-rpart(housing~Correlation_Lag1+Correlation_TEMP+sfwithfloor+ar2, method="class",data=train2)
printcp(fit4)
plotcp(fit4)
summary(fit4)
plot(fit4, uniform=TRUE, main="classification for Housing")
text(fit4,use.n=TRUE,all=TRUE,cex=.8)
test2$tree<-predict(fit4,newdata=test2)
test2$tree2<-0
test2$tree2[which(test2$tree[,1]<0.5)]<-1
length(which(test2$housing==test2$tree2))/122

#logistic - utility
building6$utility<-0
building6$utility[which(building6$PRIMARY_USE=="Utility")]<-1
train3<-building6[ind,]
test3<-building6[-ind,]
fit5<-glm(utility~Correlation_Lag1+Correlation_TEMP+ar2,data=train3, family=binomial)
summary(fit5)
test3$predicted<-predict(fit5,newdata=test3)
test3$rounded<-round(test3$predicted)
summary(test3$rounded)
test3$rounded2<-0
test3$rounded2[which(test3$rounded>0)]<-1
length(which(test3$utility==test3$rounded2))/122

#tree - utility
fit5<-rpart(utility~Correlation_Lag1+Correlation_TEMP+ar2, method="class",data=train3)
printcp(fit5)
plotcp(fit5)
summary(fit5)
plot(fit5, uniform=TRUE, main="classification for Utility")
text(fit5,use.n=TRUE,all=TRUE,cex=.8)
test3$tree<-predict(fit5,newdata=test3)
test3$tree2<-0
test3$tree2[which(test3$tree[,1]<0.5)]<-1
length(which(test3$utility==test3$tree2))/122

#logistic - Student Life
building6$studentlife<-0
building6$studentlife[which(building6$PRIMARY_USE=="Student Life")]<-1
train4<-building6[ind,]
test4<-building6[-ind,]
fit6<-glm(studentlife~Correlation_TEMP+ma1,data=train4, family=binomial)
summary(fit6)
test4$predicted<-predict(fit6,newdata=test4)
test4$rounded<-round(test4$predicted)
summary(test4$rounded)
test4$rounded2<-0
test4$rounded2[which(test4$rounded>0)]<-1
length(which(test4$studentlife==test4$rounded2))/122

#tree - Student Life
fit7<-rpart(studentlife~Correlation_TEMP+ma1, method="class",data=train4)
printcp(fit7)
plotcp(fit7)
summary(fit7)
plot(fit7, uniform=TRUE, main="classification for Student Life")
text(fit7,use.n=TRUE,all=TRUE,cex=.8)
test4$tree<-predict(fit7,newdata=test4)
test4$tree2<-0
test4$tree2[which(test4$tree[,1]<0.5)]<-1
length(which(test4$studentlife==test4$tree2))/122

#k-means clustering
names(building6)
building7<-building6[,c(2,3,4,5,9,10,11,12,13,14,15,16)]
kfit2<-kmeans(building7,4)
aggregate(building7,by=list(kfit2$cluster),FUN=mean)
building7<-data.frame(building7,kfit2$cluster)
building8<-building6[,c("Correlation_TEMP","ar1","ma1")]
kfit3<-kmeans(building8,4)
aggregate(building8,by=list(kfit3$cluster),FUN=mean)
building8<-data.frame(building8,kfit3$cluster)

final_building<-data.frame(building6,building8$kfit3.cluster)
