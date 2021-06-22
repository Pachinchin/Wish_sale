#Analysis of whish data set for August 2020
#Data set from Kaggle
#import the data set Summary product of wish data 2020:8 
Summproduct <- read.csv("SumProducts2020.csv")
D <- Summproduct 
#Visualize the data values, 
summary (D)
str(D)
#the data is unstructured and needs cleaning. 
#Remove the unecessary Colums by choosing the one you want
colnames(D)

Df <- D[ ,c("price","retail_price","units_sold","rating","uses_ad_boosts","rating_count",
            "rating_five_count","rating_four_count","rating_three_count","rating_two_count",
            "rating_one_count","merchant_rating_count", "merchant_rating")]
#check the data set    
colnames(Df)
summary (Df)

#look for missing values.
write.csv(Df,"CleanedData.csv", row.names = FALSE)
complete.cases(Df)
complete.cases(Df[1])
na.action(Df)
Df


#data cleaning complete 
#to know the number of missing valuse for each column
apply(is.na(Df),2,sum)
names(Df)
#number of column you want to populate witht he mean 
Df[is.na(Df[,7]), 7] <- mean(Df[,7], na.rm = TRUE)
#check again
apply(is.na(Df),2,sum)
#do for 8,9,10,11 this is for all ratings 
Df[is.na(Df[,8]), 8]   <- mean(Df[,8], na.rm = TRUE)
Df[is.na(Df[,9]), 9]   <- mean(Df[,9], na.rm = TRUE)
Df[is.na(Df[,10]), 10] <- mean(Df[,10], na.rm = TRUE)
Df[is.na(Df[,11]), 11] <- mean(Df[,11], na.rm = TRUE)
apply(is.na(Df),2,sum)
Df


#Next we have to get he proportion of each rating 
#Afunction that will be called when we want to convert the rating data PROPORTION 
dscore<-function(data,tot_rating,rating,N)
      {
        
        A<-(data[,rating])
        B<-(data[,tot_rating])
        
        data$New_RatProp<-A/B
        
        return(data)
      }


#Call for each rating 
Df<-dscore(Df,"rating_count","rating_one_count",1)
#we need to rename immediatly 
#call a rename function dplyr
library(dplyr)
Df<-rename(Df, RatProp1 = New_RatProp)
#Df <-cbind(Df,RatProp1)
Df

Df<-dscore(Df,"rating_count","rating_two_count",2)
Df<-rename(Df, RatProp2 = New_RatProp)
Df
#this can also be used 
#colnames(table)["oldName"] <- "newName"
Df<-dscore(Df,"rating_count","rating_three_count",3)
Df<-rename(Df, RatProp3 = New_RatProp)
Df<-dscore(Df,"rating_count","rating_four_count",4)
Df<-rename(Df, RatProp4 = New_RatProp)
Df<-dscore(Df,"rating_count","rating_five_count",5)
Df<-rename(Df, RatProp5 = New_RatProp)
Df


#check relationships 
plot(RatProp5~units_sold,data=Df)
plot(units_sold~RatProp4,data=Df)
plot(units_sold~RatProp3,data=Df)
plot(units_sold~RatProp2,data=Df)
plot(units_sold~RatProp1,data=Df)

#find the discount rate 
dRate<-function(data,price,selprice)
      {
        
        A<-(data[,price])
        B<-(data[,selprice])
        
        data$Discount<-(B-A)/A
        
        return(data)
      }

Df<-dRate(Df,"retail_price","price")
Df

#find the effect of sellers rating 
dselRate<-function(data,SelRat,Ratingcount)

      {
        
        A<-(data[,SelRat])
        B<-(data[,Ratingcount])
        
        data$SelRatprop<-A/B
        
        return(data)
      }


#Call for each rating 
Df<-dselRate(Df,"merchant_rating","merchant_rating_count")
Df

Ae <- Df[-c(2,6,7:13)]
Ae

plot(units_sold~SelRatprop,data=Df)
plot(units_sold~price,data=Df)
Df


mtcars
BJsales
https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/AirPassengers.html
AirPassengers
C<- mtcars
str(C)
summary(C)
plot(mpg~cyl,data=C)
#There is no linear relationship 
plot(mpg~disp,data=C)
plot(mpg~hp,data=C)
plot
abline(lm(mpg~hp,data=C))
#emeka writ the relationship 
#There is no linear relationship 
plot(mpg~disp,data=C)


m1=lm(mpg~.,data=C)
summary(m1)
names(C)
A=C[c(1:2,4:11)]
A
m2=lm(mpg~.,data=A)
summary(m2)

B=C[c(1:3,5:11)]
m3=lm(cyl~.,data=B)
summary(m2)


X1=C[c(1,3:7)]
X1

y1=C[,2]
y1



out=summary(regsubsets(X1,y1,nbest=2,nvmax=ncol(X)))
#could try also with nbest=6 but it is not necessary
out=summary(regsubsets(X1,y1,nbest=2,nvmax=ncol(X)))
out
tab=cbind(out$which,out$rsq,out$adjr2,out$cp)
tab
## but it is better with column labels:
tab=cbind(out$which,"R-sq"=out$rsq,"R-sq(adj)"=out$adjr2,"Cp"=out$cp)
tab