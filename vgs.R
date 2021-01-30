rm(list=ls())
library(ggplot2)


library(corrplot)
setwd("C:\\Users\\mladi\\Desktop\\Data")


# Read in the data
vgs = read.csv("vgs.csv")
str(vgs)


#-----------------------intro------------------------------
#----------------------------------------------------------

#Summary,Structure and type of Variable for name
summary(vgs$Name)
str(vgs$Name)
typeof(vgs$Name)

#Summary,Structure and type of Variable for Platform
summary(vgs$Platform)
str(vgs$Platform)
typeof(vgs$Platform)

#Summary,Structure and type of Variable for Year
summary(vgs$Year_of_Release)
str(vgs$Year_of_Release)
typeof(vgs$Year_of_Release)


#Summary,Structure and type of Variable for Ganre
summary(vgs$Genre)
str(vgs$Genre)
typeof(vgs$Genre)


#Summary,Structure and type of Variable for Publisher
summary(vgs$Publisher)
str(vgs$Publisher)
typeof(vgs$Publisher)


#Summary,Structure and type of Variable for North America Sales
summary(vgs$NA_Sales)
str(vgs$NA_Sales)
typeof(vgs$NA_Sales)

#Summary,Structure and type of Variable for Europa Sales
summary(vgs$EU_Sales)
str(vgs$EU_Sales)
typeof(vgs$EU_Sales)

#Summary,Structure and type of Variable for Japan Sales
summary(vgs$JP_Sales)
str(vgs$JP_Sales)
typeof(vgs$JP_Sales)

#Summary,Structure and type of Variable for Other Sales
summary(vgs$Other_Sales)
str(vgs$Other_Sales)
typeof(vgs$Other_Sales)

#Summary,Structure and type of Variable for Global Sales
summary(vgs$Global_Sales)
str(vgs$Global_Sales)
typeof(vgs$Global_Sales)

#Summary,Structure and type of Variable for Critic Score
summary(vgs$Critic_Score)
str(vgs$Critic_Score)
typeof(vgs$Critic_Score)

#Summary,Structure and type of Variable for Critic Count 
summary(vgs$Critic_Count)
str(vgs$Critic_Count)
typeof(vgs$Critic_Count)


#Summary,Structure and type of Variable for User Score 
summary(vgs$User_Score)
str(vgs$User_Score)
typeof(vgs$User_Score)


#Summary,Structure and type of Variable for User count 
summary(vgs$User_Count)
str(vgs$User_Count)
typeof(vgs$User_Count)

#Summary,Structure and type of Variable for Developers
summary(vgs$Developer)
str(vgs$Developer)
typeof(vgs$Developer)


#Summary,Structure and type of Variable for Rating
summary(vgs$Rating)
str(vgs$Rating)
typeof(vgs$Rating)

#-------------------------Data--------------------------------
#-------------------------------------------------------------


#number of columns that are missing in Critic Score
sum(is.na(vgs$Critic_Score))


#number of columns that are missing Critic Count
sum(is.na(vgs$Critic_Count))

#number of columns that are missing User Score
summary(vgs$User_Score)
sum(vgs$User_Score =="")+ sum(vgs$User_Score=="tbd")

#number of columns that are missing User Count
sum(is.na(vgs$User_Count))

#Removing Missing User Count from data
vgs<-vgs[!is.na(vgs$User_Count),]

#number of columns that are missing Critic Count
sum(is.na(vgs1$Critic_Count))

#Removing Missing Critic Count from data
vgs<-vgs[!is.na(vgs$Critic_Count),]



#Checking missing values
sum(is.na(vgs$Name))
sum(is.na(vgs$Platform))
sum(is.na(vgs$Year_of_Release))
sum(is.na(vgs$Genre))
sum(is.na(vgs$Publisher))
sum(is.na(vgs$NA_Sales))
sum(is.na(vgs$EU_Sales))
sum(is.na(vgs$JP_Sales))
sum(is.na(vgs$Other_Sales))
sum(is.na(vgs$Global_Sales))
sum(is.na(vgs$Critic_Score))
sum(is.na(vgs$Critic_Count))
sum(is.na(vgs$User_Score))
sum(is.na(vgs$User_Count))
sum(is.na(vgs$Developer))
sum(is.na(vgs$Rating))

#Checking missing values in Ratings
sum(vgs$Rating =="")

#Removing the missing values in Ratings
vgs<-vgs[-which(vgs$Rating == ""), ]

#Checking missing values in Ratings
sum(vgs$Year_of_Release == "N/A")

#Removing the missing values in Ratings
vgs<-vgs[-which(vgs$Year_of_Release == "N/A"), ]


#Cheking is there a missing values
sum(is.na(vgs$Name))
summary(vgs$Name)

sum(is.na(vgs$Platform))
summary(vgs$Platform)

sum(is.na(vgs$Year_of_Release))
summary(vgs$Year_of_Release)

sum(is.na(vgs$Genre))
summary(vgs$Genre)

sum(is.na(vgs$Publisher))
summary(vgs$Publisher)

sum(is.na(vgs$NA_Sales))
summary(vgs$NA_Sales)

sum(is.na(vgs$EU_Sales))
summary(vgs$EU_Sales)

sum(is.na(vgs$JP_Sales))
summary(vgs$JP_Sales)

sum(is.na(vgs$Other_Sales))
summary(vgs$Other_Sales)

sum(is.na(vgs$Global_Sales))
summary(vgs$Global_Sales)

sum(is.na(vgs$Critic_Score))
summary(vgs$Critic_Score)

sum(is.na(vgs$Critic_Count))
summary(vgs$Critic_Count)

sum(is.na(vgs$User_Score))
summary(vgs$User_Score)

sum(is.na(vgs$User_Count))
summary(vgs$User_Count)

sum(is.na(vgs$Developer))
summary(vgsFinale$Developer)

sum(is.na(vgs$Rating))
summary(vgs$Rating)





#-------------------Analysis---------------------------
#------------------------------------------------------

#Totaln number of Games Soled
sum(vgs$Global_Sales)



#Games Sold by year
ggplot(vgs, aes(x = Year_of_Release, fill = Year_of_Release)) + 
  geom_bar()+
  scale_y_continuous(breaks=seq(0, 600, 25))+
  xlab("Year") + ylab("Games Represented in millions")+
  ggtitle("Games sold by year")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#--------------------Platforms---------------------------
#--------------------------------------------------------

#Pie chart of all video games Platforms
ggplot(vgs, aes(x="", y="", fill=Platform)) +
  theme_bw()+
  geom_bar(stat="identity", width=1) +
  ggtitle("Video games Platforms")+
  coord_polar("y", start=0)+
  theme(plot.title = element_text(hjust = 0.5,size = 10))


#Total relesad Games on PlayStation's
PlayStation <- length(which(vgs$Platform== "PS"))+length(which(vgs$Platform== "PS2"))+
length(which(vgs$Platform== "PS3"))+length(which(vgs$Platform== "PS4"))+
length(which(vgs$Platform== "PSV")) + length(which(vgs$Platform== "PSP"))


sum(vgs$Global_Sales[which(vgs$Platform== "PS" | vgs$Platform== "PS2" | vgs$Platform== "PS3"|
                         vgs$Platform== "PS4"|vgs$Platform== "PSV"|vgs$Platform== "PSP")])

#Total Reales games on PC
Pc<-length(which(vgs$Platform== "PC"))

sum(vgs$Global_Sales[which(vgs$Platform== "PC")])




#Total Realas games on xbox
Xbox<-length(which(vgs$Platform== "X360"))+length(which(vgs$Platform== "XB"))+length(which(vgs$Platform== "XOne"))

#Total games soled by Xbox
sum(vgs$Global_Sales[which(vgs$Platform== "X360" | vgs$Platform== "XB" | vgs$Platform== "XOne"  )])


#--------------------Ganre---------------------------
#--------------------------------------------------------


#Pie chart of all video gamea Ganre
ggplot(vgs, aes(x="", y="", fill=Genre)) +
  theme_bw()+
  geom_bar(stat="identity", width=1) +
  ggtitle("Video games Ganre")+
  coord_polar("y", start=0)+
  theme(plot.title = element_text(hjust = 0.5,size = 10))

#Number of Games in certen ganre
Action<-sum(vgs$Global_Sales[which(vgs$Genre== "Action" )])
Rpg<-sum(vgs$Global_Sales[which(vgs$Genre== "Role-Playing" )])
Shooter<-sum(vgs$Global_Sales[which(vgs$Genre== "Shooter" )])
sport<-sum(vgs$Global_Sales[which(vgs$Genre== "Sports" )])

Action+Rpg+Shooter+sport




ggplot(data = vgs, aes(x = Genre , y = Global_Sales)) +
  geom_boxplot()+
  ggtitle("Global Sales of video game by Genre")+
  xlab("Genre")+
  scale_y_continuous(name = "Global Sales",
                     limits = c(0, 40),
                     breaks = seq(0, 40, by = 5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#--------------Cor----------------------------






UserScore<- as.numeric(vgs$User_Score)
cor(UserScore, vgs$NA_Sales)
cor(UserScore, vgs$EU_Sales)
cor(UserScore, vgs$JP_Sales)
cor(UserScore, vgs$Other_Sales)
cor(UserScore, vgs$Global_Sales)


cor(vgs)

#change  Type 
vgs$User_Score<-as.numeric(vgs$User_Score)
vgs$User_Count<-as.numeric(vgs$User_Count)
vgs$Critic_Score<-as.numeric(vgs$Critic_Score)
vgs$Critic_Count<-as.numeric(vgs$Critic_Count)
vgs$Year_of_Release<-as.numeric(vgs$Year_of_Release)
vgs$Genre<-as.factor(vgs$Genre)
typeof(vgs$Genre)


#North America Coralation
NA_Cor<-vgs[c(6,11,12,13,14)]
corrplot(corrgram(NA_Cor), method = "number")


#Europa Coralation
EU_Cor<-vgs[c(7,11,12,13,14)]
corrplot(corrgram(EU_Cor), method = "number")

#Japan Coralation
JP_Cor<-vgs[c(8,11,12,13,14)]
corrplot(corrgram(JP_Cor), method = "number")

#Other Coralation
Other_Cor<-vgs[c(9,11,12,13,14)]
corrplot(corrgram(Other_Cor), method = "number")


#--------------Regresion Model----------------------

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets

set.seed(6000)
split = sample.split(vgs$Global_Sales, SplitRatio = 0.70)

# Split up the data using subset
Vgs_train = subset(vgs, split==TRUE)
Vgs_test = subset(vgs, split==FALSE)


#----------North America----------------

#linear model For North America.
NA_Reg1<-lm(NA_Sales ~Year_of_Release
            +Critic_Score+Critic_Count+
              User_Score+User_Count
            , data=Vgs_train)
summary(NA_Reg1)
plot(NA_Reg1)

#sum of errors
SSE = sum(NA_Reg1$residuals^2)
SSE

#NA Sales
NA_Reg2<-lm(NA_Sales ~Genre+Platform+Publisher+Developer , data=Vgs_test)
#only print significant factors
model1<-coef(summary(NA_Reg2))
sig_model_NA<-model1[model1[,"Pr(>|t|)"]<.05,]
printCoefmat(sig_model_NA)


predictTest1 = predict(NA_Reg1, newdata=Vgs_test)
predictTest1
summary(predictTest1)

# Compute out-of-sample R^2
SSE = sum((Vgs_test$NA_Sales - predictTest)^2)
SST = sum((Vgs_test$NA_Sales - mean(Vgs_test$NA_Sale))^2)
R2= 1 - SSE/SST

# Compute the RMSE
RMSE = sqrt(SSE/nrow(Vgs_test))
RMSE
vgs$EU_Sales

#----------EU Model----------------

#linear model For North America.
EU_Reg1<-lm(EU_Sales ~Year_of_Release
            +Critic_Score+Critic_Count+
              User_Score+User_Count
            , data=Vgs_train)
summary(EU_Reg1)


#sum of errors
SSE = sum(EU_Reg1$residuals^2)
SSE

#Eu Sales
EU_Reg2<-lm(EU_Sales ~Genre+Platform+Publisher+Developer , data=Vgs_test)
#only print significant factors
model2<-coef(summary(EU_Reg2))
sig_model_EU<-model2[model2[,"Pr(>|t|)"]<.05,]
printCoefmat(sig_model_EU)


predictTest2 = predict(EU_Reg1, newdata=Vgs_test)
predictTest2
summary(predictTest2)

# Compute out-of-sample R^2
SSE = sum((Vgs_test$EU_Sales - predictTest)^2)
SST = sum((Vgs_test$EU_Sales - mean(Vgs_test$EU_Sales))^2)
R2= 1 - SSE/SST

# Compute the RMSE
RMSE = sqrt(SSE/nrow(Vgs_test))
RMSE


#----------EU Model----------------


vgs$JP_Sales
#linear model For North America.
JP_Reg1<-lm(JP_Sales ~Year_of_Release
            +Critic_Score+Critic_Count+
              User_Score+User_Count
            , data=Vgs_train)
summary(JP_Reg1)


#sum of errors
SSE = sum(JP_Reg1$residuals^2)
SSE

#Eu Sales
JP_Reg2<-lm(JP_Sales ~Genre+Platform+Publisher+Developer , data=Vgs_test)
#only print significant factors
model3<-coef(summary(JP_Reg2))
sig_model_JP<-model3[model3[,"Pr(>|t|)"]<.05,]
printCoefmat(sig_model_JP)


predictTest3 = predict(JP_Reg1, newdata=Vgs_test)
predictTest3
summary(predictTest2)

# Compute out-of-sample R^2
SSE = sum((Vgs_test$JP_Sales - predictTest)^2)
SST = sum((Vgs_test$JP_Sales - mean(Vgs_test$JP_Sales))^2)
R2= 1 - SSE/SST

# Compute the RMSE
RMSE = sqrt(SSE/nrow(Vgs_test))
RMSE



