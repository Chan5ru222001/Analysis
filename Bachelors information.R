#Importing the data

temp=file.choose()
Bachelors_degree_data=read.csv(temp)
View(Bachelors_degree_data)

#Structure of the data
str(Bachelors_degree_data)

#Checking the null values in Data frame
sum(is.na.data.frame(Bachelors_degree_data)=="TRUE")

#Convert the Character va/lues into Integer Values
integer_converter = function(String){
  return(gsub(",","",String))
}

Bachelors_degree_data$Bachelor.s.Degree.Holders = as.integer(integer_converter(Bachelors_degree_data$Bachelor.s.Degree.Holders)) 
Bachelors_degree_data$Science.and.Engineering = as.integer(integer_converter(Bachelors_degree_data$Science.and.Engineering))
Bachelors_degree_data$Science.and.Engineering.Related.Fields = as.integer(integer_converter(Bachelors_degree_data$Science.and.Engineering.Related.Fields))
Bachelors_degree_data$Business = as.integer(integer_converter(Bachelors_degree_data$Business))
Bachelors_degree_data$Education = as.integer(integer_converter(Bachelors_degree_data$Education))
Bachelors_degree_data$Arts..Humanities.and.Others = as.integer(integer_converter(Bachelors_degree_data$Arts..Humanities.and.Others))

#After converting into integer, Check the structure and summary of the Data
str(Bachelors_degree_data)
summary(Bachelors_degree_data)

#Using Subset function remove the unwanted columns and meaningless columns
df=subset(Bachelors_degree_data,Sex!= "Total")
df=subset(df,Age.Group!="25 and older")

#Count the unique States of the data
length(unique(df$State))

#Objective 1: Which gender got the most number of the degree's
#Using histogram find out the most number of Degree holders in Gender wise

library(lattice)
histogram(~Bachelor.s.Degree.Holders|Sex,data=df,breaks=50,main="Distribution of the Degree Holders by Gender")

#Subset the Female Candidates by using dplyr package

library(dplyr)
Female = filter(df,df$Sex=="Female")

#Objective 2: Find out the which age group has the more number of degree holders

library(lattice)
histogram(~Bachelor.s.Degree.Holders|Age.Group,data=Female,breaks=50,main="Distribution of the Degree holder's by Age group")

#Subset the 40 to 64 Age group candidates 
Group_2=filter(Female,Female$Age.Group=="40 to 64")

#plot the density plot for the Branch of studies
par(mfrow=c(3,2)) 
kd1=density(Group_2$Science.and.Engineering)
plot(kd1,col="red",lwd=1)

kd2=density(Group_2$Science.and.Engineering.Related.Fields)
plot(kd2,col="blue",lwd=1)

kd3=density(Group_2$Business)
plot(kd3,col="green",lwd=1)

kd4=density(Group_2$Education)
plot(kd4,col="violet",lwd=1)

kd5=density(Group_2$Arts..Humanities.and.Others)
plot(kd5,col="yellow",lwd=1)

#Subset the Male candidates using dplyr package

Male=filter(df,df$Sex=="Male")

#Objective 3:Find out the which age group has the more number of degree holders in Male

histogram(~Bachelor.s.Degree.Holders|Age.Group,data=Male,breaks=50,main="Distribution of the male Degree holder's by Age group ")

#Subset the 40 to 64 Age group

Group_1=filter(Male,Male$Age.Group=="40 to 64")

#plot the density plot for the Branch of studies

par(mfrow=c(3,2)) 
kd1=density(Group_1$Science.and.Engineering)
plot(kd1,col="red",lwd=1)

kd2=density(Group_1$Science.and.Engineering.Related.Fields)
plot(kd2,col="blue",lwd=1)

kd3=density(Group_1$Business)
plot(kd3,col="green",lwd=1)

kd4=density(Group_1$Education)
plot(kd4,col="violet",lwd=1)

kd5=density(Group_1$Arts..Humanities.and.Others)
plot(kd5,col="yellow",lwd=1)

#Objective 4: Which state has the most of the degree holders

library(plotly)
fig=plot_ly(data=df,x=~State,y=~Bachelor.s.Degree.Holders,type="scatter")%>% layout(title="Sactterplot between State and Degree Holders")
fig

#Subset the California state degree holders
California=filter(df,df$State=="California")

#Objective 5:Which gender has the most no.of Degree holders in California

library(dplyr)
histogram(~Bachelor.s.Degree.Holders|Sex,data=California,breaks=50,main="Distribtion of the California Degree holder's by Gender wise")

#Subset the Female Degree holder's in California State
California_Female=filter(California,California$Sex=="Female")

#Objective 6:Which age group has the most no.of Female Degree holder's 

histogram(~Bachelor.s.Degree.Holders|Age.Group,data=California_Female,breaks=50,main="Distribution of the Female Degree holder's in California by Age group")

#Objective 7:Which Branch study has most no.of Degree holder's in California degree holder's

par(mfrow=c(3,2))
library(plotly)
Course1=plot_ly(data=California,x=~Age.Group,y=~Science.and.Engineering,type="bar",color=~Sex)%>%layout(title="Science.and.Engineering Degree Holder's Age Group wise")
Course1
Course2=plot_ly(data=California,x=~Age.Group,y=~Science.and.Engineering.Related.Fields,type="bar",color=~Sex)%>%layout(title="Science.and.Engineering.Related.Fields Degree holder's Age group wise")
Course2
Course3=plot_ly(data=California,x=~Age.Group,y=~Education,type="bar",color=~Sex)%>%layout(title="Education Degree Holder's Age Group wise")
Course3
Course4=plot_ly(data=California,x=~Age.Group,y=~Business,type="bar",color =~Sex)%>%layout(title="Business Degree Holder's Age Groups wise")
Course4
Course5=plot_ly(data=California,x=~Age.Group,y=~Arts..Humanities.and.Others,type="bar",color=~Sex)%>%layout(title="Arts and Humanities and Others Degree Holder's Age Group wise")
Course5

#Uni-variate Analysis using Histogram 

histogram(~Science.and.Engineering|Sex,data=df,breaks=50,main="Distribution of the Science and Engineering degree holder's by Gender")
histogram(~Science.and.Engineering.Related.Fields|Sex,data=df,breaks=50,main="Distribution of the Science and Engineering related fields by Gender")
histogram(~Business|Sex,data=df,breaks=50,main="Distribution of the Business degree holder's by Gender")
histogram(~Education|Sex,data=df,breaks=50,main="Distribution of the Education degree holder's by Gender")
histogram(~Arts..Humanities.and.Others|Sex,data=df,breaks=50,main="Distribution of the Arts and Humanities degree holder's by Gender")







