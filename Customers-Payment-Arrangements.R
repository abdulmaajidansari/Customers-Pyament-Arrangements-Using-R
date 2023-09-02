#####################Libraries######################################
rm(list = ls())
install.packages("readr")

library(rstudioapi)
library(data.table)
library(dplyr)
library(lubridate)
library(readxl)
library(readr)
library(zoo)
library(data.table)
library(openxlsx)
library(stringr)
install.packages("tidyverse")

library(tidyverse)
library(lubridate)
library("zoo")



#Reading The data
Dat1 <- read_csv("C:/Users/HAWK/Desktop/Intrum/CaseStudy Data.csv")

# creating a data set with single payments
singe_payments = subset(Dat1, NumberOfPayments == 0 |NumberOfPayments ==1)
# creating a data set with multiple payments
multiple_payments = subset(Dat1, NumberOfPayments >1)
# creating duplicates in multiple payments according to number of payments
multiple_payments = multiple_payments[rep(seq(nrow(multiple_payments)), multiple_payments$NumberOfPayments-1),]
final_data = rbind(singe_payments,multiple_payments)
final_data <- final_data %>%group_by(CustomerReference) %>% 
  mutate(Date2= ifelse(NumberOfPayments ==0, FirstPaymentDate, ifelse(FrequencyNumber ==1, as.Date(FirstPaymentDate %m+% months(1),origin = "1899-12-30"), FirstPaymentDate+days(7))))
final_data$Date2 <- as.Date(final_data$Date2)
dataset1 <- subset(final_data,NumberOfPayments <= 1)
dataset2 <- subset(final_data,NumberOfPayments >= 2)
dummy <- data.frame()
library(DescTools)
# for loop to create payment schedule 
for (x in unique(dataset2$CustomerReference) ){
  dummy_data <- subset(dataset2,CustomerReference==x)
  dummy_data$Date2 <- as.Date(dummy_data$Date2, format = "%Y-%m-%d")
  if (unique(dummy_data$FrequencyNumber) == 1){
    
    for (i in 1:nrow(dummy_data)){
      if (i+1 <= nrow(dummy_data)){
        dummy_data[i+1,"Date2"] = as.Date(AddMonths(as.double(dummy_data[i,"Date2"]),1) )
        
        }
      
    }
  }else{
   
    for (i in 1:nrow(dummy_data)){
      if (i+1 <= nrow(dummy_data)){
      dummy_data[i+1,"Date2"] = as.Date(as.double(dummy_data[i,"Date2"]))+days(7)
      

    }
    }
  }
  dummy=rbind(dummy, dummy_data)
}

dataframe <- rbind(dummy,dataset1)
# Calculating last payment date and value
dataframe <- dataframe %>% group_by(CustomerReference) %>% mutate(LastPaymentDate  = max(Date2),LastPaymentValue  = max(InstalmentAmount))
write.csv(dataframe,"C:/Users/HAWK/Desktop/Intrum/MonthlyStatements.csv", row.names=FALSE)
