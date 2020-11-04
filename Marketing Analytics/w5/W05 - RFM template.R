library(readxl)

RFM_Example_History <- read_excel("W05 - RFM Example History.xlsx")

#Read Transaction Data Set
hist <- RFM_Example_History
cust <- NA

cust <- aggregate(hist$Date, by=list(hist$Customer), max)
cust$RecentDate <- as.Date(cust$x,format="%Y-%m-%d")
cust <- cust[-c(2)]
names(cust)[1]<-"CustID"

temp <- aggregate(hist$Date, by=list(hist$Customer), min) 
cust$FirstDate <- as.Date(temp$x,format="%Y-%m-%d")

present <- as.Date("2014-01-01",format="%Y-%m-%d")
cust$tenure <- (as.numeric(present)-as.numeric(cust$FirstDate))/ 365

temp <- aggregate(hist$Amount, by=list(hist$Customer), sum)
cust$Monetary <- (temp$x /cust$tenure)

temp <- aggregate(hist$Transaction, by=list(hist$Customer), length)
cust$Freq <- (temp$x / cust$tenure)

cust$orderR[order(cust$RecentDate)] <- 1:nrow(cust)
cust$orderM[order(cust$Monetary)] <- 1:nrow(cust)
cust$orderF[order(cust$Freq)] <- 1:nrow(cust)

#repeat for F and M

cust$R <- floor((cust$orderR-1)/1000)+1
cust$M <- floor((cust$orderM-1)/1000)+1
cust$F <- floor((cust$orderF-1)/1000)+1
#repeat for F and M

library(readxl)
RFM_Example_Response <- read_excel("W05 - RFM Example Response(1).xlsx")

resp <- RFM_Example_Response
temp <- aggregate(resp$Customer, by=list(cust$R, cust$M,cust$F), length)



#Read Response Data Set


cust2 <- NA

cust2 <- aggregate(resp$Response, by=list(cust$R, cust$M,cust$F), sum)

names(cust2)[1]<-"R"
names(cust2)[2]<-"F"
names(cust2)[3]<-"M"
#repeat for F and M
names(cust2)[4]<-"Response"

cust2$contacted <- temp$x
cust2$ResponseRate <- with(cust2,Response/contacted)

#order cust2 by RespondRate

