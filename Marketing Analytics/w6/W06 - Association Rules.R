install.packages("tidyverse")
install.packages("arules")
install.packages("arulesViz")

library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)


retail <- read_excel('Online Retail.xlsx')
retail <- retail[complete.cases(retail), ]
retail$Description <-  as.factor(retail$Description)
retail$Country <- as.factor(retail$Country)
retail$Date <- as.Date(retail$InvoiceDate)
retail$InvoiceNo <- as.numeric(retail$InvoiceNo)
glimpse(retail)

#HOURLY SHOPPING TRAFFIC
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)
ggplot(aes(x = Time),data = retail) + 
  geom_histogram(stat="count")

#Examine number of unique items per transaction
detach("package:plyr", unload=TRUE)
retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = n()) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  coord_cartesian(xlim=c(0,80))

#Find out top selling items
tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(total = sum(Quantity)) %>% 
  arrange(desc(total))
tmp <- head(tmp, n=10)
tmp
tmp %>% 
  ggplot(aes(x=reorder(Description,total), y=total))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

#GET READY FOR APRIORI ANALYSIS
retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

itemFrequencyPlot(tr, topN=10, type='absolute')

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:10])

topRules <- rules[1:10]
plot(topRules)

plot(topRules, method="graph")
