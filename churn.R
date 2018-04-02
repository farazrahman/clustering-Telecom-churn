library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

Online.Retail <- read.csv("Online Retail.csv", stringsAsFactors = FALSE)
View(Online.Retail)

summary(Online.Retail)
str(Online.Retail)


MV <- Online.Retail %>% summarise_all(funs(sum(is.na(.))/n()))
MV <- gather(MV, key = "variables", value = "percent_missing") 
ggplot(MV, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red")+coord_flip()

#customerID has close to 25% missing values

#Removing NA customerID

order.wise <- na.omit(Online.Retail)

#The RFM analysis
amount <- order.wise$Quantity*order.wise$UnitPrice

order.wise <- cbind(order.wise, amount)

monetary <- order.wise %>% group_by(CustomerID) %>% summarise(monetary = sum(amount))


#order.wise <- order.wise[order(order.wise$CustomerID),]
#monetary <- aggregate(amount~CustomerID, order.wise, sum)

#Next, let's compute the frequency of purchase for each customer, i.e. 
#the F of the RFM framework. For this, you will count the number of unique Invoice Numbers for each Customer ID. 


#frequency <- order.wise %>% group_by(CustomerID ,InvoiceNo) %>% 
# summarise(freq = length(unique(InvoiceNo))) %>% 
#group_by(CustomerID) %>% summarise(freq = sum(freq))


frequency <- order.wise %>% group_by(CustomerID ,InvoiceNo) %>% 
  summarise(freq = length(InvoiceNo)) %>% 
  group_by(CustomerID) %>% summarise(freq = sum(freq))


#frequency <- order.wise[,c(7,1)]
#temp<-table(as.factor(frequency$CustomerID))
#temp<-data.frame(temp)
#colnames(temp)[1]<-c("CustomerID")

#Caluclating the Recency i.e. for how long a customer has not visited the online store

order.wise$InvoiceDate <- as.Date(order.wise$InvoiceDate, "%m/%d/%Y %H:%M")

maximum <- max(order.wise$InvoiceDate)
maximum <- maximum +1

recency <- order.wise %>% group_by(CustomerID, InvoiceDate)
recency$diff <- maximum - recency$InvoiceDate
recency <- recency %>% group_by(CustomerID) %>% summarise(recency = min(diff))



#recency <- order.wise[,c(7,5)]
#recency$InvoiceDate<-as.Date(recency$InvoiceDate,"%m/%d/%Y %H:%M")

#maximum<-max(recency$InvoiceDate)
#maximum<-maximum+1
#maximum$diff <-maximum-recency$InvoiceDate
#recency$diff<-maximum$diff
#recency<-aggregate(recency$diff,by=list(recency$CustomerID),FUN="min")
#colnames(recency)[1]<- "CustomerID"
#colnames(recency)[2]<- "Recency"

#merge recency, monetary and frequency

RFM <- merge(recency, frequency, by = "CustomerID")
RFM <- merge(RFM, monetary,by = "CustomerID" )

str(RFM)
RFM$recency <- as.numeric(RFM$recency)

#Treating outliers
out1 <- boxplot(RFM$recency)$out 
out2 <- boxplot(RFM$freq)$out
out3 <- boxplot(RFM$monetary)$out


RFM1 <- RFM[ !RFM$recency %in% out1, ]
RFM1 <- RFM[ !RFM$freq %in% out2, ]
RFM1 <- RFM[ !RFM$monetary %in% out3, ]

RFMnorm <- RFM1[,-1]

#Standardisation of values

RFMnorm$recency <- scale(RFMnorm$recency)
RFMnorm$freq <- scale(RFMnorm$freq) 
RFMnorm$monetary <- scale(RFMnorm$monetary)


#Creating the clusters

kmclus <- kmeans(RFMnorm, centers = 5, iter.max = 50)

summary(kmclus)

clusters <- kmclus$cluster

RFMnorm_clus <- cbind(RFMnorm, clusters)


