
library(ggplot2)
library(reshape)
library(corrplot)

#BARPLOT OF ROCE IN PERCENT

ggplot(Stock_Data,aes(x=Name,y=ROCE_percent))+geom_bar(stat="identity",aes(fill=Type_Of_MarketCap))+labs(title = "BARPLOT OF ROCE IN PERCENT")+theme(axis.title = element_text(size = 16),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))

#BARPLOT OF 5 YEAR PROFIT GROWTH PERCENT

ggplot(Stock_Data,aes(x=Name,y=FiveYearProfitGr_percent))+geom_bar(stat="identity",aes(fill=Type_Of_MarketCap))+labs(title = "BARPLOT OF 5 YEAR PROFIT GROWTH PERCENT")+theme(axis.title = element_text(size = 16),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))

#Correlation Plot

dat<- Stock_Data[,c(4,6:12,22:31)]
corrplot(cor(as.matrix(dat)))

#ScatterPlot between Latest PE and Latest PBV

plot(Stock_Data$Latest_PE,Stock_Data$Latest_PBV,pch=19,col="magenta4",main="ScatterPlot between Latest PE and Latest PBV",xlab="Latest PE",ylab="Latest PBV")

#ScatterPlot between Latest PE and ROCE PERCENT

plot(Stock_Data$Latest_PE,Stock_Data$ROCE_percent,pch=19,col="green3",main="ScatterPlot between Latest PE and ROCE PERCENT",xlab="Latest PE",ylab="ROCE_PERCENT")

#ScatterPlot between Adjusted Net Profit and Return on Equity

plot(Stock_Data$Adjusted_Net_Profit,Stock_Data$Return_On_Equity,pch=19,col="red",main="ScatterPlot between Adjusted Net Profit and Return on Equity",xlab="Adjusted Net Profit",ylab="Return on Equity")


#Boxplot of Sales of 10 years

library(reshape)

s1<-Stock_Data[,c(12:21)]

s<-melt(s1)

ggplot(s,aes(x=variable,y=value))+geom_boxplot(aes(fill=variable))+theme(axis.title = element_text(size = 16),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))


#Pie Chart

a1 <- length(which(Stock_Data$Type_Of_MarketCap=="Large Cap"))

a2 <- length(which(Stock_Data$Type_Of_MarketCap=="Mid Cap"))

a3 <- length(which(Stock_Data$Type_Of_MarketCap=="Small Cap"))

Type  <- c("Large Cap","Mid Cap","Small Cap")

Percent <- 100*c(a1/nrow(Stock_Data),a2/nrow(Stock_Data),a3/nrow(Stock_Data))

d <- data.frame(Type,Percent)

ggplot (d, aes(x="", y=Percent, fill=Type)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y") +
  theme_void() + 
  geom_text(aes( label = round(Percent,2)),position=position_stack(vjust=0.5),size=12, color = "white")+theme(legend.title = element_text(size=20),legend.text=element_text(size=20))+labs(title="PIE CHART OF TYPE OF MARKET CAP")
)
