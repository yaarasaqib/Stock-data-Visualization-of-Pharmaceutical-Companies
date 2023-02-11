library(rvest)
library(dplyr)

html<- read_html("https://www.moneyworks4me.com/best-index/bse-stocks/top-pharma-companies-in-india/page/1")
table_15 <- html %>% html_elements(".pr-0.comp-name a ") %>% html_attr("title")
name1<-html%>%html_elements(".pr-0.comp-name a")%>%html_attr("title")
tw<-gsub("\n","",html%>%html_elements(".m-cap.align-self-center")%>%html_text())
tw1<-gsub(",","",gsub("(Large Cap)","",gsub("(Small Cap)","",gsub("(Mid Cap)","",tw))))
tw2<-as.numeric(substr(tw1,1,nchar(tw1)-3))
cap<-html%>%html_elements(".m-cap.align-self-center")%>%html_elements(".font-1rem")%>%html_text()
cmp<-as.numeric(gsub("\n","",html%>%html_elements(".cmp span[itemprop=price]")%>%html_text()))
perce<-gsub("\n","",html%>%html_elements(".cmp")%>%html_text())
perce1<-substr(perce,nchar(perce)-8,nchar(perce)-1)
perce2<-gsub(" ","",perce1)
perce3<-as.numeric(substr(perce2,2,nchar(perce2)-1)[-1])
PE<-html%>%html_elements(".PE")%>%html_text()
PE1<-as.numeric(PE[-1])
PB<-html%>%html_elements(".PB")%>%html_text()
PB1<-as.numeric(PB[-1])
sale<-html%>%html_elements(".sales")%>%html_text()
sale1<-as.numeric(sale[-1])
profit<-html%>%html_elements(".profit")%>%html_text()
profit1<-as.numeric(profit[-1])
url<-paste("https://www.moneyworks4me.com/",html%>%html_elements(".pr-0.comp-name a")%>%html_attr("href"),sep="")
tab<-list(length=30)
ROCE<-numeric(30)
Sales<-numeric(30)
TTMSales<-numeric(30)
BVPS<-numeric(30)
TTMBVPS<-numeric(30)
ANP<-numeric(30)
TTMANP<-numeric(30)
ROE<-numeric(30)
OPM<-numeric(30)
NPM<-numeric(30)
equity<-numeric(30)
ipe<-numeric(30)
for(i in 1:30)
{
  a<-read_html(url[i])
  tab[[i]]<-a%>%html_table()
  
}
for(i in 1:30)
{
  a1<-tab[[i]][[1]][2,]
  b1<-tab[[i]][[1]][6,]
  ROCE[i]<-as.numeric(gsub("%","",substr(a1,1,4)[11]))
  Sales[i]<-as.numeric(gsub(",","",substr(b1,1,6)[11]))
  TTMSales[i]<-as.numeric(gsub(",","",substr(b1,1,6)[12]))
  a<-tab[[i]][[1]][10,]
  BVPS[i]<-as.numeric(substr(a,1,nchar(a))[11])
  TTMBVPS[i]<-as.numeric(substr(a,1,nchar(a))[12])
  h<-tab[[i]][[1]][11,]
  ANP[i]<-as.numeric(gsub(",","",h)[11])
  TTMANP[i]<-as.numeric(gsub(",","",h)[12])
  b<-tab[[i]][[3]][2,]
  ROE[i]<-as.numeric(substr(b,1,nchar(b))[11])
  c<-tab[[i]][[3]][3,]
  OPM[i]<-as.numeric(substr(c,1,nchar(c))[11])
  d<-tab[[i]][[3]][4,]
  NPM[i]<-as.numeric(substr(d,1,nchar(d))[11])
  equity[i]<-as.numeric(tab[[i]][[5]][4,][2])
  ipe[i]<-as.numeric(tab[[i]][[5]][6,][2])
}
#######
p1 <- data.frame(name1,tw2,cap,cmp,perce3,PE1,PB1,sale1,profit1,ROCE,Sales,TTMSales,BVPS,TTMBVPS,ANP,TTMANP,ROE,OPM,NPM,equity,ipe)

PE1_n <- replace(PE1 ,which( PE1 == " -") ,0 )
BVPS_n <- replace(BVPS ,which(is.na(BVPS) == T) ,0)
TTMBVPS_n <- replace(TTMBVPS , which(is.na(TTMBVPS) == T) ,0)

p1_new <- data.frame(name1,tw2,cap,cmp,perce3,PE1_n,PB1,sale1,profit1,ROCE,Sales,TTMSales,BVPS_n,TTMBVPS_n,ANP,TTMANP,ROE,OPM,NPM,equity,ipe)

save( p1_new , file = "page1.Rdata")
######
# pg2
library(rvest)
html<-read_html("https://www.moneyworks4me.com/best-index/bse-stocks/top-pharma-companies-in-india/page/2")
table_15 <- html %>% html_elements(".pr-0.comp-name a ") %>% html_attr("title")
name1<-html%>%html_elements(".pr-0.comp-name a")%>%html_attr("title")
tw<-gsub("\n","",html%>%html_elements(".m-cap.align-self-center")%>%html_text())
tw1<-gsub(",","",gsub("(Large Cap)","",gsub("(Small Cap)","",gsub("(Mid Cap)","",tw))))
tw2<-as.numeric(substr(tw1,1,nchar(tw1)-3))
cap<-html%>%html_elements(".m-cap.align-self-center")%>%html_elements(".font-1rem")%>%html_text()
cmp<-as.numeric(gsub("\n","",html%>%html_elements(".cmp span[itemprop=price]")%>%html_text()))
perce<-gsub("\n","",html%>%html_elements(".cmp")%>%html_text())
perce1<-substr(perce,nchar(perce)-8,nchar(perce)-1)
perce2<-gsub(" ","",perce1)
perce3<-as.numeric(substr(perce2,2,nchar(perce2)-1)[-1])
PE<-html%>%html_elements(".PE")%>%html_text()
PE1<-as.numeric(PE[-1])
PB<-html%>%html_elements(".PB")%>%html_text()
PB1<-as.numeric(PB[-1])
sale<-html%>%html_elements(".sales")%>%html_text()
sale1<-as.numeric(sale[-1])
profit<-html%>%html_elements(".profit")%>%html_text()
profit1<-as.numeric(profit[-1])
url<-paste("https://www.moneyworks4me.com/",html%>%html_elements(".pr-0.comp-name a")%>%html_attr("href"),sep="")
tab<-list(length=30)
ROCE<-numeric(30)
Sales<-numeric(30)
TTMSales<-numeric(30)
BVPS<-numeric(30)
TTMBVPS<-numeric(30)
ANP<-numeric(30)
TTMANP<-numeric(30)
ROE<-numeric(30)
OPM<-numeric(30)
NPM<-numeric(30)
equity<-numeric(30)
ipe<-numeric(30)
for(i in 1:30)
{
  a<-read_html(url[i])
  tab[[i]]<-a%>%html_table()
  
}
for(i in 1:30)
{
  a1<-tab[[i]][[1]][2,]
  b1<-tab[[i]][[1]][6,]
  ROCE[i]<-as.numeric(gsub("%","",substr(a1,1,4)[11]))
  Sales[i]<-as.numeric(gsub(",","",substr(b1,1,6)[11]))
  TTMSales[i]<-as.numeric(gsub(",","",substr(b1,1,6)[12]))
  a<-tab[[i]][[1]][10,]
  BVPS[i]<-as.numeric(substr(a,1,nchar(a))[11])
  TTMBVPS[i]<-as.numeric(substr(a,1,nchar(a))[12])
  h<-tab[[i]][[1]][11,]
  ANP[i]<-as.numeric(gsub(",","",h)[11])
  TTMANP[i]<-as.numeric(gsub(",","",h)[12])
  b<-tab[[i]][[3]][2,]
  ROE[i]<-as.numeric(substr(b,1,nchar(b))[11])
  c<-tab[[i]][[3]][3,]
  OPM[i]<-as.numeric(substr(c,1,nchar(c))[11])
  d<-tab[[i]][[3]][4,]
  NPM[i]<-as.numeric(substr(d,1,nchar(d))[11])
  equity[i]<-as.numeric(tab[[i]][[5]][4,][2])
  ipe[i]<-as.numeric(tab[[i]][[5]][6,][2])
}
######
p2 <- data.frame(name1,tw2,cap,cmp,perce3,PE1,PB1,sale1,profit1,ROCE,Sales,TTMSales,BVPS,TTMBVPS,ANP,TTMANP,ROE,OPM,NPM,equity,ipe)

PE1_n <- replace(PE1 ,which( PE1 == " -") ,0 )

p2_new <- data.frame(name1,tw2,cap,cmp,perce3,PE1_n,PB1,sale1,profit1,ROCE,Sales,TTMSales,BVPS,TTMBVPS,ANP,TTMANP,ROE,OPM,NPM,equity,ipe)

save( p2_new , file = "page2.Rdata")
######




# pg3
library(rvest)
html<-read_html("https://www.moneyworks4me.com/best-index/bse-stocks/top-pharma-companies-in-india/page/3")
table_15 <- html %>% html_elements(".pr-0.comp-name a ") %>% html_attr("title")
name1<-html%>%html_elements(".pr-0.comp-name a")%>%html_attr("title")
tw<-gsub("\n","",html%>%html_elements(".m-cap.align-self-center")%>%html_text())
tw1<-gsub(",","",gsub("(Large Cap)","",gsub("(Small Cap)","",gsub("(Mid Cap)","",tw))))
tw2<-as.numeric(substr(tw1,1,nchar(tw1)-3))
cap<-html%>%html_elements(".m-cap.align-self-center")%>%html_elements(".font-1rem")%>%html_text()
cmp<-as.numeric(gsub("\n","",html%>%html_elements(".cmp span[itemprop=price]")%>%html_text()))
perce<-gsub("\n","",html%>%html_elements(".cmp")%>%html_text())
perce1<-substr(perce,nchar(perce)-8,nchar(perce)-1)
perce2<-gsub(" ","",perce1)
perce3<-as.numeric(substr(perce2,2,nchar(perce2)-1)[-1])
PE<-html%>%html_elements(".PE")%>%html_text()
PE1<-as.numeric(PE[-1])
PB<-html%>%html_elements(".PB")%>%html_text()
PB1<-as.numeric(PB[-1])
sale<-html%>%html_elements(".sales")%>%html_text()
sale1<-as.numeric(sale[-1])
profit<-html%>%html_elements(".profit")%>%html_text()
profit1<-as.numeric(profit[-1])
url<-paste("https://www.moneyworks4me.com/",html%>%html_elements(".pr-0.comp-name a")%>%html_attr("href"),sep="")
tab<-list(length=30)
ROCE<-numeric(30)
Sales<-numeric(30)
TTMSales<-numeric(30)
BVPS<-numeric(30)
TTMBVPS<-numeric(30)
ANP<-numeric(30)
TTMANP<-numeric(30)
ROE<-numeric(30)
OPM<-numeric(30)
NPM<-numeric(30)
equity<-numeric(30)
ipe<-numeric(30)
for(i in 1:30)
{
  a<-read_html(url[i])
  tab[[i]]<-a%>%html_table()
  
}
for(i in 1:30)
{
  a1<-tab[[i]][[1]][2,]
  b1<-tab[[i]][[1]][6,]
  ROCE[i]<-as.numeric(gsub("%","",substr(a1,1,4)[11]))
  Sales[i]<-as.numeric(gsub(",","",substr(b1,1,6)[11]))
  TTMSales[i]<-as.numeric(gsub(",","",substr(b1,1,6)[12]))
  a<-tab[[i]][[1]][10,]
  BVPS[i]<-as.numeric(substr(a,1,nchar(a))[11])
  TTMBVPS[i]<-as.numeric(substr(a,1,nchar(a))[12])
  h<-tab[[i]][[1]][11,]
  ANP[i]<-as.numeric(gsub(",","",h)[11])
  TTMANP[i]<-as.numeric(gsub(",","",h)[12])
  b<-tab[[i]][[3]][2,]
  ROE[i]<-as.numeric(substr(b,1,nchar(b))[11])
  c<-tab[[i]][[3]][3,]
  OPM[i]<-as.numeric(substr(c,1,nchar(c))[11])
  d<-tab[[i]][[3]][4,]
  NPM[i]<-as.numeric(substr(d,1,nchar(d))[11])
  equity[i]<-as.numeric(tab[[i]][[5]][4,][2])
  ipe[i]<-as.numeric(tab[[i]][[5]][6,][2])
}
####
p3 <- data.frame(name1,tw2,cap,cmp,perce3,PE1,PB1,sale1,profit1,ROCE,Sales,TTMSales,BVPS,TTMBVPS,ANP,TTMANP,ROE,OPM,NPM,equity,ipe)

PE1_n <- replace(PE1 ,which( PE1 == " -") ,0 )
PB1_n <- replace(PB1 ,which(PB1 == " -") ,0 )
ROCE_n <- replace(ROCE , which(is.na(ROCE) == T) ,0)
Sales_n <- replace(Sales ,which(is.na(Sales) == T) ,0)
BVPS_n <- replace(BVPS ,which(is.na(BVPS) == T) ,0)
TTMBVPS_n <- replace(TTMBVPS , which(is.na(TTMBVPS) == T) ,0)
TTMSales_n <- replace(TTMSales , which(is.na(TTMSales) == T), 0)
ANP_n <- replace(ANP , which(is.na(ANP) == T) ,0)
TTMANP_n <- replace(TTMANP , which(is.na(TTMANP) == T) ,0)
ROE_n <- replace(ROE ,  which(is.na(ROE) == T) , 0)
OPM_n <- replace(OPM , which(is.na(OPM) == T) , 0)
NPM_n <- replace(NPM , which(is.na(NPM) == T) ,0 )

p3_new <- data.frame(name1,tw2,cap,cmp,perce3,PE1_n,PB1_n,sale1,profit1,ROCE_n,Sales_n,TTMSales_n,BVPS_n,TTMBVPS_n,ANP_n,TTMANP_n,ROE_n,OPM_n,NPM_n,equity,ipe)

save( p3_new , file = "page3.Rdata")
######














# pg4
library(rvest)
html<-read_html("https://www.moneyworks4me.com/best-index/bse-stocks/top-pharma-companies-in-india/page/4")
table_15 <- html %>% html_elements(".pr-0.comp-name a ") %>% html_attr("title")
name1<-html%>%html_elements(".pr-0.comp-name a")%>%html_attr("title")
tw<-gsub("\n","",html%>%html_elements(".m-cap.align-self-center")%>%html_text())
tw1<-gsub(",","",gsub("(Large Cap)","",gsub("(Small Cap)","",gsub("(Mid Cap)","",tw))))
tw2<-as.numeric(substr(tw1,1,nchar(tw1)-3))
cap<-html%>%html_elements(".m-cap.align-self-center")%>%html_elements(".font-1rem")%>%html_text()
cmp<-as.numeric(gsub("\n","",html%>%html_elements(".cmp span[itemprop=price]")%>%html_text()))
perce<-gsub("\n","",html%>%html_elements(".cmp")%>%html_text())
perce1<-substr(perce,nchar(perce)-8,nchar(perce)-1)
perce2<-gsub(" ","",perce1)
perce3<-as.numeric(substr(perce2,2,nchar(perce2)-1)[-1])
PE<-html%>%html_elements(".PE")%>%html_text()
PE1<-as.numeric(PE[-1])
PB<-html%>%html_elements(".PB")%>%html_text()
PB1<-as.numeric(PB[-1])
sale<-html%>%html_elements(".sales")%>%html_text()
sale1<-as.numeric(sale[-1])
profit<-html%>%html_elements(".profit")%>%html_text()
profit1<-as.numeric(profit[-1])
url<-paste("https://www.moneyworks4me.com/",html%>%html_elements(".pr-0.comp-name a")%>%html_attr("href"),sep="")
tab<-list(length=5)
ROCE<-numeric(5)
Sales<-numeric(5)
TTMSales<-numeric(5)
BVPS<-numeric(5)
TTMBVPS<-numeric(5)
ANP<-numeric(5)
TTMANP<-numeric(5)
ROE<-numeric(5)
OPM<-numeric(5)
NPM<-numeric(5)
equity<-numeric(5)
ipe<-numeric(5)
for(i in 1:5)
{
  a<-read_html(url[i])
  tab[[i]]<-a%>%html_table()
  
}
for(i in 1:5)
{
  a1<-tab[[i]][[1]][2,]
  b1<-tab[[i]][[1]][6,]
  ROCE[i]<-as.numeric(gsub("%","",substr(a1,1,4)[11]))
  Sales[i]<-as.numeric(gsub(",","",substr(b1,1,6)[11]))
  TTMSales[i]<-as.numeric(gsub(",","",substr(b1,1,6)[12]))
  a<-tab[[i]][[1]][10,]
  BVPS[i]<-as.numeric(substr(a,1,nchar(a))[11])
  TTMBVPS[i]<-as.numeric(substr(a,1,nchar(a))[12])
  h<-tab[[i]][[1]][11,]
  ANP[i]<-as.numeric(gsub(",","",h)[11])
  TTMANP[i]<-as.numeric(gsub(",","",h)[12])
  b<-tab[[i]][[3]][2,]
  ROE[i]<-as.numeric(substr(b,1,nchar(b))[11])
  c<-tab[[i]][[3]][3,]
  OPM[i]<-as.numeric(substr(c,1,nchar(c))[11])
  d<-tab[[i]][[3]][4,]
  NPM[i]<-as.numeric(substr(d,1,nchar(d))[11])
  equity[i]<-as.numeric(tab[[i]][[5]][4,][2])
  ipe[i]<-as.numeric(tab[[i]][[5]][6,][2])
}
#######
p4 <- data.frame(name1,tw2,cap,cmp,perce3,PE1,PB1,sale1,profit1,ROCE,Sales,TTMSales,BVPS,TTMBVPS,ANP,TTMANP,ROE,OPM,NPM,equity,ipe)

PE1_n <- replace(PE1 ,which( PE1 == " -") ,0 )

p4_new <- data.frame(name1,tw2,cap,cmp,perce3,PE1_n,PB1,sale1,profit1,ROCE,Sales,TTMSales,BVPS,TTMBVPS,ANP,TTMANP,ROE,OPM,NPM,equity,ipe)


save( p4_new , file = "page4.Rdata")



page1 <- load("/Users/debarghyajana/Documents/IITK 1st Semester,2022/page1.rdata")
page2 <- load("/Users/debarghyajana/Documents/IITK 1st Semester,2022/page2.rdata")
page3 <- load("/Users/debarghyajana/Documents/IITK 1st Semester,2022/page3.rdata")
page4 <- load("/Users/debarghyajana/Documents/IITK 1st Semester,2022/page4 (1).rdata")

name_all <- c(p1_new$name1 , p2_new$name1 , p3_new$name1 , p4_new$name1 )
tw2_all <-  c(p1_new$tw2 , p2_new$tw2 , p3_new$tw2 , p4_new$tw2 )
cap_all <-  c(p1_new$cap , p2_new$cap , p3_new$cap , p4_new$cap )
cmp_all <-  c(p1_new$cmp , p2_new$cmp , p3_new$cmp , p4_new$cmp )
perce3_all <-  c(p1_new$perce3 , p2_new$perce3 , p3_new$perce3 , p4_new$perce3 )
PE1_all <-  c(p1_new$PE1 , p2_new$PE1 , p3_new$PE1_n , p4_new$PE1_n )
PB1_all <-  c(p1_new$PB1 , p2_new$PB1 , p3_new$PB1_n , p4_new$PB1 )
sale1_all <-  c(p1_new$sale1 , p2_new$sale1 , p3_new$sale1 , p4_new$sale1 )
profit1_all <-  c(p1_new$profit1 , p2_new$profit1 , p3_new$profit1 , p4_new$profit1 )
ROCE_all <-  c(p1_new$ROCE , p2_new$ROCE , p3_new$ROCE_n , p4_new$ROCE )
Sales_all <-  c(p1_new$Sales , p2_new$Sales , p3_new$Sales_n , p4_new$Sales )
TTMSales_all <-  c(p1_new$TTMSales , p2_new$TTMSales , p3_new$TTMSales_n , p4_new$TTMSales )
BVPS_all <-  c(p1_new$BVPS_n , p2_new$BVPS , p3_new$BVPS_n , p4_new$BVPS )
TTMBVPS_all <-  c(p1_new$TTMBVPS_n , p2_new$TTMBVPS , p3_new$TTMBVPS_n, p4_new$TTMBVPS )
ANP_all <-  c(p1_new$ANP , p2_new$ANP, p3_new$ANP_n, p4_new$ANP )
TTMANP_all <-  c(p1_new$TTMANP , p2_new$TTMANP, p3_new$TTMANP_n, p4_new$TTMANP )
ROE_all <-  c(p1_new$ROE , p2_new$ROE , p3_new$ROE_n , p4_new$ROE )
OPM_all <-  c(p1_new$OPM , p2_new$OPM , p3_new$OPM_n , p4_new$OPM )
NPM_all <-  c(p1_new$NPM , p2_new$NPM , p3_new$NPM_n , p4_new$NPM )
equity_all <-  c(p1_new$equity , p2_new$equity , p3_new$equity , p4_new$equity )
ipe_all <-  c(p1_new$ipe , p2_new$ipe , p3_new$ipe , p4_new$ipe )

project_main_data <- data.frame(name_all,tw2_all,cap_all,cmp_all,perce3_all,PE1_all,PB1_all,sale1_all,profit1_all,ROCE_all,Sales_all,TTMSales_all,BVPS_all,TTMBVPS_all,ANP_all,TTMANP_all,ROE_all,OPM_all,NPM_all,equity_all,ipe_all)

save(project_main_data , file = "main_data.Rdata")


########


library(rvest)
html<-read_html("https://www.moneyworks4me.com/best-index/bse-stocks/top-pharma-companies-in-india/page/1")
url<-paste("https://www.moneyworks4me.com/",html%>%html_elements(".pr-0.comp-name a")%>%html_attr("href"),sep="")
n=30
tab<-list(length=n)
for(i in 1:n)
{
  a<-read_html(url[i])
  tab[[i]]<-a%>%html_table()
  
}
m<-matrix(0,nrow=n,ncol=10)
colnames(m)<-c("Sales_Mar'13","Sales_Mar'14","Sales_Mar'15","Sales_Mar'16","Sales_Mar'17","Sales_Mar'18","Sales_Mar'19","Sales_Mar'20","Sales_Mar'21","Sales_Mar'22")
for(i in 1:n)
{
  a1<-tab[[i]][[1]][6,]
  a11<-(as.numeric(gsub(",","",a1)))[2:11]
  m[i,]<-a11
}



html<-read_html("https://www.moneyworks4me.com/best-index/bse-stocks/top-pharma-companies-in-india/page/2")
url<-paste("https://www.moneyworks4me.com/",html%>%html_elements(".pr-0.comp-name a")%>%html_attr("href"),sep="")
n=30
tab<-list(length=n)
for(i in 1:n)
{
  a<-read_html(url[i])
  tab[[i]]<-a%>%html_table()
  
}
m1<-matrix(0,nrow=n,ncol=10)
colnames(m)<-c("Sales_Mar'13","Sales_Mar'14","Sales_Mar'15","Sales_Mar'16","Sales_Mar'17","Sales_Mar'18","Sales_Mar'19","Sales_Mar'20","Sales_Mar'21","Sales_Mar'22")
for(i in 1:n)
{
  a1<-tab[[i]][[1]][6,]
  a11<-(as.numeric(gsub(",","",a1)))[2:11]
  m1[i,]<-a11
}


html<-read_html("https://www.moneyworks4me.com/best-index/bse-stocks/top-pharma-companies-in-india/page/3")
url<-paste("https://www.moneyworks4me.com/",html%>%html_elements(".pr-0.comp-name a")%>%html_attr("href"),sep="")
n=30
tab<-list(length=n)
for(i in 1:n)
{
  a<-read_html(url[i])
  tab[[i]]<-a%>%html_table()
  
}
m2<-matrix(0,nrow=n,ncol=10)
colnames(m)<-c("Sales_Mar'13","Sales_Mar'14","Sales_Mar'15","Sales_Mar'16","Sales_Mar'17","Sales_Mar'18","Sales_Mar'19","Sales_Mar'20","Sales_Mar'21","Sales_Mar'22")
for(i in 1:n)
{
  a1<-tab[[i]][[1]][6,]
  a11<-(as.numeric(gsub(",","",a1)))[2:11]
  m2[i,]<-a11
}

html<-read_html("https://www.moneyworks4me.com/best-index/bse-stocks/top-pharma-companies-in-india/page/4")
url<-paste("https://www.moneyworks4me.com/",html%>%html_elements(".pr-0.comp-name a")%>%html_attr("href"),sep="")
n=5
tab<-list(length=n)
for(i in 1:n)
{
  a<-read_html(url[i])
  tab[[i]]<-a%>%html_table()
  
}
m3<-matrix(0,nrow=n,ncol=10)
colnames(m)<-c("Sales_Mar'13","Sales_Mar'14","Sales_Mar'15","Sales_Mar'16","Sales_Mar'17","Sales_Mar'18","Sales_Mar'19","Sales_Mar'20","Sales_Mar'21","Sales_Mar'22")
for(i in 1:n)
{
  a1<-tab[[i]][[1]][6,]
  a11<-(as.numeric(gsub(",","",a1)))[2:11]
  m3[i,]<-a11
}


m4<-rbind(m,m1,m2,m3)
m4
m4[is.na(m4)]<-0
proj_data<-data.frame(project_main_data,m4)
proj_data
save(proj_data,file="Final.Rdata")
#page4
 newp4 <- as.data.frame(p4_new)
 newp4$year = c(2001,1995,1960,2000,1991)
 print(newp4)
 save(newp4,file = "newp4.Rdata")
 #page3
 newp3 <- as.data.frame(p3_new)
 newp3$year = c(1983,1997,1996,2011,1998,2017,1994,1996,1992,1984,1989,1993,1978,1969,1977,1981,2020,1989,2000,1984,1984,1999,1992,1995,1988,1990,1987,1951,1988,1979)
 print(newp3)
 save(newp3,file = "newp3.Rdata")
 #page1
 newp1 <- as.data.frame(p1_new)
 newp1$year= c(1983,1990,1935,1984,1983,1959,1944,2001,1973,1952,1978,1978,1968,1986,2005,1924,1950,1993,1949,1996,1849,1973,1976,2000,2004,1987,2000,1907,2018,1981) 
 print(newp1)
 save(newp1,file = "newp1.Rdata")
#page2
 newp2<- as.data.frame(p2_new)
 newp2$year= c(1977,2007,1984,1995,1980,1979,1937,1999,2007,1990,2009,1936,1984,1989,1947,1986,1996,1960,1987,1992,2007,1947,1990,1944,2002,1985,1970,1986,1992,1984) 
 print(newp2)
 save(newp2,file = "newp2.Rdata")
 #main data
 newmain<- as.data.frame(project_main_data)
 newmain$year= c(1983,1990,1935,1984,1983,1959,1944,2001,1973,1952,1978,1978,1968,1986,2005,1924,1950,1993,1949,1996,1849,1973,1976,2000,2004,1987,2000,1907,2018,1981,1977,2007,1984,1995,1980,1979,1937,1999,2007,1990,2009,1936,1984,1989,1947,1986,1996,1960,1987,1992,2007,1947,1990,1944,2002,1985,1970,1986,1992,1984,1983,1997,1996,2011,1998,2017,1994,1996,1992,1984,1989,1993,1978,1969,1977,1981,2020,1989,2000,1984,1984,1999,1992,1995,1988,1990,1987,1951,1988,1979,2001,1995,1960,2000,1991) 
 print(newmain)
 save(newmain,file = "newmain.Rdata")

headquart1<- c("Mumbai","Hyderabad","Mumbai","Hyderabad","Chennai","Ahmedabad",
               "Mumbai","Delhi","Mumbai","Ahmedabad","Bengaluru","Hyderabad",
               "Mumbai","Hyderabad","Vishakapatnam","Mumbai","Mumbai","Bengaluru",
               "Delhi","Gurgaon","New York, U.S","Mumbai","Mumbai","Bengaluru",
               "Mumbai","Dubai","Secunderabad","Vadodara","Hyderabad","Hyderabad",
               "Mumbai","Ahmedabad","Hyderabad","Haryana","Mumbai","Cambridge,U.K.",
               "Chinchinati,Ohio,U.S.A","Delhi","Lucknow","Chennai","Noida","Hyderabad",
               "Mumbai","Mumbai","Kolkata","Mumbai","Bangalore","Mumbai",
               "Mumbai","Karnataka","Surat, Gujarat","Mumbai",
               "Bengaluru","Mumbai","Mumbai","India","Mumbai","Ludhiana",
               "Mumbai","Hyderabad"
               )

headquart2<- c("Gujarat","Switzerland","Coimbatore","Delhi",
               "Ahmedabad","Chennai","Ahmedabad","Mumbai","Ahmedabad",
               "Switzerland","Andhra Pradesh","Mumbai","Kolkata","Mumbai",
               "Delhi","Hyderabad","Ahmedabad","Mumbai","Mumbai",
               "Gujarat","Gujarat","Delhi","Mumbai",
               "Mumbai","Mumbai","Hyderabad","Hyderabad","Mumbai",
               "Bihar","Gujarat","Uttarakhand","Kolkata","Mumbai","Chennai",
               "Gujarat"
               )             

headquarter <- c(headquart1,headquart2)

new_main <- new_main %>% mutate(headquarter)

save(data_main, file="main_HQ.Rdata")


load("C:/Users/DELL/Downloads/newmain.Rdata")
View(newmain)
proj_data$year<-newmain$year
load("C:/Users/DELL/Downloads/main_HQ.Rdata")
View(tab)
View(data_main)
proj_data$Headquaters<-data_main$headquarter
proj_data

Name<-proj_data$name_all
Headquarters<-proj_data$Headquaters
Year<-proj_data$year
MarketCap_Rs.Cr.<-as.numeric(proj_data$tw2_all)
CMP<-as.numeric(proj_data$cmp_all)
CMP_Change_percent<-as.numeric(proj_data$perce3_all)
Latest_PE<-as.numeric(proj_data$PE1_all)
Latest_PBV<-as.numeric(proj_data$PB1_all)
FiveYearProfitGr_percent<-as.numeric(proj_data$profit1_all)
ROCE_percent<-as.numeric(proj_data$ROCE_all)





Sales_Mar.13<-as.numeric(proj_data$Sales_Mar.13)
Sales_Mar.14<-as.numeric(proj_data$Sales_Mar.14)
Sales_Mar.15<-as.numeric(proj_data$Sales_Mar.15)
Sales_Mar.16<-as.numeric(proj_data$Sales_Mar.16)
Sales_Mar.17<-as.numeric(proj_data$Sales_Mar.17)
Sales_Mar.18<-as.numeric(proj_data$Sales_Mar.18)
Sales_Mar.19<-as.numeric(proj_data$Sales_Mar.19)
Sales_Mar.20<-as.numeric(proj_data$Sales_Mar.20)
Sales_Mar.21<-as.numeric(proj_data$Sales_Mar.21)
Sales_Mar.22<-as.numeric(proj_data$Sales_Mar.22)




TTM_Sales<-as.numeric(proj_data$TTMSales_all)
BVPS<-as.numeric(proj_data$BVPS_all)
TTM_BVPS<-as.numeric(proj_data$TTMBVPS_all)
Adjusted_Net_Profit<-as.numeric(proj_data$ANP_all)
TTM_ANP<-as.numeric(proj_data$TTMANP_all)
Return_On_Equity<-as.numeric(proj_data$ROE_all)
Operating_Profit_Margin<-as.numeric(proj_data$OPM_all)
Net_Profit_Margin<-as.numeric(proj_data$NPM_all)
Equity_Rs.Cr.<-as.numeric(proj_data$equity_all)
Industry_PE<-as.numeric(proj_data$ipe_all)



Stock_Data<-data.frame(Name,Headquarters,Year,MarketCap_Rs.Cr.,CMP,CMP_Change_percent,Latest_PE,Latest_PBV,FiveYearProfitGr_percent,ROCE_percent,Sales_Mar.13,Sales_Mar.14,Sales_Mar.15,Sales_Mar.16,Sales_Mar.17,Sales_Mar.18,Sales_Mar.19,Sales_Mar.20,Sales_Mar.21,Sales_Mar.22,TTM_Sales,BVPS,TTM_BVPS,Adjusted_Net_Profit,TTM_ANP,Return_On_Equity,Operating_Profit_Margin,Net_Profit_Margin,Equity_Rs.Cr.,Industry_PE)
Stock_Data$Type_Of_MarketCap<-proj_data$cap_all


library(dplyr)
Stock_Data<-Stock_Data%>%relocate(Type_Of_MarketCap,.after=MarketCap_Rs.Cr.)


Stock_Data
save(Stock_Data,file="Pharma_Companies_Stock.Rdata")

 


