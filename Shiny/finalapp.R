

library(shiny)
library(dplyr)
library(ggplot2)
library(reshape)



ui <- navbarPage(titlePanel(div(h4("VISUALIZATIONS ON STOCK DATA OF PHARMACEUTICAL COMPANIES"), style ="color: #CCFF33")),

                 
                 tabPanel ( title = h4 ("PIE CHART"),
                          fluidPage(
                            
                            titlePanel ("PIE CHART OF TYPE OF MARKET CAP"),
                            
                             mainPanel(
                              plotOutput("distPlot1"),
                               
                              br(),
                              br(),
                              textOutput("top8"), 
                               
                              tags$head(tags$style("#top8{color: red;
                                 font-size: 24px;
                                 font-style: italic;
                                 }"
                              )
                              )
                            )
                          )),
                 
                 tabPanel(title = h4("ANALYSIS OF STOCK VARIABLES"),
                          fluidPage(
                            
                            titlePanel (h1 ("ANALYSIS OF STOCK VARIABLES")),
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                radioButtons (inputId = "var1",
                                             label = "Choose A Variable",
                                             choices = c(names(Stock_Data)[c(4,6:12,22:31)]),
                                             selected = "MarketCap_Rs.Cr."),
                                
                                selectInput (inputId = "type1",
                                            label = "Choose which type to colour the barplot",
                                            choices = c("Type_Of_MarketCap","Year")
                                ),
                                
                                selectInput (inputId = "type2",
                                            label = "Choose which type to colour the histogram",
                                            choices = c("Type_Of_MarketCap","Year")
                                )
                                
                                
                              ),
                             
                              mainPanel(
                                
                                tabsetPanel(
                                  
                                  tabPanel ("Barplot",plotOutput("distPlot2"),br(),textOutput("top2")),
                                  
                                  tabPanel ("Summary and Boxplot", verbatimTextOutput("summary"),plotOutput("box")),
                                  
                                  tabPanel ("Histogram",plotOutput("hist")),
                                  
                                  tags$head(tags$style("#top2{color: red;
                                 font-size: 18px;
                                 font-style: italic;
                                 }"
                                  )
                                  ),
                                  
                                  tags$head(tags$style("#summary{color: black;
                                 font-size: 18px;
                                 }"
                                  )
                                  
                                  )
                                )
                              )
                            )
                          )),

                 tabPanel (h4 ("YEARLY SALES ANALYSIS"),
                          fluidPage(
                            
                           
                            titlePanel( h1("YEARLY SALES ANALYSIS")),
                            
                            sidebarLayout(
                              
                              sidebarPanel(
                                
                                checkboxGroupInput ("var",
                                                   "Choose a variable",
                                                   choices = c(names(Stock_Data)[c(12:21)]),
                                                   selected = "Sales_Mar.13"),width = 3,
                                
                              ),
                              
                              
                              mainPanel(
                                
                                tabsetPanel(
                                  
                                  tabPanel ("Boxplots",plotOutput("distPlot3")),
                                  
                                  tabPanel ("Overlapped Histogram and Frequency Polygon",plotOutput("hist2"),plotOutput("top4")),
                                  
                                )
                              )
                            )
                          )),
                 
                 tabPanel (title = h4("COMPARISON OF YEARLY SALES FOR DIFFERENT COMPANIES"),
                           
                          fluidPage(
                            
                            titlePanel(h1("COMPARISON OF YEARLY SALES FOR DIFFERENT COMPANIES")),
                            
                            fluidRow(column(2,checkboxGroupInput(inputId = "comp",
                                                                 label = "Choose which Company",
                                                                 choices = sort(Stock_Data$Name),
                                                                 selected = c("Cipla Ltd","Divis Laboratories Ltd"))),
                                     
                                     column(8,plotOutput("line1"),
                                            br(),
                                            textOutput("top1"),
                                            tags$head(tags$style("#top1{color: red;
                                 font-size: 18px;
                                 font-style: italic;
                                 }"))),
                                     
                                     column(2,radioButtons(inputId = "year",
                                                           label = "Choose A year",
                                                           choices=names(Stock_Data)[12:21],
                                                           selected = "Sales_Mar.14")),
                                     
                                     position = "left"
                            )
                            
                          ))
                 ,
                 inverse = TRUE,
                 
                 )

server <- function (input, output) {
  
  
  a1 <- length(which(Stock_Data$Type_Of_MarketCap=="Large Cap"))
    
    a2 <- length(which(Stock_Data$Type_Of_MarketCap=="Mid Cap"))
    
    a3 <- length(which(Stock_Data$Type_Of_MarketCap=="Small Cap"))
    
    Type  <- c("Large Cap","Mid Cap","Small Cap")
    
    Percent <- 100*c(a1/nrow(Stock_Data),a2/nrow(Stock_Data),a3/nrow(Stock_Data))
    
    d <- data.frame(Type,Percent)
 

#Pie chart 
  output$distPlot1 <- renderPlot({
    
    
    ggplot (d, aes(x="", y=Percent, fill=Type)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y") +
      theme_void() + 
      geom_text(aes( label = round(Percent,2)),position=position_stack(vjust=0.5),size=12, color = "white")+theme(legend.title = element_text(size=20),legend.text=element_text(size=20))
    
  })
  
  output$top8<-renderText({
    
    paste (d[1,2]," % , ",round(d[2,2],2)," % and ",round(d[3,2],2)," % of the pharmaceutical companies have LARGE, MID and SMALL market cap respectively.")
    
  })

    
  sub <- reactive ({
    cont<-input$var1
    subset(Stock_Data,select=c("Name",cont))
  })
  
  sub1 <- reactive({
    con <- input$type1
    subset(Stock_Data,select=con)
  })
  sub2 <- reactive({
    con <- input$type2
    subset(Stock_Data,select=con)
  })
  
  

 #Bar plot
  
  output$distPlot2 <- renderPlot({
    
    da <- sub()
    da1 <- sub1()
    colnames(da) <- c("Name","Value")
    t <- numeric()
    if(names(da1) == "Type_Of_MarketCap")
    {
      
      r <- Stock_Data$Type_Of_MarketCap
      for(i in 1:95)
      {
        if(r[i] == "Large Cap")
          t[i]<-1
        else if (r[i] == "Mid Cap")
          t[i]<-2
        else
          t[i]<-3
      }
      t1 <- as.factor(t)
      levels(t1) <- c("Large Cap","Mid Cap","Small Cap")
      colnames(da) <- c("Name","Value")
      ggplot(da,aes_string(y="Value",x="Name"))+geom_bar(stat="identity",aes(fill=t1))+theme(axis.title = element_text(size = 16),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))+labs(fill="Type of Market Cap",x=input$var1)
    }
    
    else
    {
      
      r <- Stock_Data$Year
      for(i in 1:95)
      {
        if(r[i]<1900)
          t[i]<-1
        else if (r[i]>=1900 && r[i]<1950)
          t[i]<-2
        else if(r[i]>=1950 && r[i]<1975)
          t[i]<-3
        else if(r[i]>=1975 && r[i]<2000)
          t[i]<-4
        else
          t[i]<-5
      }
      t1 <- as.factor(t)
      levels(t1) <- c("before 1900","from 1900 to before 1950","from 1950 to before 1975","from 1975 to before 2000","from 2000")
      colnames(da) <- c("Name","Value")
      ggplot(da,aes_string(y="Value",x="Name"))+geom_bar(stat="identity",aes(fill=t1))+theme(axis.title = element_text(size = 16),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))+labs(fill="Year of Establishment")
    }
    

    
  })
  

  #Histogram
  
  output$hist <- renderPlot({
    
    da <- sub()
    da2 <- sub2()
    t <- numeric()
    if(names(da2) == "Type_Of_MarketCap")
    {
      
      r <- Stock_Data$Type_Of_MarketCap
      for(i in 1:95)
      {
        if(r[i] == "Large Cap")
          t[i]<-1
        else if (r[i] == "Mid Cap")
          t[i]<-2
        else
          t[i]<-3
      }
      t1 <- as.factor(t)
      levels(t1) <- c("Large Cap","Mid Cap","Small Cap")
      colnames(da) <- c("Nam","Value")
      ggplot(da,aes_string(x="Value"))+geom_histogram(aes(fill=t1,bins = 200))+theme(axis.title = element_text(size = 16),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))+labs(fill="Type of Market Cap",x=input$var1,y="Frequency")
    }
    
    else
    {
      
      r <- Stock_Data$Year
      for(i in 1:95)
      {
        if(r[i]<1900)
          t[i]<-1
        else if (r[i]>=1901 && r[i]<1950)
          t[i]<-2
        else if(r[i]>=1950 && r[i]<1975)
          t[i]<-3
        else if(r[i]>=1975 && r[i]<2000)
          t[i]<-4
        else
          t[i]<-5
      }
      t1 <- as.factor(t)
      levels(t1) <- c("before 1900","from 1900 to before 1950","from 1950 to before 1975","from 1975 to before 2000","from 2000")
      
      colnames(da) <- c("Nam","Value")
      ggplot(da,aes_string(x="Value"))+geom_histogram(aes(fill=t1,bins = 200))+theme(axis.title = element_text(size = 16),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))+labs(fill="Year of Establishment",x=input$var1,y="Frequency")
    }
    
  })
  
  output$summary <- renderPrint({
    da <- sub()
    summary(da)
  })
  

  #Box Plot


  output$box <- renderPlot({
    da <- sub()
    da <- melt(da)
    ggplot(da,aes(x=variable,y=value))+geom_boxplot(aes(fill=variable))+theme(axis.title = element_text(size = 14),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))+labs(fill="Stock Variable")
  })
  
  output$top2 <- renderText({
    da <- sub()
    t1 <- which.max(da[,2])
    t2 <- which.min(da[,2])
    str1 <- paste("Company having highest value of ",input$var1,"is",da[t1,1],".","   ")
    str2 <- paste("Company having lowest value of ",input$var1,"is",da[t2,1],".")
    paste(str1,str2)
  })
  
    
  a <- as.matrix(Stock_Data)
    
  colnames(a) <- NULL
    
  s <- c("March'13","March'14","March'15","March'16","March'17","March'18","March'19","March'20","March'21","March'22")
    
  e <- NULL
    
  for(i in 1:95)
  {
    b <- cbind(rep(a[i,1],10),s,a[i,12:21])
    e <- rbind(e,b)
  }
    
  colnames(e) <- c("Name","Year","Sales")
    
  e <- data.frame(e)
  
  sub3 <- reactive({
    cont <- input$comp
    subset(e,e$Name %in% cont)
  })
  
  sub4 <- reactive({
    con1 <- input$comp
    filter(Stock_Data,Stock_Data$Name %in% con1)
  })
    
  sub5 <- reactive({
    con <- input$year
    subset(sub4(),select = con)
  })
  
  
  output$line1 <- renderPlot({
    
    dat <- sub3()
    dat1 <- sub4()
    ci <- sub5()
    ind1 <- which(ci==max(ci))
    ind2 <- which(ci==min(ci))
    der <- Stock_Data[12:21]
    r <- which(colnames(der)==input$year)
    ggplot(dat,aes_string("Year", "Sales" ,group="Name" ,col="Name"))+geom_line(size=1)+theme(
      axis.title = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14)
    )
    
  })
  output$top1 <- renderText({
    dat1 <- sub4()
    time <- sub5()
    c <- numeric()
    c <- time
    ind1 <- which(c==max(c))
    ind2 <- which(c==min(c))
    paste("Company with the highest Sales is",dat1[ind1,]$Name,"and lowest Sales is", dat1[ind2,]$Name,"for", input$year)
    
    
  })
  
  sub8 <- reactive({
    cont <- input$var
    subset(Stock_Data,select=cont)
  })
  
  #Boxplot


  output$distPlot3 <- renderPlot({
    dat <- sub8()
    da <- melt(dat)
    ggplot(da,aes(x=variable,y=value))+geom_boxplot(aes(fill=variable))+theme(axis.title = element_text(size = 16),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))+labs(fill="Year Of Sales")
  })
  

   #Histogram

  output$hist2 <- renderPlot({
    dat <- sub8()
    da <- melt(dat)
    ggplot(da,aes(x=value))+geom_histogram(aes(fill=variable),col="black")+theme(axis.title = element_text(size = 16),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))+labs(fill="Year Of Sales")+labs(col="Year Of Sales",x="Sales",y="Frequency")
    
  })


  #Frequency Polygon


  output$top4 <- renderPlot({
    dat <- sub8()
    da <- melt(dat)
    ggplot(da,aes(x=value))+geom_freqpoly(aes(col=variable),size=1)+theme(axis.title = element_text(size = 16),axis.text=element_text(size=14),legend.title=element_text(size=16),legend.text=element_text(size=16))+labs(col="Year Of Sales",x="Sales",y="Frequency")
  })  
    
  }
  

# Run the application 
shinyApp(ui = ui, server = server)
