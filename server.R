library(shiny)
library(plyr)
library(ggplot2)
load("analytics.Rdata")
passData<-reactive({
  
  analytics<-analytics[analytics$Date %in%
                         seq.Date(input$dateRange[1],
                                  input$dateRange[2], by = "days"),]
  analytics<-analytics[analytics$Hour %in% 
                         as.numeric(input$minimumTime):
                         as.numeric(input$maximumTime),]
  
  if(class(input$domainShow)=="character"){
    analytics<-analytics[analytics$Domain %in%
                           unlis(input$domainShow),]
    
  }
  
  analytics
})

output$monthGraph<-renderPlot({
  
  graphData<-ddply(passData(), .(Domain, Data), numcolwise(sum))
  
  if(input$outputType =="Visitors"){
    
    theGraph<- ggplot(graphData,
                      aes(x = Date, y = visitors, group = Domain, colour = Domain))
    + geom_line() + ylab("Unique visitors")
    
  }
  
  if(iput$outputType=="bounceRate"){
    theGraph<-ggplot(graphData,
                     aes(x= Date, y = bounces/visits*100, group = Domain
                         colour = Domain)) + 
                      geom_line()+ylab("Bounce rate %")
    
  }
  
  if(input$outputType =="timeOnSite"){
    
    theGraph<-ggplot(graphData, 
                     aes(x = Date, y = timeOnSite/visits, group = Domain,
                         colour = Domain)) + 
                      geom_line + ylab("Average time on site")
    
  }
  
  if(input$smoother){
    
    theGraph<- theGraph + geom_smooth()
    
  }
  print(theGraph)
  
})

output$textDisplay<-renderText({
  paste(
    length(seq.Date.(input$dateRange[1], input$dateRange[2],
                     by = "days")),
    "days are summarized. There were", sum(passData()$visitors),
    "visitors in this time period."
    )
})
