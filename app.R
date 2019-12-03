library(shiny)
library(sqldf)
library(reticulate)
library(jsonlite)
source("datamine.R")


#https://blog.rstudio.com/2018/03/26/reticulate-r-interface-to-python/

ucb=as.data.frame(UCBAdmissions)

#list in R are like dictionaries in python, vector in R are like numpy arrays in python
#print(dim(ucb))
print(ucb[[1]][1]) #wtf r indexes starts at 1 and not 0
print(ucb[1,]) #access first row
print(ucb[,1])
print(ucb$Admit) #samething as accessing column but this is by column name
print(ucb)
#print(nrow(sqldf("select Gender from ucb")))
#print(nrow(sqldf("select Freq from ucb")))
#print(nba_scrape)


ui <- fluidPage(sliderInput('testID', 'slider label', 1, nrow(ucb), 0, step = 1), plotOutput('testoutid'))
server <- function(input, output){
  

  output$testoutid<-renderCachedPlot(
      {
        #rownums <- seq_len(input$testID)
        plot(ucb[1:input$testID,2], ucb[1:input$testID, 4])
      },
      cacheKeyExpr = {input$testID}
    )
  
  
  #renderCachedPlot(){
    
  #}
  #assumble input to output
  #output$testoutid dollar sign used to retrieve the testoutid from the output list  
  #output$testoutid <- renderPlot(expr = {hist(rnorm(input$testID))})
  
}
shinyApp(ui=ui, server=server)


