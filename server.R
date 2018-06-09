#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   output$texto_salida3<-renderText({
    #as.character(dt2[1,1])
    outputtable<-mostfrequenttable(input$texto)
    outputtable<-removeinput(input$texto,outputtable)
    as.character(outputtable[1,1])
    #as.character("prueba")
  })
  output$plot_salida<-renderPlot({
    outputtable<-mostfrequenttable(input$texto)
    outputtable<-removeinput(input$texto,outputtable)
    wordcloud(words=outputtable$ngram,freq=outputtable$count,max.words=20,vfont=c("sans serif","plain"),colors=palette())
    
  })
  
})
