#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("TEXT PREDICTOR BASED ON TWITTER, BLOGS AND NEWS DATA"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h1("Text to predict"),  
      textInput("texto",label=NULL,value="write here your text"),
       submitButton("Predict!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h1("Most Probable word"), 
       textOutput("texto_salida3"),
      h1("Other words worth considering"),
       plotOutput("plot_salida")
    )
  )
))
