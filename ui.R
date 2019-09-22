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
  titlePanel("T3xtr"),
  h3("Pablo Sainz"),
  h3("September 2019"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("targetString","Enter the test string:"),
       submitButton("Predict text")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       h3("And the resulting string is..."),
       textOutput("resultString")
    )
  )
))
