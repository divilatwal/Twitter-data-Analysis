library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme= shinytheme("darkly"),
  
  titlePanel("Budget 2018 Analysis using Tweets"),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput("term","Enter Search Term:","#example"),
      
      sliderInput("i", "Select no. of Tweets:", 0, 1500, 100, step = 50, round = FALSE, format = NULL, locale = NULL, ticks = TRUE, animate = FALSE, width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL, timezone = NULL, dragRange = TRUE),
      
      
      radioButtons("pType", "Select a Plot type:",
                   list("Word Cloud"='a', "Sentiment Analysis"='b', "Hierarchical Clustering"='c',"Topic Modelling" ='d',"Sentiment Trends"='e',"Sentiment and Emotion Analysis"='f')),
      #menuItem("Globe Visulization",  tabName = "globegl"),
      
      
      
      
      submitButton("Analyze!"),
      
      
      print(h6("  Be Patient, Good Things Take Time!"))
      
      
      
    ),
    
    
    mainPanel(
      dataTableOutput("table"),
      plotOutput("plot")
    )
  ) )
)