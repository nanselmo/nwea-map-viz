library(shiny)
library(readxl)
library(rCharts) 
#load the roster choices
#this file should be the most recent NWEA file
roster<- read_excel("NWEA_Winter_15_16.xls")
hrs<-unique(roster[c("StudentHomeroom")])


shinyUI(fluidPage(
  titlePanel("NWEA Change over Time"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="HR", label="Your HR:", 
                  choices=hrs),
      radioButtons("Subject", label = h5("Which Content Area?"),
                   choices = list("Math" = "Mathematics", "Reading" = "Reading"), 
                   selected = "Reading"),
      hr(),
      helpText("Students that are missing MAP scores will be shifted left")
    ),
    mainPanel(tabsetPanel(type = "tabs", 
                tabPanel("HR MAP Growth", showOutput("scorePlot", "highcharts")), 
                #tabPanel("Summary", verbatimTextOutput("summary")), 
                tabPanel("Student Scores", chartOutput('mytable', 'datatables') )
                
    ))
    
  )
))


