# LOAD YOUR PACKAGES
library(shiny)
library(quantmod)
library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(dygraphs)
library(feedeR)# show news
library(shinyWidgets)
library(here)
library(dplyr)
library(DT) # create table professionally
library(vars)
library(shinydashboard)
library(shinycssloaders )# Loading message
library(tidyverse)
library(lubridate)
library(xtable)
library(rmgarch)

shinyUI(fluidPage(
  titlePanel(title = h3("Daily prices and Spillover effects between Stock and NASDAQ")),
  
  options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2),
  

  sidebarPanel(
    selectInput("stock_1", "Select Stock",
                choices = c("Apple Inc.","IBM","Starbucks Corporation","NXP Semiconductors N.V.","Netflix",
                            "Johnson & Johnson"),width = "250px"),
    tags$hr(),
    helpText("SplilOver in Mean: How much Asset A' return can affect Asset B' return.(Vector Autoregressive Model)"),
    helpText("SplilOver in Volatility: How much Asset A' Sd can affect Asset B' Sd.(GARCH Dynamic Conditional Correlation Documentation Model)"),
    helpText("Data is Updated daily.Source:Quantmod"),
    img(src='Capture.JPG',height="70%", width="70%",align = "bottom")
    
    ),

  
  


  mainPanel(
    column(width=12,
           withSpinner( dygraphOutput("dygraph"), type = 2)),
   
    column(12,dataTableOutput('table')
    )

  )
)
)

