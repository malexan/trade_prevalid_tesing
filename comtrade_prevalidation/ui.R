library(shiny)
library(ggvis)

data <- readRDS(file = "data.rds") #Dirty way to get values from data

fluidPage(
  
  titlePanel("Comtrade prevalidation"),
  
  
  tabsetPanel(
    tabPanel("Aggregate by HS",
             fluidRow(div(DT::dataTableOutput('hsdata')), style = 
                        "font-size:80%; line-height:71%; 	border-spacing: 1px 1px")
    ),
    tabPanel("Flow outliers", 
             fluidRow(div(DT::dataTableOutput('tbl')), style = 
                        "font-size:80%; line-height:71%; 	border-spacing: 1px 1px"),
             fluidRow(
               column(6, plotOutput('tsplot', width = "600px", height = "300px")),
               column(6, plotOutput('origvaluests', width = "600px", height = "300px"))
             )
    ),
    tabPanel("Settings", 
             
             sliderInput("years", "Years range", min = min(data$year), max = max(data$year),
                         value = c(2009, max(data$year)), sep = "", ticks = T, round = T, step = 1),
             sliderInput("roundn", "Round digits", value = 2, min = 0, max = 4),
             selectInput("uvcompare", "How to show aggregated UVs", 
                         c("As is ($1000/t)" = "asis",
                           "Distance from UV absolute ($1000/t)" = "abso",
                           "Distance from UV relative (percent)" = "rela"))
             # sliderInput("baseyear", "Base year", min = min(data$year), max = max(data$year) - 1, value = 2000)
             
    ),
    tabPanel("Time series data", 
             fluidRow(div(DT::dataTableOutput('tsdata')), style = 
                        "font-size:80%; line-height:71%; 	border-spacing: 1px 1px")
    ),
    tabPanel("Tariffline no quantity",
             fluidRow(ggvisOutput("tariffline_hs2_noquant")))
  ))

