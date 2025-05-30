#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
#install.packages("shinythemes")
library(shiny)
library(splines)
library(DT)
#library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predicting Department Sales"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition="input.tabselected==2",
      selectInput("sales", label = h4("Department"),
                  choices = list("Labels" = "Labels",
                                 "Packaging" = "Packaging",
                                 "Placards" = "Placards",
                                 "Placarding Systems", 
                                 "Software"), selected = 1),
      radioButtons("data_type","Choose dataset",c("Monthly Economic Indicator", "Monthly Movements", "Quarterly Economic Indicator"), selected = NULL),
      conditionalPanel(condition= "input.data_type== 'Monthly Economic Indicator'", 
                        selectInput("pred_var", label = h4("External variable"), choices = NULL, selected = 1)
      ),
      conditionalPanel(condition= "input.data_type== 'Monthly Movements'", 
                      selectInput("pred_var2", label = h4("External variable"), choices = NULL, selected = 1)
                                        
      ),
      conditionalPanel(condition= "input.data_type== 'Quarterly Economic Indicator'", 
                       selectInput("pred_var3", label = h4("External variable"), choices = NULL, selected = 1)
      )
      
    ),
    conditionalPanel(condition="input.tabselected==3",
         selectInput("dept", label = h4("Department"),
                     choices = list("Labels" = "Labels",
                                    "Packaging" = "Packaging",
                                    "Placards" = "Placards",
                                    "Placarding Systems",
                                    "Software"), selected = 1),
         #selectInput("pred_var", label = h4("External variable"), choices = NULL, selected = 1)
         radioButtons("dataType","Choose dataset",c("Monthly Economic Indicator", "Monthly Movements", "Quarterly Economic Indicator"), selected = NULL),
         conditionalPanel(condition= "input.dataType== 'Monthly Economic Indicator'", 
                          uiOutput("value_inp", label = h4("Enter value for external variable")),  
                          actionButton("predict_sales", label = "Predict Sales")),
         conditionalPanel(condition= "input.dataType== 'Monthly Movements'", 
                          uiOutput("value_inp2", label = h4("Enter value for external variable")),  
                          actionButton("predict_sales2", label = "Predict Sales")),
         conditionalPanel(condition= "input.dataType== 'Quarterly Economic Indicator'", 
                          uiOutput("value_inp3", label = h4("Enter value for external variable")),  
                          actionButton("predict_sales3", label = "Predict Sales"))         
         
         #numericInput("pred_dat", label = h4("Enter value for external variable"), value = 0, min = 0),
    )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplot", value = 2, fluid = TRUE,
                 plotOutput("scatterplot")
        ),
        tabPanel("Model Summary", value = 2, fluid = TRUE,
                   verbatimTextOutput("summary")
                 
        ),
        tabPanel("Prediction Result", value = 3, fluid = TRUE,

                     #verbatimTextOutput("results")
                 plotOutput("results"),
                 DT::dataTableOutput("dataset"),
                 verbatimTextOutput("warning")
                 
                 #)
        ), id = "tabselected")
    )
  )
  
))
