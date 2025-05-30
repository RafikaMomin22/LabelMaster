#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library("readxl")
library(DT)
choice <- list("Labels", "Packaging", "Placards", "Placarding Systems")
choice[["Labels"]] <- list("Retail Sales" = "RS",
                           "Consumer Price Index" = "CPIALL",
                           "Business Inventories (Mil $)" = "BUSINV",
                           "Total Transp. and Warehouse Emp. (000's)" = "TTWEMP",
                           "Other Transportation Employment (000's)" = "OTHERTRANEMP",
                           "S&P 500" = "SP500",
                           "Food Index" = "G311")
choice[["Packaging"]] <- list("Retail Sales" = "RS",
                              "Consumer Price Index" = "CPIALL",
                              "Public Construction Spending (Mil $)" = "PUBCON",
                              "Business Inventories (Mil $)" = "BUSINV",
                              "Manufacturing Employment (000's)" = "MANEMP",
                              "National Avg. Diesel/Gal." = "D/GAL",
                              "W. Texas Int. Crude Oil ($Bbl.)" = "WTICO",
                              "Food Index" = "G311")
choice[["Placards"]] <- list("Retail Sales" = "RS",
                             "Consumer Price Index" = "CPIALL",
                             "Business Inventories (Mil $)" = "BUSINV")

choice[["Placarding Systems"]] <- list("Business Inventories (Mil $)" = "BUSINV",
                                       "Retail Sales" = "RS",
                                       "S&P 500" = "SP500")

choice2 <- list("Labels", "Packaging", "Placards", "Placarding_Systems")

choice2[["Labels"]] <- list("Intermodal Revenue Movements - Domestic Containers" = "Domestic_Containers_IRM",
                           "International Movements Index" = "International_Movements_Index_TRMI",
                           "Domestic Movements Index" = "Domestic_Movements_Index_TRMI",
                           "U.S. Origin Intermodal Volumes - Domestic" = "Domestic_US",
                           "East Coast Port Activity - Imports" = "Imports_ECPORT",
                           "Gulf Coast Port Activity - Imports" = "Imports_GCPORT",
                           "Gulf Coast Port Activity - Exports" = "Exports_GCPORT",
                           "Total Intermodal Rates - with Fuel Surcharge" = "Total_IM_w_FSC",
                           "Total Intermodal Rates - without Fuel Surcharge" = "Total_IM_w/o_FSC")
choice2[["Packaging"]] <- list("Intermodal Revenue Movements - Domestic Containers" = "Domestic_Containers_IRM")
choice2[["Placards"]] <- list("Intermodal Revenue Movements - Domestic Trailer" = "Domestic_Trailers_IRM",
                            "Intermodal Revenue Movements - Domestic Containers" = "Domestic_Containers_IRM",
                            "International Movements Index" = "International_Movements_Index_TRMI",
                            "U.S. Origin Intermodal Volumes - Domestic" = "Domestic_US")
choice2[["Placarding Systems"]] <- list("International Movements Index" = "International_Movements_Index_TRMI",
                                        "Domestic Movements Index" = "Domestic_Movements_Index_TRMI",
                                        "U.S. Origin Intermodal Volumes - Domestic" = "Domestic_US",
                                        "Total Intermodal Rates - without Fuel Surcharge" = "Total_IM_w/o_FSC")

choice3 <- list("Labels", "Packaging", "Placards", "Placarding_Systems", "Software")

choice3[["Labels"]] <- list("Real GDP" = "Real_GDP",
                            "Real Imports Goods" = "Real_Imports_Goods",
                            "Real Exports Goods" = "Real_Exports_Goods",
                            "Consumer Expenditure" = "Consumer_Expenditure",
                            "Real Exports" = "Real_Exports", 
                            "Real Imports" = "Real_Imports",
                            "CPI Index" = "CPI_Index",
                            "AAA Bonds" = "AAA_Bonds",
                            "Goods Producing Sector" = "Goods_Producing_Sector")
choice3[["Packaging"]] <- list("Real GDP" = "Real_GDP",
                               "Real Imports Goods" = "Real_Imports_Goods",
                               "Real Exports Goods" = "Real_Exports_Goods",
                               "Consumer Expenditure" = "Consumer_Expenditure",
                               "Real Exports" = "Real_Exports",
                               "Real Imports" = "Real_Imports",
                               "CPI Index" = "CPI_Index",
                               "3 Month T-Bill Rate %" = "T_Bill_rate",
                               "AAA Bonds" = "AAA_Bonds",
                               "Federal Surplus, Bil " = "Fed_Surplus",
                               "Housing Starts" = "Housing_Starts",
                               "Goods Producing Sector" = "Goods_Producing_Sector")
choice3[["Placards"]] <- list("Real GDP" = "Real_GDP", 
                              "Real Exports Goods" = "Real_Exports_Goods",
                              "Consumer Expenditure" = "Consumer_Expenditure",
                              "Real Exports" = "Real_Exports", 
                              "CPI Index" = "CPI_Index",
                              "AAA Bonds" = "AAA_Bonds",
                              "Housing Starts" = "Housing_Starts")

choice3[["Placarding Systems"]] <- list("Real GDP" = "Real_GDP",
                                        "Real Imports Goods" = "Real_Imports_Goods",
                                        "Real Exports Goods" = "Real_Exports_Goods",
                                        "Consumer Expenditure" = "Consumer_Expenditure",
                                        "Real Exports" = "Real_Exports",
                                        "Real Imports" = "Real_Imports", 
                                        "CPI Index" = "CPI_Index",
                                        "AAA Bonds" = "AAA_Bonds",
                                        "Output of Goods Producing Sector" = "Goods_Producing_Sector")
choice3[["Software"]] <- list("Real GDP" = "Real_GDP",
                              "Consumer Expenditure" = "Consumer_Expenditure",
                              "CPI Index" = "CPI_Index",
                              "Output of Goods Producing Sector" = "Goods_Producing_Sector")



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  df <- data.frame()
  predVar <- ""
  observe({
    department <- input$sales
    dataSet <- input$data_type
    if(dataSet == 'Monthly Economic Indicator') predVar <- input$pred_var
    else if (dataSet == 'Monthly Movements') predVar <- input$pred_var2
    else predVar <- input$pred_var3
    
    paste(predVar)
    
    if(department == 'Packaging'){
      if(dataSet == 'Monthly Economic Indicator'){
        updateSelectInput(session, "pred_var",
                          choices = choice$Packaging,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_indicators_m.xlsx", sheet = 'Packaging'))
      }
      if(dataSet == 'Monthly Movements'){
        updateSelectInput(session, "pred_var2",
                          choices = choice2$Packaging,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_summary_m.xlsx", sheet = 'Packaging'))
      }
      if(dataSet == 'Quarterly Economic Indicator'){
        updateSelectInput(session, "pred_var3",
                          choices = choice3$Packaging,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_economics_q.xlsx", sheet = 'Packaging'))
      }

    }
    
    if(department == 'Labels'){
      if(dataSet == 'Monthly Economic Indicator'){
        updateSelectInput(session, "pred_var",
                          choices = choice$Labels,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_indicators_m.xlsx", sheet = 'Labels'))
      }
      if(dataSet == 'Monthly Movements'){
        updateSelectInput(session, "pred_var2",
                          choices = choice2$Labels,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_summary_m.xlsx", sheet = 'Labels'))
      }
      if(dataSet == 'Quarterly Economic Indicator'){
        updateSelectInput(session, "pred_var3",
                          choices = choice3$Labels,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_economics_q.xlsx", sheet = 'Labels'))
      }
      
    }
    if(department == 'Placards'){
      if(dataSet == 'Monthly Economic Indicator'){
        updateSelectInput(session, "pred_var",
                          choices = choice$Placards,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_indicators_m.xlsx", sheet = 'Placards'))
      }
      if(dataSet == 'Monthly Movements'){
        updateSelectInput(session, "pred_var2",
                          choices = choice2$Placards,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_summary_m.xlsx", sheet = 'Placards'))
      }
      if(dataSet == 'Quarterly Economic Indicator'){
        updateSelectInput(session, "pred_var3",
                          choices = choice3$Placards,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_economics_q.xlsx", sheet = 'Placards'))
      }

      
    }
    if(department == 'Placarding Systems'){
      if(dataSet == 'Monthly Economic Indicator'){
        updateSelectInput(session, "pred_var",
                          choices = choice$`Placarding Systems`,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_indicators_m.xlsx", sheet = 'Placarding_Systems'))
      }
      if(dataSet == 'Monthly Movements'){
        updateSelectInput(session, "pred_var2",
                          choices = choice2$`Placarding_Systems`,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_summary_m.xlsx", sheet = 'Placarding_Systems'))
      }
      
      if(dataSet == 'Quarterly Economic Indicator'){
        updateSelectInput(session, "pred_var3",
                          choices = choice3$`Placarding_Systems`,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_economics_q.xlsx", sheet = 'Placarding_Systems'))
      }
    }
    
    if(department == 'Software'){
      if(dataSet == 'Monthly Economic Indicator'){
        updateSelectInput(session, "pred_var",
                          choices = list(),
                          selected = tail(predVar, 1))
      }
      if(dataSet == 'Monthly Movements'){
        updateSelectInput(session, "pred_var2",
                          choices = list(),
                          selected = tail(predVar, 1))
      }
      
      if(dataSet == 'Quarterly Economic Indicator'){
        updateSelectInput(session, "pred_var3",
                          choices = choice3$Software,
                          selected = tail(predVar, 1))
        df <- as.data.frame(read_excel("tr_economics_q.xlsx", sheet = 'Software'))
      }
      
    }

    output$summary <- renderText({
      #print(predVar)
      #print(colnames(df))
      model <- lm(DeptSales ~ bs(df[,predVar], df = 30), data = df)
      model_summary <- summary(model)
      paste("Formula for spline regression: \n \n lm(DeptSales) ~ bs(df[,",predVar, ", df = 30), data = df", "\n",
            "Residual standard error: " , round(model_summary$sigma), " on " , model$df , " degrees of freedom \n\n" ,
            "Multiple R-squared: ", round(model_summary$r.squared, 4), " Adjusted R-squared: ", round(model_summary$adj.r.squared,4),
            "\n\nF-statistic: ", round(model_summary$fstatistic[1], 4) , " on ", model_summary$fstatistic[2], " and ", model_summary$fstatistic[3], " DF ")
    })
    
    output$scatterplot <- renderPlot({
      model <- lm(DeptSales ~ bs(df[,predVar], df = 30), data = df)
      options(scipen=10000)
      x <- df[,predVar][!is.na(df[, predVar])]
      y <- df$DeptSales[!is.na(df[, predVar])]
      correlation_val <- round(cor(x, y), digits = 4)
      plot(x, y, main = input$sales, 
           xlab = predVar, ylab = "Sales", pch = 19)
      lines(smooth.spline(x, predict(model)), col = 'blue', lwd = 3)
      legend("topleft", legend=c("Spline Fit"),
             col=c("blue"), lty=1, lwd = 3, cex=0.8)
      legend("topright", legend=c(paste("Correlation", correlation_val)))
    })
  


    
  })
  observeEvent(input$dataType, {
    if(input$dataType == 'Monthly Economic Indicator'){
      output$value_inp <- renderUI({
        dept <- input$dept
        numVar <- length(choice[[dept]])
        lapply(1:numVar, function(i) {
          numericInput(inputId = as.character(choice[[dept]][i]), label = names(choice[[dept]])[i], value = 0)
        })
      })
    }
    else if(input$dataType == 'Monthly Movements'){
      output$value_inp2 <- renderUI({
        dept <- input$dept
        numVar <- length(choice2[[dept]])
        lapply(1:numVar, function(i) {
          numericInput(inputId = as.character(choice2[[dept]][i]), label = names(choice2[[dept]])[i], value = 0)
        })
      })
    }
    else if(input$dataType == 'Quarterly Economic Indicator'){
      output$value_inp3 <- renderUI({
        dept <- input$dept
        numVar <- length(choice3[[dept]])
        lapply(1:numVar, function(i) {
          numericInput(inputId = as.character(choice3[[dept]][i]), label = names(choice3[[dept]])[i], value = 0)
        })
      })
    }
  })
  observeEvent( input$predict_sales, {
    dept <- input$dept
    print(dept)
    numVar <- length(choice[[dept]])
    if(dept == 'Placarding Systems'){
      df <- as.data.frame(read_excel("tr_indicators_m.xlsx", sheet = 'Placarding_Systems'))
    }
    else{
      df <- as.data.frame(read_excel("tr_indicators_m.xlsx", sheet = dept))
    }
    
    print(colnames(df))
    v_values <- c()
    pred_values <- c()
    x_names <- c()
    accuracy <- c()
    corr_value <- c()
    warning_message <- ""
    
    for ( i in 1: numVar) v_values <- c(v_values, input[[as.character(choice[[dept]][i])]])

    print(v_values)
    for (i in 1: numVar){
      if(!(is.na(v_values[i])) && v_values[i] > 0){

        col_name <- as.character(choice[[dept]][i])
        print(col_name)

        x <- df[,col_name][!is.na(df[, col_name])]
        y <- df$DeptSales[!is.na(df[, col_name])]
        
        correlation_val <- round(cor(x, y), digits = 2)
        corr_value <- c(corr_value, correlation_val)
        
        model <- lm(y ~ bs(x, df = 30))
        
        outside_range = 0
        input_number <- v_values[i]
        # Check if the number is within the range acceptable, if not then make predictions for number that is closest to it. 
        if(v_values[i] < min(x) || v_values[i] > max(x)) {
          input_number <- x[which.min(abs(x-v_values[i]))]
          outside_range = 1 
        }
        p <-predict(model, newdata = data.frame("x" = c(input_number)))
        p <- trunc(p)
        pred_values <- c(pred_values, p)
        
        #r_squared <- round(summary(model)$adj.r.squared, 2)
        x_names <- c(x_names, paste(col_name))
        if (outside_range == 1){
          accuracy <- c(accuracy, paste(formatC(p, format="d", big.mark=","), "(Correlation: ", correlation_val, ") **"))
          warning_message <- "\n** Input value outside range, using closest value to predict."
        }
        else accuracy <- c(accuracy, paste(formatC(p, format="d", big.mark=","), " (Correlation: ", correlation_val, ")"))
      }
    }
    print(pred_values)
    output$results <- renderPlot({
      output.data <- data.frame(pred_values, x_names, corr_value) 
      ggplot(data= output.data, aes(x=pred_values, y= x_names, fill = corr_value)) + 
        geom_bar(stat="identity", width = 0.2 ) + 
        scale_fill_gradient(low="white",high="darkblue") +
        geom_text(aes(label= pred_values), vjust=2.0, size=3.5) +
        labs ( x = "Sales", y = "External Variable") +
        theme_grey()
    })
    output$dataset <- DT::renderDataTable({
      data.frame("External_Variable" = x_names, "Predicted_Sales" = accuracy)
    })
    
    output$warning <- renderText({
      paste0("Predicted Sales : " , mean(pred_values), 
             warning_message)
    })
  })
  
  observeEvent( input$predict_sales2, {
    dept <- input$dept
    print(dept)
    numVar <- length(choice2[[dept]])
    if(dept == 'Placarding Systems'){
      df <- as.data.frame(read_excel("tr_summary_m.xlsx", sheet = 'Placarding_Systems'))
    }
    else{
      df <- as.data.frame(read_excel("tr_summary_m.xlsx", sheet = dept))
    }

    
    print(colnames(df))
    v_values <- c()
    pred_values <- c()
    x_names <- c()
    accuracy <- c()
    corr_value <- c()
    warning_message <- ""
    
    for ( i in 1: numVar) v_values <- c(v_values, input[[as.character(choice2[[dept]][i])]])
    
    print(v_values)
    for (i in 1: numVar){
      if(!(is.na(v_values[i])) && v_values[i] > 0){

        col_name <- as.character(choice2[[dept]][i])
        print(col_name)

        x <- df[,col_name][!is.na(df[, col_name])]
        y <- df$DeptSales[!is.na(df[, col_name])]
  
        correlation_val <- round(cor(x, y), digits = 2)
        corr_value <- c(corr_value, correlation_val)
        
        model <- lm(y ~ bs(x, df = 30))
        
        outside_range = 0
        input_number <- v_values[i]
        # Check if the number is within the range acceptable, if not then make predictions for number that is closest to it. 
        if(v_values[i] < min(x) || v_values[i] > max(x)) {
          input_number <- x[which.min(abs(x-v_values[i]))]
          outside_range = 1 
        }
        
        p <-predict(model, newdata = data.frame("x" = c(input_number)))
        p <- trunc(p)
        pred_values <- c(pred_values, p)
        
        x_names <- c(x_names, paste(col_name))
        
        if (outside_range == 1){
          accuracy <- c(accuracy, paste(formatC(p, format="d", big.mark=","), "(Correlation: ", correlation_val, ") **"))
          warning_message <- "\n** Input value outside range, using closest value to predict."
        }
        else accuracy <- c(accuracy, paste(formatC(p, format="d", big.mark=","), " (Correlation: ", correlation_val, ")"))
      }
    }
    print(pred_values)
    output$results <- renderPlot({
      output.data <- data.frame(pred_values, x_names, corr_value) 
      ggplot(data= output.data, aes(x=pred_values, y= x_names, fill = corr_value)) + 
        geom_bar(stat="identity", width = 0.2 ) + 
        scale_fill_gradient(low="white",high="darkblue") +
        geom_text(aes(label= pred_values), vjust=2.0, size=3.5) +
        labs ( x = "Sales", y = "External Variable") +
        theme_grey()
      
      
    })
    output$dataset <- renderDataTable({
      data.frame("External_Variable" = x_names, "Predicted_Sales" = accuracy)
    })
    
    output$warning <- renderText({
      paste0("Predicted Sales : " , mean(pred_values), warning_message)
    })
  })
  
  observeEvent( input$predict_sales3, {
    dept <- input$dept
    print(dept)
    numVar <- length(choice3[[dept]])
    if(dept == 'Placarding Systems'){
      df <- as.data.frame(read_excel("tr_economics_q.xlsx", sheet = 'Placarding_Systems'))
    }
    else{
      df <- as.data.frame(read_excel("tr_economics_q.xlsx", sheet = dept))
    }
    
    print(colnames(df))
    v_values <- c()
    pred_values <- c()
    x_names <- c()
    accuracy <- c()
    corr_value <- c()
    warning_message <- ""
    for ( i in 1: numVar) v_values <- c(v_values, input[[as.character(choice3[[dept]][i])]])
    

    print(v_values)
    for (i in 1: numVar){
      if(!(is.na(v_values[i])) && v_values[i] > 0){

        col_name <- as.character(choice3[[dept]][i])

        x <- df[,col_name][!is.na(df[, col_name])]
        y <- df$DeptSales[!is.na(df[, col_name])]
        
      
        correlation_val <- round(cor(x, y), digits = 2)
        
        model <- lm(y ~ bs(x, df = 30))
        outside_range = 0
        input_number <- v_values[i]
        # Check if the number is within the range acceptable, if not then make predictions for number that is closest to it. 
        if(v_values[i] < min(x) || v_values[i] > max(x)) {
          input_number <- x[which.min(abs(x-v_values[i]))]
          outside_range = 1 
        }

        print(input_number)
        p <-predict(model, newdata = data.frame("x" = c(input_number)))
        p <- trunc(p)
        pred_values <- c(pred_values, p)
        
        #r_squared <- round(summary(model)$adj.r.squared, 2)
        x_names <- c(x_names, paste(col_name))
        corr_value <- c(corr_value, correlation_val)
        if (outside_range == 1){
        accuracy <- c(accuracy, paste(formatC(p, format="d", big.mark=","), "(Correlation: ", correlation_val, ") **"))
        warning_message <- "\n** Input value outside range, using closest value to predict."
        }
        else accuracy <- c(accuracy, paste(formatC(p, format="d", big.mark=","), " (Correlation: ", correlation_val, ")"))
      }
    }
    print(pred_values)
    output$results <- renderPlot({
      
      output.data <- data.frame(pred_values, x_names, corr_value) 
      ggplot(data= output.data, aes(x=pred_values, y= x_names, fill = corr_value)) + 
        geom_bar(stat="identity", width = 0.2 ) + 
        scale_fill_gradient(low="white",high="darkblue") +
        geom_text(aes(label= pred_values), vjust= -0.3, size=3.5) +
        labs ( x = "Sales", y = "External Variable") +
        theme_grey()
      
    })
    output$dataset <- renderDataTable({
      data.frame("External_Variable" = x_names, "Predicted_Sales" = accuracy)
    })
    
    output$warning <- renderText({
      paste0("Predicted Sales : " , round(mean(pred_values)), 
      "\n* Please note that the predicted sales is the expected sales for each month in the given quarter",
      warning_message)
      
    })
  })
  
  
  
})
