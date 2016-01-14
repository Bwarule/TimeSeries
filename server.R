## create the Data for time stamp application 
## time series model development
library(forecast)
library(dplyr)
library(xts)
library(shiny)
library(dygraphs)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com
# Create the sample data logic
## This M-Competition data
## more gereral data library library(Mcomp)

## Data repository 
## setwd("C:\\Users\\bharat.warule\\Documents\\NAV_Forecasting")
dataIn <- read.table("NAVHistoryReport311215_Franklin.txt",
                                         fill = TRUE ,
                                         header = TRUE,
                                         sep=";" , 
                                         quote = "",  
                                         row.names = NULL,  
                                         stringsAsFactors = FALSE)

## Just clean the data as per our requirement
## remove the comments lines from txt file
dataIn <- dataIn[which(!(is.na(dataIn$Net.Asset.Value))),] 
## here is help http://www.r-bloggers.com/date-formats-in-r/
dataIn$Date <- as.Date(dataIn$Date,format = "%d-%B-%Y")

## Timebstamp_timeseries used for the data processing 
## Time_stamp = "Date"
## convert <- "Monthly"## c("qtr", "Monthly", "Weekly", "day", "hourly")
time_var <- "Net.Asset.Value"
Data <- dataIn


source("Timestamp_timeseries_new.R")

shinyServer(function(input, output) {
  
  # By declaring datasetInput as a reactive expression we ensure 
  # that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers 
  #	  (it only executes a single time)
  #
  ## "We apologize, but this facility is temporarily unavailable. Please try later."
  datasetInput <- reactive({
  ## datasetInput <- eventReactive(input$goButton, {
    ## input <- list(period = "Monthly",Scheme.Name="Franklin India Dynamic Accrual Fund-Growth")
	validate(
    need(input$Scheme.Name != "",
	"We apologize, but this facility is temporarily unavailable. Please select Scheme.Name"))
    eventdata <-  Timestamp_timeseries(Time_stamp="Date",
                                       Data[which(Data$Scheme.Name == input$Scheme.Name),] ,
                                       convert = as.character(input$period),
                                       time_var= "Net.Asset.Value")
    print(eventdata)
    eventdata
    
  })
  
  
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so will be re-executed whenever
  # input$dataset or input$obs is changed. 
  output$view <- renderTable({
    datasetInput()
  })
  
    ## lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
    ## dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
    ## dyHighlight(highlightCircleSize = 5,
    ##          highlightSeriesBackgroundAlpha = 0.2,
    ##          hideOnMouseOut = FALSE)
	
	output$dygraph <- renderDygraph({
		eventdata <- datasetInput()
		
		# Plot the forecasting plot based on the statistical model
		## This in general model for : monthly, qtr, annual series
		convert = as.character(input$period)
		
		if(convert == "qtr"| convert == "Monthly"){
		    Predicted <- forecast(auto.arima(eventdata))
			}else{
			## daywise, weekly seires are different model
			Predicted <- forecast(HoltWinters(eventdata))
			# plot(forecast(stlm(eventdata,s.window=7)))
		}
		forecast_Data <- cbind(Predicted$mean, Predicted$lower, Predicted$upper)
        dygraph(cbind(eventdata, forecast_Data), main = "Predicted") #%>%
       # dySeries(c("eventdata","mean","lower.80","lower.95","upper.80","upper.95"), label = "Mutual Fund") %>%
       # dyOptions(drawGrid = input$showgrid)
    })
		  

  output$distPlot <- renderPlot({
    
    # forecasting code procedure will run 
    # please set the all input information from ui
    eventdata <- datasetInput()
    
    # Plot the forecasting plot based on the statistical model
    ## This in general model for : monthly, qtr, annual series
    convert = as.character(input$period)
    if(convert == "qtr"| convert == "Monthly"){
        plot(forecast(auto.arima(eventdata)),main=input$Scheme.Name)
    }else{
    ## daywise, weekly seires are different model
       plot(forecast(HoltWinters(eventdata)),main=input$Scheme.Name)
       ## plot(forecast(stlm(eventdata,s.window=7)))
      
    }
    
    
  })
  
})
