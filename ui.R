# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
dataIn <- read.table("NAVHistoryReport311215_Franklin.txt",
                     fill = TRUE ,
                     header = TRUE,
                     sep=";" , 
                     quote = "",  
                     row.names = NULL,  
                     stringsAsFactors = FALSE)
library(shiny)
## library(dygraphs)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Forecasting Appliaction"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("period", "period:",
                  c("qtr", "Monthly", "Weekly", "day", "hourly")),
      selectInput("Scheme.Name", "Scheme.Name:",
                  c(unique(dataIn$Scheme.Name))),
	   br(),
	   submitButton("RunModel"),
	   p("Click the button to Run foresting model which based on ARIMA and HoltWinter.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
  	 img(src='logo.png', align = "right"),
	  ## dygraphOutput("dygraph"),
    plotOutput('distPlot')#,
    #tableOutput("view")
    )
  )
))
