library(plotly)
library(zoo)

ui <- fluidPage(

    titlePanel("[Synthetic future's app]"),

    inputPanel(
        sliderInput("call140", "Price of $1.40 Call", min = .01, max = 0.50,
                    value = .09, step = .01, animate = TRUE),
        sliderInput("call150", "Price of $1.50 Call", min = .01, max = 0.50,
                    value = .04, step = .01, animate = TRUE),
        sliderInput("put140", "Price of $1.40 Put", min = .01, max = .50,
                    value = .03, step = .01, animate = TRUE),
        sliderInput("put150", "Price of $1.50 Put", min = .01, max = .50,
                    value = .12, step = .01, animate = TRUE)
    
    ),

    mainPanel(
        tabsetPanel(type = "pills",
            tabPanel(title = "Synthetic future's", plotlyOutput("cumPrinOut"))
           
        ),
        img(src="mi.png")#, height = 400, width = 400)
    )
)

server <- function(input, output) {

    dataInput <- reactive({

        
        input$call140
        input$call150
        input$put140
        input$put150
         
# Definitions:
        # CP1.50 = 1.50 call price 
        # CF1.50 = 1.50 cash flow; formula: 
        # SP= strike price
        # CP1.40 = 1.40 call price
        # CF1.40 = 1.40 cash flow 
        # FP = forward price  
        
        
### List of data to be used in plots below -----
        theData <- list(forCumPrin = data.cumPrin.df, years = input$years, months = months, forPercentHomeOwned = data.percentHomeOwned.df, forMonthlyInt = data.monthlyInt.df, forCumInt = data.cumInt.df)
        
    })

    output$cumPrinOut <- renderPlotly({
        theData <- dataInput()
        p <- plot_ly(
            ## x = monthRange,
            ## y = ratioPVCFLoan,
            data = theData$forCumPrin,
            x = ~Months,
            y = ~Cumulative_Principal,
            xaxis = "Month",
            type = "scatter",
            mode = "lines",
            name = paste0("Cumulative Principal, ", theData$years, " Yr"), 
            text = paste0("Cumulative Principal: Month ", theData$months)
        ) %>%
            add_trace(y = ~Cumulative_Principal_30_Yr, name = "Cumulative Principal, 30 Yr") %>%
            add_trace(y = ~Cumulative_Principal_15_Yr, name = "Cumulative Principal, 15 Yr")
        p
    })

       

    
}


shinyApp(ui = ui, server = server)
