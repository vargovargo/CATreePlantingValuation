#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT) 

options(DT.options = list(pageLength = 21, language = list(search = 'Filter:')))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("California Tree Planting Valuation Dashboard"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("investment",
                     "Millions of dollars invested",
                     min = 0,
                     max = 100,
                     value = 50),
         sliderInput("socDisRate",
                     "Social Discount Rate",
                     min = 0,
                     max = 10,
                     value = 3.5,step = 0.5),
         sliderInput("carDisRate",
                     "Carbon Discount Rate",
                     min = 0,
                     max = 10,
                     value = 0, step = 0.5)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(type = "tabs",
                     tabPanel(title = "Introduction",  includeHTML("intro.html")),
                     tabPanel(title = "Bang Per Buck",  dataTableOutput('periodDT'), dataTableOutput('carbonDT')),
                     tabPanel(title = "Carbon Benefits", plotOutput('carbBene'), dataTableOutput('carbBeneDT')),
                     tabPanel(title = "Net $ Benefits", plotOutput('dollarBene'), dataTableOutput('dollarBeneDT'))
                     )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  conversion2015 <- reactive({
    read.csv("raw.csv", header=T) %>%
        gather(5:44, key = "Year", value = "value") %>%
        mutate(Year = as.numeric(substring(Year, first=5))) 
  })
  
  discountTable <- reactive({
    inner_join({
        filter({
          conversion2015() %>%
            spread(key = Metric, value = value)
        } , Units == "dollars") %>%
          select(Region, Size, Year, benefitsPerTree, costsPerTree)
      },
      {
        filter({
          conversion2015() %>%
            spread(key = Metric, value = value)
        }, Units == "pounds") %>%
          select(Region, Size, Year,carbonSequestration)
      })  %>%
      mutate(`Net Benefit per Tree per Year` = (benefitsPerTree - costsPerTree)*(1/(1+(input$socDisRate/100))^(Year-1)),
             `Carbon per Tree per Year` = (carbonSequestration)*(1/(1+(input$carDisRate/100))^(Year-1)))
    
  })
  
  
  
  
  plotData <- reactive({
    
    Regions <- factor(c("Southwest desert", "Inland Empire","Northern California Coast","Southern California Coast","Temperate Interior West",  "Inland Valley (San Juaquin Valley)"),
                      levels=c("Southwest desert", 
                               "Inland Empire",
                               "Northern California Coast",
                               "Southern California Coast",
                               "Temperate Interior West", 
                               "Inland Valley (San Juaquin Valley)"))
    
    Sizes <- factor(c("Small", "Med", "Large","Conifer"), levels=c("Small", "Med", "Large","Conifer"))
    
    temp <- conversion2015() %>%
      filter(Metric == "costsPerTree") %>%
      group_by(Region, Size) %>%
      summarize(TotalCosts = sum(value),
                `Number of Trees` = round((1000000*input$investment)/sum(value),0)) %>%
      select(Region, Size, `Number of Trees`) %>%
      left_join(discountTable())
    
    plotDataTemp <- data.frame()
    
    for(region in Regions){
      for(size in Sizes){
        
        new <- temp %>% filter(Region == region & Size == size) %>%
          mutate(carbonChart = cumsum(`Carbon per Tree per Year`)*0.000453592*`Number of Trees`,
                 dollarsChart = cumsum(`Net Benefit per Tree per Year`)*`Number of Trees`)
        
        # set the name of my new variable and bind the new data to it
        if(exists("plotDataTemp")){
          plotDataTemp <- bind_rows(plotDataTemp, new)
        }else{
          plotDataTemp <- new
        }
      }
    }
    
    return(plotDataTemp)
    
  })
  
  
  
    
  output$periodDT <- renderDataTable({
      
   discount40 <- discountTable() %>%
     group_by(Region, Size) %>%
     summarise(`Cummulative Net Benefit per Tree per Year` = sum(`Net Benefit per Tree per Year`),
               `Cummulative Carbon per Tree per Year` = sum(`Carbon per Tree per Year`))
   
   conversion2015() %>%
     filter(Metric == "costsPerTree") %>%
     group_by(Region, Size) %>%
     summarize(TotalCosts = sum(value),
               `Number of Trees` = round((1000000*input$investment)/sum(value),0)) %>% 
     inner_join(discount40) %>%
     mutate(`Tons of Carbon/$1,000` = round(1000*(`Cummulative Carbon per Tree per Year`*0.000453592)/TotalCosts,3)) %>%
     select(Region, Size, `Number of Trees`, `Tons of Carbon/$1,000`)
  
  })
  
  
  output$carbBene <- renderPlot({
    
    ggplot(plotData(),aes(x=Year, y=carbonChart/1000, color=Size)) + geom_line(size=2) + facet_wrap(~ Region)  +
      ylab(label = "Carbon Sequestered (1,000 Metric Tons)") + xlab("Years After Investment")
  
  })
  
  
  output$carbBeneDT <- renderDataTable({

    plotData() %>%
      filter(Year %in% c(5, 15, 40)) %>%
      select(Region, Size, Year, carbonChart) %>%
      mutate(carbonChart = round(carbonChart,1)) %>%
      spread(key = Year, value = carbonChart)

  })
   
  output$dollarBene <- renderPlot({ 
    
    plotData() %>% ggplot(aes(x=Year, y=dollarsChart/1000000, color=Size)) + geom_line(size=2) + geom_hline(yintercept = 0, size=1, alpha=0.5) + facet_wrap(~Region) +
      ylab(label = "Net Benefits (Millions $)") + xlab("Years After Investment")
  
  })
  
  output$dollarBeneDT <- renderDataTable({
    
    plotData() %>%
      filter(Year %in% c(5, 15, 40)) %>%
      select(Region, Size, Year, dollarsChart) %>%
      mutate(dollarsChart = round(dollarsChart,0)) %>%
      spread(key = Year, value = dollarsChart)
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

