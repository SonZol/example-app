options(shiny.maxRequestSize = 30*1024^2) # increase the file size to 30 Mb

library(shiny)
# install.packages("here")
library(here)
library(shinyWidgets)
#install.packages("shinythemes")
library(shinythemes)
library(ggplot2)
# 
# 
europe <- readRDS("~/Documents/UniLu/HS 2020 LUMACSS/Campus Luzern/Shiny/App2/data/europe.rds")%>% 
    mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius

# 
# dim(europe)
# length(unique(europe$Country))
# length(unique(europe$City))
# range(europe$Date)
# range(europe$AvgTemperatureF)
# is.na(europe)

source('~/Documents/UniLu/HS 2020 LUMACSS/Campus Luzern/Shiny/App2/prep.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
    #specify the selected theme to the 'theme' argument
    theme = shinytheme("flatly"),
    
    # Application title
    titlePanel("Storm Shiny App"),
    
    # App description
    "This app shows the average daily temperatures (in Fahrenheit) from cities around Europe from 2000 to 2019",
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            " This is the sidebar Panel",
            
            # Input: A simple sliderInput ----
            sliderInput(inputId = "Year", label = "Year",
                        min = 2000,
                        max = 2019,
                        value = 2000,
                        sep = ""),
            
    
            
            # Input: A simple selectInput ----
            #selectInput(inputId = "Country", label = "Drop down list:",
            #           choices = c(unique(europe$Country)), multiple = TRUE),
            
            selectInput(inputId = "Country", label = "Country",
                                 choices = ""),
                        
            # Input: A simple selectInput ----
            #selectInput(inputId = "City", label = "Drop down list:",
            #           choices = c(unique(sort(europe$City))), multiple = TRUE),
            selectInput(inputId = "City", label = "City",
                        choices = ""),
            
            # Input: A simple textInput ----
            textInput(inputId = "text_input", label = "Input text here:", placeholder = "insert here"),
            
            # Input: A simple radioButton ----
            radioButtons(inputId = "Temperature", label ="Make a choice",
                         choices = list("Celcius" = "C", "Fahrenheit" = "F"),
                         selected = "C"),
            
            # Input: Action button that subsets storm data  ----
            actionButton(inputId = "button", label = "GO!"),
            
            fileInput(inputId = "file", label = "upload a file (RDS)",
                      multiple = FALSE,
                      accept = c(".rds")),
            downloadButton(outputId = "download", label = "Download")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            strong(" This is the main Panel"),
            textOutput(outputId = "text_output"),
            
            # Layout: Tabset with info, data and plots tab ----
            tabsetPanel(type = "tabs",
                        tabPanel(title = "Info",
                                 h3("App description"),
                                 p("This is the Europe shiny app. It's been created during the Shiny Workshop by Nicolas Attalides.
                                 It's based on a dataset about the temperatures in different Cities around Europe from 2000 till 2019",
                                 verbatimTextOutput("data_summary"))),
                        tabPanel(title = "Data",
                                 "This table shows the average Temperature in Celsius filtered by Country, City and Month",
                                 dataTableOutput("data_table")),
                        tabPanel(title = "Plots",
                                 plotOutput("barplot"),
                                 plotOutput("lineplot"),
                                 plotOutput("lineplot_f"),
                                 plotOutput("lineplot_c")))
            
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    europe <- eventReactive(input$file, {
        readRDS(input$file$datapath) %>% 
        mutate(AvgTemperatureC = round((AvgTemperatureF - 32)*5/9, 1)) # Create a new column with Avg Temperature in Celsius
        
    })
    
    country_df <- reactive({
        europe() %>%
            filter(Year >= input$Year) %>% # Subset the rows to keep data more than or equal to a year
            filter(Country %in% input$Country) # Subset the rows to keep a specific country
    })
    
    city_df <- reactive({
        country_df() %>% 
            filter(Country %in% input$Country) %>% # Subset the rows for specific City
            filter(Year == input$Year) # Subset the rows for specific Year
    })
    
    year_df <- eventReactive(input$button, {
        country_df() %>% 
            filter(City %in% input$City) %>% # Subset the rows for specific City
            filter(Year == input$Year) %>%  # Subset the rows for specific Year
            group_by(Country, City, Year, Month) %>% 
            summarise(MinTempF = min(AvgTemperatureF),
                      MeanTempF = round(mean(AvgTemperatureF), 1),
                      MaxTempF = max(AvgTemperatureF),
                      MinTempC = min(AvgTemperatureC),
                      MeanTempC = round(mean(AvgTemperatureC), 1),
                      MaxTempC = max(AvgTemperatureC)) %>% 
            ungroup()
    })
    
    # Output: Render a text output ----
    output$text_output <- renderText({
        paste("Your inputs are:", input$Year, input$Country, input$City, input$text_input,input$Temperature)
    })
    
    # Output: Render a barplot output ----
    output$barplot <- renderPlot({
        ggplot(europe_avgYear_CH, aes(Year,  y = Temp_Grad_mean, fill = City)) +
            geom_bar(stat = "identity", position = "dodge") +
            scale_fill_brewer(palette="Spectral") +
            ylab("Average Temperature") +
            ggtitle("Average Temp per Year and City") +
            theme_tufte() +
            facet_wrap(~City)
    })
    
    # Output: Render a lineplot output ----
    output$lineplot <- renderPlot({
        ggplot(CH_365, aes(day_365, y = Temp_Grad, group = as.character(Year), color = as.character(Year))) +
            geom_line() +
            scale_fill_brewer(palette="Spectral") +
            ylab("Average Temperature") +
            xlab("Days in a Year") +
            ggtitle("Average Temp per Day per City in Switzerland") +
            scale_colour_discrete(name = "Year") +
            theme_tufte() +
            facet_wrap(~City)
    })
    
    observe({
        new_choices <- unique(europe()$Country)
        
        updateSelectInput(session, inputId = "Country", choices = new_choices)
    })
    
    observe({
        new_choices <- unique(europe()$City[europe()$Country == input$Country])
    
        updateSelectInput(session, inputId = "City", choices = new_choices)
    })
    
    output$download <- downloadHandler(
        filename = "europe_data.csv",
        content = function(file) {
            write.csv2(europe(), file, row.names = FALSE)
        }
    )
    

    # Output: Render a lineplot output ----
    output$lineplot_f <- renderPlot({
        ggplot(data = year_df()) +
            geom_line(mapping = aes(x = Month, y = MinTempF), size = 1, colour = "red", linetype = "dotted") +
            geom_line(mapping = aes(x = Month, y = MeanTempF), size = 1, colour = "black") +
            geom_line(mapping = aes(x = Month, y = MaxTempF), size = 1, colour = "red", linetype = "dotted") +
            scale_x_discrete(name = "", limits = month.abb) +
            ylab("Average daily temperatures (in Fahrenheit)")
    })
    
    # Output: Render a lineplot output ----
    output$lineplot_c <- renderPlot({
        ggplot(data = year_df()) +
            geom_line(mapping = aes(x = Month, y = MinTempC), size = 1, colour = "red", linetype = "dotted") +
            geom_line(mapping = aes(x = Month, y = MeanTempC), size = 1, colour = "black") +
            geom_line(mapping = aes(x = Month, y = MaxTempC), size = 1, colour = "red", linetype = "dotted") +
            scale_x_discrete(name = "", limits = month.abb) +
            ylab("Average daily temperatures (in Celsius)")
    })
    
    # Output: Render a print output ----
    output$data_summary <- renderPrint({
        summary(europe())
    })
    
    
    # Output: Render a (dynamic) table output ----
    output$data_table <- renderDataTable({
        europe_df
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
