library(shiny)
library(shinyalert)
#link: https://www.kaggle.com/nikdavis/steam-store-games

# Read data
data = read.csv("data/clean_games.csv")
data['popularity'] = data['owners']*data['price']
data['year'] <- as.integer(format(as.Date(data$release_date,'%Y-%m-%d'),"%Y"))
data['active_years'] <- 2022 - data['year']
data['avg_annual_profit'] <- data['popularity']/data['active_years']
data$release_date <- NULL
library(maps)
library(mapproj)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggstream)

df_data=data.frame(data)
df_data=separate_rows(df_data, developer, sep=";")
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Steam games sales analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput("year",
                  label = "Games published between:",
                  min = 1997,
                  max = 2019,
                  value = c(1997,2019)),
      selectizeInput("developer",
                  label = "Developer:",
                  choices = NULL,
                  options = list(maxOptions = 30000)),
      radioButtons(inputId="age", label="Age >=", 
                   choices=c(0,16,18), selected=0)
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Popularity", plotOutput(outputId = "plot1")),
                  tabPanel("Revenue", plotOutput(outputId = "plot2")),
                  tabPanel("Developer Performance", plotOutput(outputId = "plot3")),
                  tabPanel("Debug Output", verbatimTextOutput("summary"))
      )
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  updateSelectizeInput(session, 'developer', choices = sort(unique(df_data$developer)), selected="Valve", server = TRUE)
  
#####PLOT 1###########
  response1 <- reactive({filter(df_data, between(year, input$year[1], input$year[2]))})

  output$plot1 <- renderPlot({
    #data['year'] <-  format(as.Date(data$release_date,'%Y-%m-%d'),"%Y")
    #data$release_date <- NULL
    data1=response1()

    separated=separate_rows(data1, genres, sep=";")
    final1 = separated %>%  group_by(year,genres) %>% summarise(owners=sum(owners))

    ggplot(final1, aes(x = year, y = owners, fill = genres)) +
      geom_stream()
  })
  ########PLOT 2#############
  ######## main #############
  output$plot2 <- renderPlot({
    
    plot2data = data.frame(data[, c('categories', 'genres', 'avg_annual_profit')])
    plot2data = separate_rows(plot2data, genres, sep=";")
    plot2data = separate_rows(plot2data, categories, sep=";")
    
    usable_data = plot2data %>%  
      group_by(genres, categories) %>% 
      summarise(profit=mean(avg_annual_profit)) %>% 
      spread(genres, profit) %>% 
      ungroup()
    usable_data[is.na(usable_data)] <- 0
    viewable_data <- usable_data %>%
      gather(colname, value, -categories)
    ggplot(viewable_data, aes(x = colname, y = categories, fill = value)) +
      geom_tile() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      labs(x = "Genres", y="Categories", value="Mean Annual Revenue")
  })
  
  
  
  ########linked#############
  
  
  
  ########PLOT 3#############

  response3 <- reactive({df_data %>% filter(developer == input$developer) %>%
      filter(required_age >= input$age)
      })

  output$plot3 <- renderPlot({
    data3=response3()
    if(nrow(data3) == 0){
      shinyalert("Oops!", "No games with that age requirement from the chosen developer", type = "error")
    }
    else{
      data3$price[data3$price == 0.00] <- 1.00
      data3['revenue'] = data3$price * data3$owners

      separated=separate_rows(data3, genres, sep=";")
      dev_genre = separated %>%  group_by(genres) %>% summarise(revenue=sum(revenue))
      ggplot(dev_genre, aes(x=genres, y=revenue )) +
        geom_bar(stat='identity', width=0.5 ) +
        # geom_text(aes(label=revenue), vjust=1.6,color="white", size=3.5)+
        scale_color_brewer(palette = "Dark2")
    }
  })
  ##DEBUG OUTPUT - REMOVE WHEN DONE
  output$summary <- renderText({
    paste("Current Selections:\n", 
          "Years: ", input$year[1], "and", input$year[2], "\n",
          "Developer: ", input$developer, "\n",
          "Age: ", input$age)
  })
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)