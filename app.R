library(shiny)

#link: https://www.kaggle.com/nikdavis/steam-store-games

# Read data
data = read.csv("data/clean_games.csv")
data['popularity'] = data['owners']*data['price']
data['year'] <-  format(as.Date(data$release_date,'%Y-%m-%d'),"%Y")
data$release_date <- NULL
library(maps)
library(mapproj)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggstream)

df_data=data.frame(data)
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
                  label = "Number of bins:",
                  min = 1997,
                  max = 2019,
                  value = c(1997,2019)),
      selectInput("developer",
                  label = "Developer:",
                  choices = unique(df_data$developer),
                  selected = 'Valve')
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      #plotOutput(outputId = "plot1")
      plotOutput(outputId = "plot3")
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
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
  
  ########PLOT 3#############
  response3 <- reactive({filter(df_data, df_data$developer == input$developer)})
  output$plot3 <- renderPlot({
    data3=response3()
    data3$price[data3$price == 0.00] <- 1.00
    data3['revenue'] = data3$price * data3$owners
    
    separated=separate_rows(data3, genres, sep=";")
    dev_genre = separated %>%  group_by(genres) %>% summarise(revenue=sum(revenue))
    ggplot(dev_genre, aes(x=as.factor(genres),y=as.factor(revenue) )) + 
      geom_bar( ) +
      scale_fill_brewer(palette = "Set1") +
      theme(legend.position="none")
  })
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)