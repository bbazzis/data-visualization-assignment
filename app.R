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

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Steam games sales analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "year",
                  label = "Number of bins:",
                  min = 1977,
                  max = 2019,
                  value = c(1077,2019))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "plot1")
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
#####PLOT 1###########
  response1 <- reactive({filter(data, between(year, input$year[1], input$year[2]))})
  
  output$plot1 <- renderPlot({
    #data['year'] <-  format(as.Date(data$release_date,'%Y-%m-%d'),"%Y")
    #data$release_date <- NULL
    data1=response1()
    genre_per_year = data1 %>%  group_by(year, genres, owners) %>% summarise(total=n())

    separated=separate_rows(genre_per_year, genres, sep=";")
    good = separated %>%  group_by(year, genres, owners) %>% summarise(total=sum(total))
    final = good %>%  group_by(year, genres) %>% summarise(owners=sum(owners))
    
    ggplot(final, aes(x = year, y = owners, fill = genres)) +
      geom_stream()
  })
  
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)