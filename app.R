if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("InteractiveComplexHeatmap")
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
library(ComplexHeatmap)
library(grid)

df_data=data.frame(data)
df_data=separate_rows(df_data, developer, sep=";")

## Data for the interactive heatmap
plot2data = data.frame(data[, c('categories', 'genres', 'avg_annual_profit')])
plot2data = separate_rows(plot2data, genres, sep=";")
plot2data = separate_rows(plot2data, categories, sep=";")

static_plot2_data = plot2data %>%  
  group_by(genres, categories) %>% 
  summarise(profit=mean(avg_annual_profit)) %>% 
  spread(genres, profit) %>% 
  ungroup() %>%
  remove_rownames() %>% 
  column_to_rownames(var="categories")
static_plot2_data[is.na(static_plot2_data)] <- 0
# The way that the clustering algorithm arguments are passed to 
# the Interactive Complex Heatmap is opposite of the default pplot heatmap. 
# TRUE indicates that the sorting is done based on the data, not the column/row names, unlike the original heatmap.
ht = Heatmap(as.matrix(static_plot2_data), cluster_rows = TRUE, cluster_columns = TRUE)
ht = draw(ht)

## Prepare subplot 2 data
subplot2data = data.frame(data[, c('categories', 'genres', 'avg_annual_profit', 'year')])
subplot2data = separate_rows(subplot2data, genres, sep=";")
subplot2data = separate_rows(subplot2data, categories, sep=";")

# Define UI for app that draws the plots ----
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
                  tabPanel("Revenue", 
                           InteractiveComplexHeatmapOutput(
                             response = "click", 
                             width1 = 600, 
                             height1 = 600,
                             output_ui = plotOutput("subplot", width = 400, height = 400))),
                  tabPanel("Developer Performance", plotOutput(outputId = "plot3"))
      )
    )
  )
)
# ## Interactive heatmap click action
click_action = function(df, output) {
  output$subplot = renderPlot({
    if(is.null(df)) {
      grid.text("Click on a heatmap cell to visualize this plot \nfor the selected row-column details.")
    } else {
      i1 = df$column_index
      i2 = df$row_index
      col = colnames(static_plot2_data)[i1]
      row = rownames(static_plot2_data)[i2]
      print(col)
      print(row)
      
      filteredData = filter(subplot2data, genres == col & categories == row)
      filteredData = filteredData[, c(3,4)]
      filteredData = filteredData %>%  
        group_by(year) %>% 
        summarise(profit=mean(avg_annual_profit)) %>% 
        spread(year, profit) %>% 
        ungroup()
      if(nrow(filteredData)*ncol(filteredData) == 0) {
        grid.text("Insufficient data for the selected combination")
      } else {
        
        filteredData = as.data.frame(filteredData)
        filteredData = t(filteredData)
        xValue <- rownames(filteredData)
        yValue <- filteredData[,1]
  
        plot(xValue,yValue, type="l", col="red", lwd=5, xlab="year", ylab="Average annual profit")
      }
      
    }
  })
}
# Define server logic required to draw the plots
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

  
  makeInteractiveComplexHeatmap(input, output, session, ht,
                                click_action = click_action)
  
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
}
# Create Shiny app ----
shinyApp(ui = ui, server = server)