#  meat consumption by country overtime


# import libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)


# Read data
meat <- read.csv("C:/Users/alecoo/OneDrive - Karolinska Institutet/Desktop/Portfolio/public data/meat_consumption.csv",
                 header = T,
                 sep = ",",
                 stringsAsFactors = F)




# change subject, measure, and location to factor variables
meat <- meat %>% mutate(location = as.factor(location), 
                              subject = as.factor(subject),
                        measure = as.factor(measure),
                        subject = fct_collapse(subject,  
                                               beef = c("BEEF"),
                                               pig = c("PIG"),
                                               sheep = c("SHEEP"),
                                               poultry = c("POULTRY")))
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title
  titlePanel("Meat consumption by Country"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel( 
      
      HTML("<h3>Compare variables</h3>"),
      # Input values
      sliderInput("time", "Year: ",
                  min = 1990,
                  max = 2023,
                  value = 1990,
                  step = 1,
                  animate = animationOptions(interval = 1000, loop = T)),
      selectInput("subject", "Meat type:",
                  choices = list("Beef meat" = "beef", "Pork meat" = "pig", 
                                 "Sheep meat" = "sheep", "Poultry meat" = "poultry"),
                  selected = "Beef"),
      selectInput("measure", "Measurement", 
                  choices = list("Kilograms/capita" = "KG_CAP", 
                                 "Thousand Tonnes" = "THND_TONNE"),
                  selected = "Kilograms/capita")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Barplot ----
      plotOutput(outputId = "plot")
      
    )
  )
)

# Define server logic for Shiny app
server <- function(input, output) {
  # Render the plot based on the selected inputs
  output$plot <- renderPlot({
    # Filter the data based on the selected inputs
    filtered_data <-  meat %>%
      filter(time == input$time, subject == input$subject, measure == input$measure)
    
    # Generate ggplot object for the filtered data
    plot_obj <- ggplot(filtered_data, aes(location, value)) +
      geom_bar(stat = "summary", fun = "median") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    # Render the ggplot object
    print(plot_obj)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)