library(tidyverse)
library(here)
library(shiny)


# Carga de datos  ---------------------------------------------------------

# load(file = here("data/processed/datos_finca.RData"))
# cats <- c("Premium", "P Sup", "Segunda")

# UI  ---------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Una historia de plátanos"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput("categoria",
                  label    = "Selecciona una categoría:",
                  choices  = c("Premium", "P Sup", "Segunda"),
                  selected = "Premium"
      )
    ),
    
    mainPanel(
      textOutput("selected_categoria")
    )
  )
)


# Servidor  ---------------------------------------------------------------

server <- function(input, ouput){
  
  output$selected_categoria <- renderText({
      paste("La selección es", input$categoria)
      })
}


# Ejecución  --------------------------------------------------------------

shinyApp(ui = ui, server = server)
