library(tidyverse)
library(here)
library(shiny)

# version: 2025-04-14

# Esto viene de un tiempo anterior Abril 2023
# Eliminar el código e intentar hacer algo similar al report

# Carga de datos  ---------------------------------------------------------

load(here("data/processed/datos_coop_liq.RData"))
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

server <- function(input, ouput) {
  output$selected_categoria <- renderText({
    paste("La selección es", input$categoria)
  })
}


# Ejecución  --------------------------------------------------------------

shinyApp(ui = ui, server = server)
