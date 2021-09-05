library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(sp)
ui <- fluidPage(
  
  titlePanel("Rocio 19160169"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Ingresar CSV Aqui",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      tags$hr(),
      
      checkboxInput("header", "Cabecera", TRUE),
      
      radioButtons("sep", "Separador",
                   choices = c(Coma = ",",
                               puntoyComa = ";",
                               Tab = "\t",
                               espacio=''),
                   selected = ","),
      
      radioButtons("quote", "Cita",
                   choices = c(Ninguna = "",
                               "cita doble" = '"',
                               "cita simple" = "'"),
                   selected = '"'),
      
      tags$hr(),
      
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
    ),
    
    mainPanel(
      tableOutput("contents"),
      leafletOutput("map")
      
    )
  )
)

server <- function(input, output) {
  output$contents <- renderTable({
    req(input$file1)
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        stop(safeError(e))
      }
      
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  output$map<- renderLeaflet(({
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    mapa<-leaflet() %>% 
      addCircles(data = df, lat = df$Latitude, lng = df$Longitude)%>%
      addTiles()
  }))
  
}




# Run the application 
shinyApp(ui = ui, server = server)
