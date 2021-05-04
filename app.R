#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(glue)
library(xts, lib.loc="/home/giovanni/R/x86_64-pc-linux-gnu-library/3.5")
library(dygraphs, lib.loc="/home/giovanni/R/x86_64-pc-linux-gnu-library/3.5")
source("R/extract_ts.R")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(selectInput("year", label = NULL, 
                             choices = 2016:2020, 
                             selected = 2020),
                 selectInput("poll", label = NULL, 
                             choices = list("PM10 daily average"="PM10",
                                            "NO2 daily maximum"="NO2",
                                            "O3 daily maximum of 8 hrs running mean"="O3"), 
                             selected = "PM10"),
                 actionButton("get", label = "plot"),
                 downloadButton('downloadData', label = 'download')
    ),
    mainPanel(leafletOutput("mymap"),
              dygraphOutput("plot1")
    )
  )
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(lng = 13,  lat = 46,  zoom = 8)
  })
  
  observeEvent(input$mymap_click, {
    click <- input$mymap_click
    if(is.null(click))
      return()
    text<-paste0("latitude: ", round(click$lat,4), ", longitude: ", round(click$lng,4))
    mymap_proxy <- leafletProxy("mymap") %>%
      clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
  
  dataset <- reactive({
    click <- input$mymap_click
    if(is.null(click))
      return()
    xy <- convert_coords(click$lng, click$lat)
    dd <- extr_ts(filein = glue("data/{input$poll}_{input$year}0101-{input$year}1231.nc"), 
                  xx = xy$x, yy = xy$y)
    dd
  })
  
  observeEvent(input$get, {
    dd <-dataset() 
    library(xts)
    data <- xts(dd[,"Value"], order.by=dd[,"Day"])
    print(str(data))
    output$plot1 <- renderDygraph({
      dygraph(data, main = input$poll) %>%
        dyOptions(drawGrid = input$showgrid, fillGraph = TRUE, fillAlpha = 0.4, drawPoints = TRUE) %>%
        dyRangeSelector()
    })
  })
  
  fileout <- reactive({
    click <- input$mymap_click
    if(is.null(click))
      return()
    library(glue)
    ff <- glue("data/{input$poll}_{input$year}0101-{input$year}1231_{round(click$lng,4)}_{round(click$lat,4)}.csv")
    ff
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      fileout()
    },
    content = function(con) {
      dd <-dataset()
      write.csv(dd, con, row.names = F)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

