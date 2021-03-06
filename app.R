source("R/configure.R")
library(shiny)
library(leaflet)
library(leaflet.extras)
library(glue)
library(xts)
library(dygraphs)
library(shinyBS)
source("R/extract_ts.R")

ui <- fluidPage(
  
  column(width = 12,
    helpText(
      "La migliore stima della qualità dell'aria su ogni punto del Friuli - Venezia Giulia viene realizzata integrando simulazioni e misure con tecniche geostatistiche.",
      "Le elaborazioni sono realizzate quotidianamente con una risoluzione di 2 km."
    ),
    selectInput("year", label = NULL, 
                choices = 2016:2020, 
                selected = 2018),
    bsTooltip("year", "scegli l\\'anno che ti interessa", placement = "right"),
    selectInput(inputId = "poll", label = NULL, 
                choices = list("PM10: media giornaliera"="PM10",
                               "NO2: massimo giornaliero"="NO2",
                               "O3: massimo giornaliero della media mobile su 8 ore"="O3"), 
                selected = "PM10"),
    bsTooltip("poll", "scegli un indicatore di qualità dell\\'aria", placement = "right")
  ),
  column(width = 12,
    leafletOutput("mymap"),
    bsTooltip("mymap", "seleziona un punto del Friuli Venezia Giulia cliccando sulla mappa (puoi cercare una località cliccando sulla lente in alto a sinistra)", placement = "bottom"),
    hr(),
    actionButton("get", label = "visualizza"),
    bsTooltip("get", "visualizza la serie temporale di un punto, selezionato cliccando sulla mappa", placement = "right"),
    dygraphOutput("plot1"),
    bsTooltip("plot1", "il grafico è interattivo: trascinando il mouse puoi visualizzare un sotto-periodo", placement = "top"),
    hr(),
    downloadButton('downloadData', label = 'scarica i dati'),
    bsTooltip("downloadData", "scarica la serie temporale in formato CSV", placement = "right"),
    hr()
  ),
  column(width = 12,
         HTML(
           "<div class='alert alert-info'>",
           "<strong>Attenzione!</strong> Gli indicatori si riferiscono alle concentrazioni di fondo.",
           "In prossimità di importanti sorgenti emissive (strade, impianti industriali, porti, edifici, ...) o in",
           "condizioni favorevoli ad accumuli localizzati (fondovalle, strade e cortili chiusi tra edifici)",
           "le concentrazioni potrebbero essere piuttosto diverse.",
           "Le elaborazioni sono realizzate nella mattinata per garantire informazione tempestiva,",
           "perciò a volte si basano su dati misurati non ancora controllati, alcuni dei quali potrebbero essere successivamente invalidati.",
           "Per l'accesso ai dati validati delle stazioni di monitoraggio, si faccia riferimento a ",
           "<a href='http://www.arpaweb.fvg.it/qagis/ariastor.asp' class='alert-link'>questa pagina</a>.",
           "</div>"
         )
  )
  
)

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      setView(lng = 13,  lat = 46,  zoom = 8) %>%
      addSearchOSM(options = searchOptions(autoCollapse = FALSE, minLength = 2))
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
    if(is.null(click)) {
      showModal(modalDialog(
        "Scegli un punto cliccando sulla mappa.",
        easyClose = TRUE,size = "s",footer = modalButton("OK")
      ))
      return(data.frame(Day=Sys.Date(),Value=NA))
    }
    xy <- convert_coords(click$lng, click$lat)
    dd <- extr_ts(filein = glue("data/{input$poll}_{input$year}0101-{input$year}1231.nc"), 
                  xx = xy$x, yy = xy$y)
    if(is.null(dd)) {
      showModal(modalDialog(
        "Nessun dato per questo punto. Scegli un punto in Friuli Venezia Giulia.",
        easyClose = TRUE,size = "s",footer = modalButton("OK")
      ))
      return(data.frame(Day=Sys.Date(),Value=NA))
    } else {
      return(dd)
    }
  })
  
  observeEvent(input$get, {
    dd <-dataset() 
    library(xts)
    data <- xts(dd[,"Value"], order.by=dd[,"Day"])
    print(str(data))
    output$plot1 <- renderDygraph({
      dygraph(data, main = glue("{input$poll} ({dd[1,'Unit']})")) %>%
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

