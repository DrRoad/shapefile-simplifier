library(shiny)
library(shinythemes)
library(shinycssloaders)
library(rgeos)
library(rgdal)
library(leaflet)
library(shinyjs)

options(shiny.maxRequestSize=30*1024^2)

## Set images resource path
addResourcePath("images", "images")

ui <- fluidPage(theme = shinytheme("cerulean"),
                
   includeCSS("css/styles.css"),
   
   titlePanel("Shapefile Simplification and Approximation Tool"),
   
   sidebarLayout(
      sidebarPanel(
          useShinyjs(),
          
          a(href = "https://oaiti.org", target = "_blank", img(src = "images/oaiti_transparent.png", width = "135")),
          h4("About"),
          HTML("This application allows users to upload a shapefile (as a .zip) and perform an approximation algorithm in order to download a simplified version of the polygons."),
          hr(),
          
          h4("Configuration"),
          fileInput("shp", "Shape File", accept = ".zip"),
          sliderInput("tol", "Tolerance", min = 0.005, step = 0.005, value = 0.02, max = 0.1),
          
          h4("Export"),
          downloadButton("download", "Download Simplified Shapefiles")
      ),
      
      mainPanel(
          h4("Original Map"),
          withSpinner(leafletOutput("map", height = "300px")),
          textOutput("size"),
          
          hr(),
          
          h4("Simple Map"),
          withSpinner(leafletOutput("simple_map", height = "300px")),
          textOutput("simple_size")
      )
   )
)

server <- function(input, output, session) {
    mytmpdir <- tempdir()
    
    observeEvent(input$shp, {
        unzip(input$shp$datapath, exdir = mytmpdir)
    })
    
    myogr_path <- reactive({
        rel_path <- file.path(mytmpdir, gsub(".zip$", "", input$shp$name))
        if (length(dir(rel_path)) == 0) rel_path <- file.path(mytmpdir)
        
        return(rel_path)
    })

    myogr <- reactive({
        if (is.null(input$shp)) return(NULL)
        
        readOGR(dsn = myogr_path(), stringsAsFactors = FALSE)
    })
    
    simplified <- reactive({
        shinyjs::disable("download")
        
        if (is.null(myogr())) return(NULL)
        
        simple_ogr <- gSimplify(myogr(), tol = input$tol, topologyPreserve = TRUE)
        
        shinyjs::enable("download")
        
        return(SpatialPolygonsDataFrame(simple_ogr, data = myogr()@data))
    })

    output$map <- renderLeaflet({
        if (is.null(myogr())) return(NULL)
        
        leaflet(myogr()) %>%
            addTiles() %>%
            addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = ~colorQuantile("YlOrRd", Shape_Area)(Shape_Area),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE))
    })
    
    output$size <- renderText({
        return(paste0("Original Size: ", format(object.size(myogr()), units = "Mb")))
    })
    
    output$simple_map <- renderLeaflet({
        if (is.null(simplified())) return(NULL)
        
        leaflet(simplified()) %>%
            addTiles() %>%
            addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                        opacity = 1.0, fillOpacity = 0.5,
                        fillColor = ~colorQuantile("YlOrRd", Shape_Area)(Shape_Area),
                        highlightOptions = highlightOptions(color = "white", weight = 2,
                                                            bringToFront = TRUE))
    })
    
    output$simple_size <- renderText({
        return(paste0("Simplified Size: ", format(object.size(simplified()), units = "Mb")))
    })
    
    mylayer <- reactive({
        return(gsub(".shp", "", dir(myogr_path())[grep(".shp$", dir(myogr_path()))]))
    })
    
    output$download <- downloadHandler(
        filename = function() { paste0(mylayer(), "SIMPLIFIED.zip") },
        content = function(fname) {
            newtmpdir <- file.path(mytmpdir, tempfile())
            dir.create(newtmpdir, recursive = TRUE)
            
            writeOGR(simplified(), newtmpdir, driver = "ESRI Shapefile", layer = paste0(mylayer(), "SIMPLIFIED"))
            
            full_files <- file.path(newtmpdir, dir(newtmpdir)[grep(paste0(mylayer(), "SIMPLIFIED"), dir(newtmpdir))])
            
            zip(zipfile = fname, files = full_files, flags = "-9Xj")
        },
        contentType = "application/zip"
    )
}

shinyApp(ui = ui, server = server)
