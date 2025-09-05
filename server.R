library(dplyr)
library(tidyr)
library(leaflet)
library(DT)
library(sf)

boroughs <- st_read("Data/statistical-gis-boundaries-london/London_Borough_Excluding_MHW.shp")

# Use standard data frame subsetting instead of @data
boroughs <- boroughs[, c("NAME", "GSS_CODE")]

# Transform CRS with st_transform
boroughs <- st_transform(boroughs, 4326) # Use EPSG code 4326 for WGS84

# Get bounding box with st_bbox
bounds <- st_bbox(boroughs)

income_long <- read.csv("Data/income_long.csv")

function(input, output, session) {
  getDataSet <- reactive({
    dataSet <- income_long[income_long$Year == input$dataYear & income_long$Measure == input$meas,]
    joinedDataset <- boroughs
    
    # Use dplyr::left_join directly on the sf object
    joinedDataset <- suppressWarnings(left_join(joinedDataset, dataSet, by = "NAME"))
    
    if(input$city == FALSE) {
      # Use standard data frame indexing
      joinedDataset$Income[joinedDataset$NAME == "City of London"] <- NA
    }
    joinedDataset
  })
  
  output$londonMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(mean(bounds[c("xmin", "xmax")]),
              mean(bounds[c("ymin", "ymax")]),
              zoom = 10)
  })
  
  observe({
    theData <- getDataSet()
    pal <- colorQuantile("magma", theData$Income, n = 10)
    
    borough_popup <- paste0("<strong>Borough: </strong>", 
                            theData$NAME, 
                            "<br><strong>",
                            input$meas, " income: </strong>£", 
                            formatC(theData$Income, format = "d", big.mark = ','))
    
    leafletProxy("londonMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData$Income), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 2,
                  popup = borough_popup)
  })
  
  output$boroughTable <- renderDataTable(datatable({
    dataSet <- getDataSet()
    # Convert sf object to a data frame for the table
    dataSet <- as.data.frame(dataSet) 
    dataSet <- dataSet[, c("NAME", "Income")]
    names(dataSet) <- c("Borough", paste0(input$meas, "income"))
    dataSet
  },
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5))
  )
  
  output$yearSelect <- renderUI({
    yearRange <- sort(unique(as.numeric(income_long$Year)), decreasing = TRUE)
    selectInput("dataYear", "Year", choices = yearRange, selected = yearRange[1])
  })
}