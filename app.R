library(tidyverse)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(raster)
library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(mapview)
library(scales)
library(plotly)
library(DT)
library(lubridate)

load("data.RData")

shinyApp(
  
  ui = shinydashboardPlus::dashboardPage(
    options = list(sidebarExpandOnHover = FALSE),
    header = shinydashboardPlus::dashboardHeader(titleWidth = 0, disable = TRUE),
    
    sidebar = shinydashboardPlus::dashboardSidebar(width = "0px"),
    
    body = shinydashboard::dashboardBody(
      
      tags$head(
        tags$style(HTML('/* logo */
                         .skin-blue .main-header .logo {
                         background-color: #23769a;
                         }
                         /* logo when hovered */
                         .skin-blue .main-header .logo:hover {
                         background-color: #23769a;
                         }
                         /* navbar (rest of the header) */
                         .skin-blue .main-header .navbar {
                         background-color: #23769a;
                         }
                         /* main sidebar */
                         .skin-blue .main-sidebar {
                         background-color: #23769a;
                         }
                         .main-sidebar {
                         font-size: 20px;
                         }
                         /* active selected tab in the sidebarmenu */
                         .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                         background-color: #23769a;
                         }
                         /* other links in the sidebarmenu */
                         .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                         background-color: #23769a;
                         color: white;
                         }
                         /* other links in the sidebarmenu when hovered */
                         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                         background-color: #23769a;
                         }
                         /* toggle button when hovered  */
                         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                         background-color: #23769a;
                         }
                         /* body */
                         .content-wrapper, .right-side {
                         background-color: white;
                         }
                         /* box */
                         .box{
                         -webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;
                         }
                         .box-body {
                         padding-left: 10px;
                         padding-right: 10px;
                         }
                         /* sidebar */
                         .sidebar {
                         padding-top: 100px;
                         }
                         /* pickerinput_waterbody */
                         .selectpicker {
                         z-index:99999 !important;
                         }
                         /* datepicker */
                         .datepicker {
                         z-index:99999 !important;
                         }
                         #caption {
                         font-size: 18px;
                         }
                         a {
                         color: #0000FF;
                         }
                         /* Adjust the percentage as needed */
                         .image-zoom {
                         zoom: 80%; 
                         }
                         '))
      ),
      
      # _ Header ----
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        
        tags$img(src = "DEQ-logo-color-horizontal370x73.png"),
        tags$div(span("Satellite Estimates of Cyanobacteria in Oregon Lakes and Reservoirs",
                      style = "color: black; font-size: 40px; font-weight:bold")),
        
        tags$h3("Reporting Period: ",
                ifelse(month(as.Date(max(dta2$Date))-6) %in% c(8,9,10,11,12,1,2),
                       gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date))-6,'%b. %d, %Y')),
                       gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date))-6,'%B %d, %Y'))),
                " - ",
                ifelse(month(as.Date(max(dta2$Date))) %in% c(8,9,10,11,12,1,2),
                       gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date)),'%b. %d, %Y')),
                       gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date)),'%B %d, %Y'))))
      ), # Header END
      
      # _ 1. Introduction ----
      shinydashboardPlus::box(
        width = 12,
        title = "Introduction",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        
        h4("This report provides an update on estimates of cyanobacteria abundance derived from satellite imagery for 49 large Oregon waterbodies. ",
           "Updates are scheduled to occur weekly from spring to fall each year. The estimates are derived from the ", 
           a("Cyanobacteria Assessment Network (CyAN)", href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan",target="_blank"),
           " project. Beginning in 2024, the report presents version 5 (V5) data, which underwent reprocessing by NASA in May 2023.",
           "The V5 dataset includes an enhanced filter for turbid water and a correction for clear water. Please refer to the ",
           a("NASA", href="https://oceancolor.gsfc.nasa.gov/data/reprocessing/projects/cyan/version/5/",.noWS = "outside",target="_blank"),
           " website for additional information on V5 data.",
           .noWS = c("after-begin", "before-end")),
        
        h4("This report illustrates cyanobacteria abundance (cells/mL) in three levels: Low: <20,000, Moderate: 20,000-100,000, and High: >100,000. ",
           "The levels correspond to the World Health Organization (WHO) exposure guideline values ",
           "(",a("WHO, 2003", href="https://apps.who.int/iris/bitstream/handle/10665/42591/9241545801.pdf?sequence=1&isAllowed=y",.noWS = "outside",target="_blank"),"). ",
           "For more information on harmful algal blooms in Oregon, please visit websites from the ",
           a("Oregon DEQ", href="https://www.oregon.gov/deq/wq/Pages/Harmful-Algal-Blooms.aspx",target="_blank")," and the ",
           a("Oregon Health Authority", href="https://www.oregon.gov/oha/ph/healthyenvironments/recreation/harmfulalgaeblooms/pages/blue-greenalgaeadvisories.aspx",.noWS = "outside",target="_blank"),".",
           .noWS = c("after-begin", "before-end")),
        
        h4("All data presented in this report are provisional and subject to change. Estimates of cyanobacteria from satellite imagery do not ",
           "imply the presence of cyanotoxins or other water quality impairments and do not have regulatory implications. ",
           tags$b("Visit the ",
                  a("Oregon Health Authority", href="https://www.oregon.gov/oha/ph/healthyenvironments/recreation/harmfulalgaeblooms/pages/blue-greenalgaeadvisories.aspx",.noWS = "outside",target="_blank"),
                  " to learn about recreational use and drinking water advisories related to cyanobacteria blooms. "),
           "Additional assessments using imagery from the",
           a("Sentinel 2", href="https://browser.dataspace.copernicus.eu/?zoom=7&lat=44.3466&lng=-119.25&themeId=DEFAULT-THEME&visualizationUrl=https%3A%2F%2Fsh.dataspace.copernicus.eu%2Fogc%2Fwms%2F274a990e-7090-4676-8f7d-f1867e8474a7&datasetId=S2_L1C_CDAS&fromTime=2023-07-01T00%3A00%3A00.000Z&toTime=2024-01-01T23%3A59%3A59.999Z&layerId=1_TRUE_COLOR&demSource3D=%22MAPZEN%22&cloudCoverage=100&dateMode=MOSAIC",
             target="_blank"),
           "Satellites, local visual assessments, and/or water quality sampling are needed to provide further information on potential human health ",
           "and environmental effects of cyanobacteria. Please note that estimates of cyanobacteria abundance presented in this report may be skewed ",
           "by cloud cover, ice cover, sun glint, water surface roughness, dry lake beds, algal mats, and shoreline effects.",
           .noWS = c("after-begin", "before-end"))
        
      ), # Introduction End
      
      # _ 2. Highlighted Waterbodies ----
      shinydashboardPlus::box(
        width = 12,
        title = "Highlighted Waterbodies",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        
        # ___ Section Introduction ----
        tags$h4(p("Waterbodies with high cyanobacteria abundance (>100,000 cells/mL) are identified based on ",
                  "the maximum value of the 7-Day Daily Maximum Geometric Mean (7DDMGM) during the reporting period, ",
                  "with the corresponding 'Date_7DDMGM' indicating the date of the maximum 7DDMGM value. ",
                  "The 7-Day Average Daily Maximum (7DADM) for each highlighted waterbody is reported as a reference. ",
                  "Both 7DDMGM and 7DADM represent moving averages calculated from the daily maximums from ",
                  "the most recent available data day within the reporting period to the preceding 7 days. ",
                  "The 'Days of Data' refers to the number of days within a 7-day moving window for computing both 7DDMGM and 7DADM.")),
        
        tags$h4(p(strong(paste0("Reporting Period: ",
                                ifelse(month(as.Date(max(dta2$Date))-6) %in% c(8,9,10,11,12,1,2),
                                       gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date))-6,'%b. %d, %Y')),
                                       gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date))-6,'%B %d, %Y'))),
                                " - ",
                                ifelse(month(as.Date(max(dta2$Date))) %in% c(8,9,10,11,12,1,2),
                                       gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date)),'%b. %d, %Y')),
                                       gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date)),'%B %d, %Y'))))))),
        
        # ___ 7-Day Table ----
        shinydashboard::box(
          width = 6,
          solidHeader = TRUE,
          
          shinycssloaders::withSpinner(DT::dataTableOutput("tbl7dadm")),
          
          tags$br(),
          tags$em("*GNISID: ",a("USGS Geographic Names Information System Identifier", 
                                href="https://www.usgs.gov/faqs/what-geographic-names-information-system-gnis",
                                .noWS = "outside",
                                target="_blank"),
                  .noWS = c("after-begin", "before-end"))
          
        ),
        
        # ___ 7-Day Map ----
        shinydashboard::box(
          width = 6,
          solidHeader = TRUE,
          
          tags$img(src = "map_7d.jpg", width = "100%")
          # shinyfullscreen::fullscreen_this(shiny::imageOutput("maps7"))
          # leaflet::leafletOutput("map7d", height = "700px")
          
        )
        
      ), # Highlighted Waterbodies End
      
      # _ 3. Satellite imagery  ----
      shinydashboardPlus::box(
        width = 12,
        title = "Satellite Imagery",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        
        # ___ Section Introduction ----
        tags$h4(p("Satellite imagery is provided from March 1, 2024 to the present due to limitations in server capacity.")),
        
        shinydashboard::box(
          width = 3,
          #title = "left",
          solidHeader = TRUE,
          
          # ___ Select a Waterbody ----
          shinyWidgets::pickerInput(inputId = "waterbody",
                                    label = tags$h4(strong("Select a Waterbody:")),
                                    choices = list("Oregon",
                                                   "Waterbody Name_GNISID" = sort(unique(lakes.resolvable$GNISIDNAME))),
                                    multiple = FALSE),
          # ___ Drinking Water Area ----
          shiny::textOutput("dw"),
          
          # ___ Select a Date ----
          shiny::dateInput(inputId = "date_map",
                           label = tags$h4(strong("Select a Date:")),
                           value = as.Date(max(dta2$Date)),
                           # min = as.Date(max(dta2$Date))-6,
                           min = as.Date("2024-03-01"),
                           max = as.Date(max(dta2$Date)),
                           format = "yyyy-mm-dd",
                           startview = "month",
                           weekstart = 0,
                           datesdisabled = missing.dates$Date)
          
          # tags$hr(),
          
          # ___ Boxplot ----
          # tags$h4("Boxplot of cyanobacteria estimates (cells/mL) in waterbody on selected date:"),
          # 
          # shinycssloaders::withSpinner(plotlyOutput("boxplot"))
          
        ),
        
        # ___ Interactive Map ----
        shinydashboard::box(
          width = 9,
          #title = "right",
          solidHeader = TRUE,
          
          shinycssloaders::withSpinner(leaflet::leafletOutput("map", height = "700px"))
          
        )
        
      ),
      
      # _ 4. Time series data ----
      shinydashboardPlus::box(
        width = 12,
        height = "100%",
        title = "Time Series Data",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        
        # ___ Section Introduction ----
        tags$h4(p("Time series data of cyanobacteria estimates is provided for each of the 49 resolvable waterbodies, according to the methods outlined in the ",
                  a("CyAN Project", href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan",.noWS = "outside",target="_blank"),
                  ".",.noWS = c("after-begin", "before-end"))),
        
        # ___ Plot and Table ----
        shinydashboard::box(
          width = 12,
          #title = "plot+table",
          solidHeader = TRUE,
          
          shinydashboard::box(
            width = 3,
            #title = "left",
            solidHeader = FALSE,
            
            # h3("Time Series Plot and Data:"),
            
            # ____ Select a Waterbody 2 ----
            shinyWidgets::pickerInput(inputId = "waterbody2",
                                      label = tags$h4(strong("Select a waterbody:")),
                                      choices = list("Oregon",
                                                     "Waterbody Name_GNISID" = sort(unique(lakes.resolvable$GNISIDNAME))),
                                      multiple = FALSE),
            
            # ____ Date range ----
            shiny::radioButtons(
              inputId = "ploty",
              label = tags$h4(strong("Date Range:")),
              choices = c("Current Year: 2024",
                          "Reset to Complete Data Range",
                          "Select a Date Range"),
              selected = "Current Year: 2024"),
            
            shiny::dateRangeInput(inputId = "date_plot",
                                  label = "",
                                  start = min(dta$Date),
                                  end = max(dta$Date),
                                  min = min(dta$Date),
                                  max = max(dta$Date),
                                  separator = "to",
                                  format = "yyyy-mm-dd",
                                  startview = "year",
                                  weekstart = 0),
            
            uiOutput("dataDate"),
            
            tags$br(),
            tags$br(),
            
            # ____ Summary statistics ----
            checkboxGroupInput(
              inputId = "matrix",
              label = tags$h4(strong("Summary Statistics:")),
              choices = c("7-Day Average Daily Maximum (7DADM)" = "7DADM",
                          "7-Day Daily Maximum Geometric Mean (7DDMGM)" = "7DDMGM",
                          "Daily Maximum" = "Daily Maximum",
                          "Daily Mean" = "Daily Mean"),
              selected = c("7DDMGM","Daily Maximum")),
            
            tags$br(),
            tags$br(),
            
            # ____ Plot types ----
            checkboxGroupInput(
              inputId = "plot_log",
              label = tags$h4(strong("y-axis:")),
              choices = c("Log Scale" = "log"))
            
          ),
          
          shinydashboard::box(
            width = 9,
            #title = "right",
            solidHeader = FALSE,
            
            # ____ Time series plot ----
            tags$h4(p(strong("Time series plot of cyanobacteria abundance (cells/mL) of the selected waterbody."))),
            
            textOutput("no_plot"),
            
            plotlyOutput("plot_cell"),
            
            tags$br(),
            tags$br(),
            
            uiOutput("who_line"),
            
            tags$br(),
            tags$br(),
            
            # ____ Data table ----
            shinydashboard::box(
              width = 12,
              #title = "right",
              solidHeader = FALSE,
              
              tags$h4(p(strong(("Time series data of the selected waterbody during the selected date range.")))),
              
              textOutput("no_data"),
              
              tags$br(),
              uiOutput("caption"),
              
              DT::dataTableOutput("table")
              
            )
            
          ),
          
          # _ 5. Copyright and Contacts ----
          shinydashboard::box(
            width = 12,
            #title = "copyright",
            solidHeader = FALSE,
            
            h4("The report is provided by the Oregon DEQ Watershed Management Section. Copyright (C) 2020-2024, Oregon DEQ."),
            h4("The source code of this report is publicly available at GitHub repository: ", 
               a("Satellite Estimates of Cyanobacteria in Oregon Lakes and Reservoirs",
                 href="https://github.com/OR-Dept-Environmental-Quality/CyAN_imagery_update",.noWS = "outside",target="_blank"),".",
               .noWS = c("after-begin", "before-end")),
            h4("For more information on this report, please contact"),
            h4("Daniel Sobota (Lead), ", a("daniel.sobota@deq.oregon.gov",href="mailto:dan.sobota@deq.oregon.gov",target="_blank")),
            # h4("Erin Costello, ", a("erin.costello@deq.oregon.gov",href="mailto:erin.costello@deq.oregon.gov",target="_blank")),
            h4("Yuan Grund, ", a("yuan.grund@deq.oregon.gov",href="mailto:yuan.grund@deq.oregon.gov",target="_blank"))
            
          )
          
        )
        
      )
      
    ) # Body End
    
  ), # ui End
  
  server = function(input, output, session) {
    
    # 1. Maps ----
    progress <- reactiveValues(value = 0)
    # _ initial map ----
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addMapPane("OpenStreetMap", zIndex = -40) %>% 
        leaflet::addMapPane("National Geographic World Map", zIndex = -40) %>%
        leaflet::addMapPane("state.boundary", zIndex = -30) %>%
        leaflet::addMapPane("HUC6",zIndex = -20) %>% 
        leaflet::addMapPane("lakes.resolvable", zIndex = 400) %>%
        leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap",
                                  options = leaflet::pathOptions(pane = "OpenStreetMap")) %>% 
        leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap,group = "National Geographic World Map",
                                  options = leaflet::pathOptions(pane = "National Geographic World Map")) %>% 
        leaflet::setView(lng = -120, lat = 44, zoom=7) %>%
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addScaleBar(position = c("bottomright"),
                             options = leaflet::scaleBarOptions()) %>% 
        leaflet::addMiniMap(position = "bottomright",
                            width = 180,
                            height = 200,
                            zoomLevelFixed = 5) %>% 
        leaflet::addPolygons(data = lakes.resolvable, 
                             color = "blue",
                             weight = 2,
                             layer = ~lakes.resolvable$GNISIDNAME,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             label = ~lakes.resolvable$GNIS_Name,
                             labelOptions = leaflet::labelOptions(style = list("font-size" = "18px",
                                                                               "color" = "blue")),
                             options = leaflet::pathOptions(pane = "lakes.resolvable"),
                             group = "lakes.resolvable") %>% 
        leaflet::addPolygons(data = huc6, 
                             group = "Basins (HUC6)",
                             color = "grey",
                             weight = 2,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = ~pal.huc6(HU_6_NAME),
                             fillOpacity = 0.2,
                             label = ~huc6$HU_6_NAME,
                             labelOptions = leaflet::labelOptions(noHide = TRUE,
                                                                  textOnly = TRUE,
                                                                  style = list("font-size" = "12px",
                                                                               "color" = "black")),
                             options = leaflet::pathOptions(pane = "HUC6")) %>% 
        leaflet::addPolygons(data = state.boundary, 
                             color = "black",
                             weight = 2,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             options = leaflet::pathOptions(pane = "state.boundary")) %>% 
        leaflet::addLayersControl(baseGroups = c("OpenStreetMap","National Geographic World Map"),
                                  overlayGroups = c("Basins (HUC6)"),
                                  position = "topleft",
                                  options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
        leaflet::hideGroup(c("Basins (HUC6)")) %>% 
        leaflet.extras::addSearchFeatures(targetGroups = "lakes.resolvable",
                                          options = leaflet.extras::searchFeaturesOptions(openPopup = TRUE,
                                                                                          zoom = 8,
                                                                                          textPlaceholder = "Search a waterbody..."))
      
    })
    
    # _ map reactive @ date selector ----
    observeEvent(input$date_map,{
      
      progress$value <- 0
      withProgress(message = 'Updating map, please wait...', value = progress$value, {
        
        df.map.date <- reactive({
          
          lookup.date %>% dplyr::filter(Date %in% as.Date(input$date_map))
          
        })
        
        if(is.na(df.map.date()$Year.dta)){
          
          return(NULL)
          
        } else {
          
          map.tif.dir <- reactive(paste0("./data/", df.map.date()$Year.dates, "/"))
          file.name <- reactive(paste0(df.map.date()$CyAN_File_NUM,".tif"))
          
          rst <- reactive({
            
            r <- raster::raster(paste0(map.tif.dir(), file.name()))
            raster::crs(r) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
            return(r)
            
          })
          
          leafletProxy("map") %>% 
            leaflet::clearImages() %>% 
            leaflet::clearControls() %>% 
            leaflet::addRasterImage(rst(), layerId = "Value", project = FALSE, colors=pal.map, opacity = 1,
                                    group = "Satellite Imagery of cyanobacteria") %>% 
            leaflet::addLegend(pal = pal.map, values = thevalues, title = "Cyanobacteria (cells/mL)", position = "topright",
                               labFormat = function(type,cuts,p){paste0(labels)},opacity = 1) %>% 
            leaflet::addLayersControl(baseGroups = c("OpenStreetMap","National Geographic World Map"),
                                      overlayGroups = c("Satellite Imagery of cyanobacteria","Basins (HUC6)"),
                                      position = "topleft",
                                      options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
            leaflet::hideGroup(c("Basins (HUC6)"))
          
        }
        
      })
      
    })
    
    
    # _ map reactive @ waterbody picker ----
    selected_waterbody <- reactiveVal(NULL)
    observeEvent(input$waterbody,{
      
      if (input$waterbody == input$waterbody2) {
        
        if(input$waterbody == c("Oregon")) {
          
          leafletProxy("map") %>% 
            leaflet::setView(lng = -120, lat = 44, zoom=7)
          
        } else {
          
          one.lake <- reactive({
            
            lakes.resolvable[which(lakes.resolvable$GNISIDNAME == input$waterbody),]
            
          })
          
          bounds <- reactive({
            
            sf::st_bbox(one.lake())
            
          })
          
          leafletProxy("map") %>% 
            leaflet::fitBounds(lng1=bounds()[[1]], lat1=bounds()[[2]], lng2=bounds()[[3]], lat2=bounds()[[4]])
        }
        
      }else{
        
        selected_waterbody(input$waterbody)
        updatePickerInput(session, "waterbody2", selected = selected_waterbody())
        
        if(input$waterbody == c("Oregon")) {
          
          leafletProxy("map") %>% 
            leaflet::setView(lng = -120, lat = 44, zoom=7)
          
        } else {
          
          one.lake <- reactive({
            
            lakes.resolvable[which(lakes.resolvable$GNISIDNAME == input$waterbody),]
            
          })
          
          bounds <- reactive({
            
            sf::st_bbox(one.lake())
            
          })
          
          leafletProxy("map") %>% 
            leaflet::fitBounds(lng1=bounds()[[1]], lat1=bounds()[[2]], lng2=bounds()[[3]], lat2=bounds()[[4]])
        }
        
      }
      
    })
    
    observeEvent(input$waterbody2, {
      if (input$waterbody != input$waterbody2) {
        selected_waterbody(input$waterbody2)
        updatePickerInput(session, "waterbody", selected = selected_waterbody())
      }
    })
    
    # _ highlighted waterbody map ----
    # output$maps7 <- renderImage({
    #   
    #   list(src = paste0("./data/map_7d.jpg"),width = "100%")
    #   
    # }, deleteFile = FALSE)
    
    # output$map7d <- leaflet::renderLeaflet({
    # 
    #   leaflet::leaflet() %>%
    #     leaflet::addControl(map.title, position = "topleft", className="map-title") %>%
    #     leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap") %>%
    #     leaflet::setView(lng = -120, lat = 44, zoom=7) %>%
    #     leaflet.extras::addResetMapButton() %>% 
    #     leaflet::addPolygons(data = lakes.resolvable.7d,
    #                          color = ~palette7dadm(lakes.resolvable.7d$`7dadm`),
    #                          weight = 2,
    #                          layer = ~lakes.resolvable.7d$GNISIDNAME,
    #                          smoothFactor = 0.5,
    #                          opacity = 1,
    #                          fillColor = "transparent",
    #                          fillOpacity = 0,
    #                          label = ~lakes.resolvable.7d$GNIS_Name,
    #                          labelOptions = leaflet::labelOptions(
    #                            noHide = TRUE,
    #                            textOnly = TRUE,
    #                            # opacity = 0.75,
    #                            direction = "right",
    #                            offset = c(15, 5),
    #                            style = list("font-size" = "16px","font-style" = "italic","color" = "blue")),
    #                          group = "lakes.resolvable.7d") %>%
    #     leaflet::addPolygons(data = state.boundary,
    #                          color = "black",
    #                          weight = 2,
    #                          fillColor = "transparent",
    #                          fillOpacity = 1.0)
    # 
    # })
    
    # 2. Plots ----
    # _ Time series plot ----
    pal.plot <- c("brown","blue","orange","green","white","white","white")
    pal.plot <- setNames(pal.plot,unique(sort(dta$`Summary Statistics`)))
    
    yr <- reactive({ 
      
      if(input$ploty == "Current Year: 2024"){"2024"}else{sort(unique(dta$Year))}
      
    })
    
    df <- reactive({
      
      if(input$ploty == "Current Year: 2024"){
        
        dta %>% 
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(`Summary Statistics` %in% input$matrix) %>% 
          dplyr::filter(Year %in% c(yr())) %>% 
          dplyr::mutate(`Cyanobacteria (cells/mL)` = round(`Cyanobacteria (cells/mL)`,0))
        
      }else if (input$ploty == "Reset to Complete Data Range") {
        
        dta %>% 
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(`Summary Statistics` %in% input$matrix) %>% 
          dplyr::mutate(`Cyanobacteria (cells/mL)` = round(`Cyanobacteria (cells/mL)`,0))
        
      } else {
        
        dta %>% 
          dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
          dplyr::filter(`Summary Statistics` %in% input$matrix) %>% 
          dplyr::filter(Year %in% c(yr())) %>%
          dplyr::mutate(`Cyanobacteria (cells/mL)` = round(`Cyanobacteria (cells/mL)`,0)) %>% 
          dplyr::filter(Date >= input$date_plot[1],Date <= input$date_plot[2])
        
      }
      
    })
    
    # df_before_gap <-  reactive({ 
    #   df() %>% dplyr::filter(Date >= as.Date("2002-01-01") & Date <= as.Date("2012-12-31"))
    # })
    
    df_after_gap <- reactive({ 
      df() %>% dplyr::filter(Date >= as.Date("2016-01-01"))
    })
    
    type <- reactive({
      
      input$plot_log
      
    })
    
    yaxis <- reactive({
      
      if_else(length(input$plot_log)>0,
              "Cyanobacteria (cells/mL)",
              "Cyanobacteria (cells/mL)")
      
    })
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$no_plot <- renderText({ 
          
          "Select a waterbody to show the plot."
          
        })
        
      } else {
        
        output$no_plot <- renderText({})
        
        output$plot_cell <- renderPlotly({
          
          if(input$ploty == "Current Year: 2024"){
            
            plotly::plot_ly() %>%
              # plotly::add_trace(data = df_before_gap(), 
              #                   x = ~as.Date(Date), 
              #                   y = ~`Cyanobacteria (cells/mL)`,
              #                   split = ~`Summary Statistics`,
              #                   type = "scatter",
              #                   mode = "lines+markers",
              #                   color = ~`Summary Statistics`,
              #                   colors = pal.plot,
              #                   marker = list(size = 8),
              #                   legendgroup = "sta",
              #                   showlegend = FALSE) %>%
            plotly::add_trace(data = df_after_gap(), 
                              x = ~as.Date(Date), 
                              y = ~`Cyanobacteria (cells/mL)`,
                              split = ~`Summary Statistics`,
                              type = "scatter",
                              mode = "lines+markers",
                              color = ~`Summary Statistics`,
                              colors = pal.plot,
                              marker = list(size = 8),
                              legendgroup = "sta") %>%
              plotly::layout(xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date)+1)),
                             # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                             title = as.character(unique(df()$GNISIDNAME))) %>% 
              plotly::layout(yaxis = list(type = type(),
                                          title = yaxis())) %>% 
              plotly::add_trace(y = 100000, mode = "lines",
                                # x = ~as.Date(dta$Date),
                                x = ~as.Date(df()$Date),
                                line = list(shape = 'spline', color = '#006d2c', width = 3),
                                name = "High",
                                legendgroup = "high",
                                showlegend = FALSE) %>% 
              plotly::layout(annotations = list(x = max(as.Date(df()$Date)),
                                                y = 100000,
                                                text = "High (100,000 cells/mL)**",
                                                font = list(size = 12),
                                                xref = "x",
                                                yref = "y",
                                                showarrow = TRUE,
                                                arrowhead = 3,
                                                arrowsize = 1,
                                                ax = -60,
                                                ay = -20)) %>%
              plotly::layout(shapes = list(list(type = "rect",
                                                text = 'Report',
                                                fillcolor = "green",
                                                line = list(color = "green"),
                                                opacity = 0.2,
                                                y0 = 0.5,
                                                y1 = max(df()$`Cyanobacteria (cells/mL)`) + 50000,
                                                x0 = as.Date(max(dta2$Date))-6,
                                                x1 = as.Date(max(dta2$Date))))) %>%
              plotly::add_text(showlegend = FALSE,
                               x = c(as.Date(max(dta2$Date))-3),
                               y = c(max(df()$`Cyanobacteria (cells/mL)`) + 30000),
                               text = c("RP*"),
                               textfont = list(size=12))
            
          } else if (input$ploty == "Reset to Complete Data Range") {
            
            plotly::plot_ly() %>%
              # plotly::add_trace(data = df_before_gap(), 
              #                   x = ~as.Date(Date), 
              #                   y = ~`Cyanobacteria (cells/mL)`,
              #                   split = ~`Summary Statistics`,
              #                   type = "scatter",
              #                   mode = "lines+markers",
              #                   color = ~`Summary Statistics`,
              #                   colors = pal.plot,
              #                   marker = list(size = 8),
              #                   legendgroup = "sta",
              #                   showlegend = FALSE) %>%
            plotly::add_trace(data = df_after_gap(), 
                              x = ~as.Date(Date), 
                              y = ~`Cyanobacteria (cells/mL)`,
                              split = ~`Summary Statistics`,
                              type = "scatter",
                              mode = "lines+markers",
                              color = ~`Summary Statistics`,
                              colors = pal.plot,
                              marker = list(size = 8),
                              legendgroup = "sta") %>%
              plotly::layout(xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date)+1)),
                             # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                             title = as.character(unique(df()$GNISIDNAME))) %>% 
              plotly::layout(yaxis = list(type = type(),
                                          title = yaxis())) %>% 
              plotly::add_trace(y = 100000, mode = "lines",
                                # x = ~as.Date(dta$Date),
                                x = ~as.Date(df()$Date),
                                line = list(shape = 'spline', color = '#006d2c', width = 3),
                                name = "High",
                                legendgroup = "high",
                                showlegend = FALSE) %>% 
              plotly::layout(annotations = list(x = max(as.Date(df()$Date)),
                                                y = 100000,
                                                text = "High (100,000 cells/mL)**",
                                                font = list(size = 12),
                                                xref = "x",
                                                yref = "y",
                                                showarrow = TRUE,
                                                arrowhead = 3,
                                                arrowsize = 1,
                                                ax = -60,
                                                ay = -20)) %>%
              plotly::layout(shapes = list(list(type = "rect",
                                                text = 'Report',
                                                fillcolor = "green",
                                                line = list(color = "green"),
                                                opacity = 0.2,
                                                y0 = 0.5,
                                                y1 = max(df()$`Cyanobacteria (cells/mL)`) + 50000,
                                                x0 = as.Date(max(dta2$Date))-6,
                                                x1 = as.Date(max(dta2$Date))))) %>%
              plotly::add_text(showlegend = FALSE,
                               x = c(as.Date(max(dta2$Date))-3),
                               y = c(max(df()$`Cyanobacteria (cells/mL)`) + 30000),
                               text = c("RP*"),
                               textfont = list(size=12))
            
            
            
          } else {
            
            plotly::plot_ly(data = df(), x = ~as.Date(Date)) %>% 
              plotly::add_trace(y = ~`Cyanobacteria (cells/mL)`,
                                split = ~`Summary Statistics`,
                                type = "scatter",
                                mode = "lines+markers",
                                #connectgaps = TRUE,
                                color = ~`Summary Statistics`,
                                colors = pal.plot,
                                marker = list(size = 8),
                                legendgroup = "sta") %>% 
              plotly::layout(xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date)+1)),
                             # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                             title = as.character(unique(df()$GNISIDNAME))) %>% 
              plotly::layout(yaxis = list(type = type(),
                                          title = yaxis())) %>% 
              plotly::add_trace(y = 100000, mode = "lines",
                                # x = ~as.Date(dta$Date),
                                x = ~as.Date(df()$Date),
                                line = list(shape = 'spline', color = '#006d2c', width = 3),
                                name = "High",
                                legendgroup = "high",
                                showlegend = FALSE) %>% 
              plotly::layout(annotations = list(x = max(as.Date(df()$Date)),
                                                y = 100000,
                                                text = "High (100,000 cells/mL)**",
                                                font = list(size = 12),
                                                xref = "x",
                                                yref = "y",
                                                showarrow = TRUE,
                                                arrowhead = 3,
                                                arrowsize = 1,
                                                ax = -60,
                                                ay = -20))
            
          }
          
        })
        
        output$who_line <- renderUI(HTML(paste("&nbsp;","&nbsp;","&nbsp;","&nbsp;",
                                               em(paste0("*RP: Reporting period from ",
                                                         ifelse(month(as.Date(max(dta2$Date))-6) %in% c(8,9,10,11,12,1,2),
                                                                gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date))-6,'%b. %d, %Y')),
                                                                gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date))-6,'%B %d, %Y'))),
                                                         # "Aug. 14, 2023",
                                                         " to ",
                                                         ifelse(month(as.Date(max(dta2$Date))) %in% c(8,9,10,11,12,1,2),
                                                                gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date)),'%b. %d, %Y')),
                                                                gsub("(\\D)0", "\\1", format(as.Date(max(dta2$Date)),'%B %d, %Y'))),
                                                         # "Aug. 20, 2023",
                                                         ".")),
                                               "<br/>",
                                               "&nbsp;","&nbsp;","&nbsp;","&nbsp;",
                                               em("**High (100,000 cells/mL): World Health Organization (WHO) Recreational Use Value (RUV) Guideline for moderate probability of adverse health effects."))))
        
      }
      
    })
    
    # _ Boxplot ----
    # df.box <- reactive({
    #   
    #   dn_tbl %>%
    #     dplyr::filter(GNISIDNAME %in% input$waterbody) %>%
    #     dplyr::filter(as.Date(Date) %in% as.Date(input$date_map))  %>%
    #     tidyr::pivot_longer(cols = tidyr::all_of(grep("^VALUE_", colnames(dn_tbl), value = TRUE)),
    #                         names_to = "pixcel_value", values_to = "counts") %>%
    #     dplyr::mutate(pixcel_value = as.numeric(str_remove(pixcel_value, "VALUE_"))) %>%
    #     dplyr::filter(!pixcel_value %in% c(254,255)) %>%
    #     dplyr::filter(!is.na(counts)) %>%
    #     dplyr::slice(rep(dplyr::row_number(), counts)) %>%
    #     dplyr::select(-counts) %>%
    #     dplyr::mutate(`Cyanobacteria (cells/mL)` = (10^(3.0 / 250.0 * pixcel_value - 4.2)) * 100000000)
    #   
    # })
    # 
    # output$boxplot <- renderPlotly({
    #   
    #   plotly::plot_ly(data = df.box(), y = ~`Cyanobacteria (cells/mL)`, type = "box")
    #   
    #   plotly::plot_ly(data = df.box(),
    #                   x = ~as.Date(Date, format= "%Y-%m-%d"),
    #                   y = ~`Cyanobacteria (cells/mL)`,
    #                   type = "box",
    #                   name = ~unique(GNISIDNAME)) %>%
    #     plotly::add_trace(x = as.Date(input$date_map),
    #                       y = 100000,
    #                       line = list(color = "#006d2c")) %>%
    #     plotly::layout(xaxis = list(title = "",
    #                                 zeroline = FALSE,
    #                                 showline = FALSE,
    #                                 showticklabels = FALSE,
    #                                 showgrid = FALSE),
    #                    yaxis = list(type = "log",
    #                                 title = "",
    #                                 zeroline = TRUE,
    #                                 showline = TRUE,
    #                                 showticklabels = TRUE,
    #                                 showgrid = FALSE),
    #                    showlegend = FALSE,
    #                    annotations = list(x = as.Date(input$date_map),
    #                                       y = log(100000)/log(10),
    #                                       #y = 100000,
    #                                       text = "WHO Threshold",
    #                                       font = list(size = 8),
    #                                       xref = "x",
    #                                       yref = "y",
    #                                       showarrow = TRUE,
    #                                       arrowhead = 3,
    #                                       arrowsize = 1,
    #                                       ax = 40,
    #                                       ay = 50))
    #   
    # })
    
    # 3. Tables ----
    # _ 7-Day Table ----
    output$tbl7dadm <- DT::renderDataTable({
      
      DT::datatable(
        data = map.tbl.data,
        style = 'bootstrap',
        extensions = 'Buttons',
        options = list(dom = 'frtilpB',
                       pageLength = 10,
                       compact = TRUE,
                       nowrap = TRUE,
                       scorllX = TRUE,
                       scorllY = TRUE,
                       autoWidth = TRUE,
                       columnDefs = list(list(targets = 0:3, className = "dt-left"),
                                         list(targets = (0), width = "50%"),
                                         list(targets = (1), width = "50%"),
                                         list(targets = (2), width = "10%"),
                                         list(targets = (3), width = "10%")),
                       buttons = list(#'print',
                         list(extend = 'collection',
                              buttons = c('csv','excel'),
                              text = 'Download')
                       )),
        rownames = FALSE,
        filter = 'bottom'
      ) #%>% 
      #DT::formatDate("Date","toLocaleString")
    }, server = FALSE
    
    )
    
    # _ Data table ----
    df_tbl <- reactive({
      
      df() %>% 
        dplyr::select(GNISIDNAME,Date,`Cyanobacteria (cells/mL)`,`Summary Statistics`) %>% 
        dplyr::mutate(`Cyanobacteria (cells/mL)` = ifelse(`Cyanobacteria (cells/mL)` <= 6310, "Non-detect",
                                                          scales::comma(`Cyanobacteria (cells/mL)`))) %>%
        dplyr::rename(Waterbody_GNISID = GNISIDNAME)
    })
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$no_data <- renderText({ 
          
          "Select a waterbody to show the data table."
          
        })
        
      } else {
        
        output$no_data <- renderText({})
        
        output$caption <- renderUI(HTML(unique((df_tbl()$Waterbody_GNISID))))
        
        output$table <- DT::renderDataTable({
          
          DT::datatable(
            data = df_tbl(),
            style = 'bootstrap',
            extensions = 'Buttons',
            options = list(dom = 'frtilpB',
                           pageLength = 10,
                           compact = TRUE,
                           nowrap = TRUE,
                           scorllX = TRUE,
                           buttons = list(
                             list(extend = 'collection',
                                  buttons = c('csv','excel'),
                                  text = 'Download')
                           )),
            rownames = FALSE,
            filter = 'bottom')
        }, server = FALSE)
        
      }
      
    })
    
    # 4. Text ----
    # _ Drinking Water Area ----
    dw <- reactive({
      
      dta %>% 
        dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
        dplyr::mutate(dwsa = ifelse(wi_DWSA == "Yes", "Public Drinking Water Source", "Recreational Waterbody")) %>% 
        pull(dwsa)
      
    })
    
    output$dw <- renderText({ 
      
      if(input$waterbody == c("Oregon")) {}
      else {
        unique(dw())
      }
    })
    
    # _ Data Date ----
    dd <- reactive({
      
      dta %>% dplyr::filter(GNISIDNAME %in% input$waterbody) 
      
    })
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {} 
      else {
        
        output$dataDate <- renderUI(HTML(paste0(
          "Data for ",input$waterbody," is available since ",
          ifelse(month(as.Date(min(dd()$Date))) %in% c(8,9,10,11,12,1,2), 
                 gsub("(\\D)0", "\\1", format(as.Date(min(dd()$Date)),'%b. %d, %Y')), 
                 gsub("(\\D)0", "\\1", format(as.Date(min(dd()$Date)),'%B %d, %Y'))),
          "."
          
        )))
        
      }
      
    })
    
  }
  
)