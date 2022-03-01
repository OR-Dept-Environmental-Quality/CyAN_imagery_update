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
library(scales)
library(plotly)
library(DT)
library(lubridate)

load("data.RData")

# Shiny App ----
shinyApp(
  ui = shinydashboardPlus::dashboardPage(
    options = list(sidebarExpandOnHover = FALSE),
    #header = shinydashboardPlus::dashboardHeader(titleWidth = 400),
    header = shinydashboardPlus::dashboardHeader(titleWidth = 0, disable = TRUE),
    
    # Sidebar ----
    sidebar = shinydashboardPlus::dashboardSidebar(
      width = "0px" #---remove sidebar
    ),              #---remove sidebar
    #  minified = TRUE, collapsed = TRUE, width = 400,
      
    #  sidebarMenu(
    #    menuItem("About", icon = icon("info-circle"),
    #             menuSubItem(h4(HTML("
    #             This web application provides an interactive<br/>
    #             map to view satellite derived data on<br/>
    #             cyanobacteria harmful algal blooms in<br/>
    #             freshwater ecosystems of Oregon. Satellite<br/>
    #             data come from the US EPA CyAN project<br/>
    #             and are updated on a regular basis.<br/>
    #             <br/>
    #             Copyright (C) 2020-2021, ODEQ.")))),
    #    menuItem("User Guide",  icon = icon("cog"), href="userGuide.html"),
    #    menuItem("Contact", icon = icon("envelope"),
    #             menuSubItem(h5(HTML("
    #             For more information on the Oregon HABs Map<br/>
    #             Application Project, please contact<br/>
    #             <br/>
    #             Dan Sobota, Water Quality Analyst (Lead) <br/>
    #             Daniel.Sobota@deq.state.or.us<br/>
    #             <br/>
    #             Erin Costello, Water Quality Analyst<br/>
    #             Erin.Costello@deq.state.or.us<br/>
    #             <br/>
    #             Yuan Grund, Water Quality Analyst<br/>
    #             Yuan.Grund@deq.state.or.us"))))
    #  ) # sidebarMenu END
    #), # dashboardSidebar END
    
    # Body ----
    body = shinydashboard::dashboardBody(
      
      tags$div(
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
                         
                         /*pickerinput_waterbody*/
                         .selectpicker {
                         z-index: 999999999 !important;
                         }
                         
                         /*datepicker*/
                         .datepicker {
                         z-index:99999 !important;
                         }
                         '))),
      
      # _ Header ----
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        
        tags$img(src = "DEQ-logo-color-horizontal370x73.png"),
        tags$div(span("Satellite Estimates of Cyanobacteria in Oregon Lakes and Reservoirs",
                      style = "color: black; font-size: 40px")),
        
        tags$h3("Report Date:",max(dta$Date)),
        
        #tags$div(span(HTML(paste0("Last sourced from the ",
        #                          a("U.S. EPA CyAN Project", 
        #                            href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan"),
        #                          " on: ",
        #                          max(dta$Date))),
        #              style = "color: black; font-size: 20px"))
        
      ), # Header box END 

      # _ 1. Introduction ----
      shinydashboardPlus::box(
        width = 12,
        title = "1. Introduction",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        #dropdownMenu = boxDropdown(),
        
        h4("This report provides a statewide update for satellite imagery derived estimates of cyanobacteria cell counts in Oregon waterbodies. ",
           "Data of estimates are retrieved from the ", a("Cyanobacteria Assessment Network (CyAN)", href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan"),
           " project, and grouped into 3 categories to illustrate cyanobacteria abundance (cells/mL): Low: <30,000, High: 30,000-100,000, and ",
           "Very High: >100,000. The level of Very High refers to the estimates above the World Health Organization (WHO) recreational exposure ",
           "guideline value of 100,000 cells/mL. Potential human health risks include skin irritations and gastrointestinal illness (",
           a("WHO, 2003", href="https://apps.who.int/iris/bitstream/handle/10665/42591/9241545801.pdf?sequence=1&isAllowed=y"),"). A total of 49 ",
           "satellite resolvable waterbodies are included in this report. The report is updated weekly from March to October."),
        
        h4("The report consists of three parts."),
        h4("Part 1 Introduction provides a general background, outline and limitations of the report."),
        h4("Part 2 highlights the waterbodies with very high cyanobacteria cell count estimates during the reporting period. The 7-Day Average Daily Maximum (7DADM) is used for summarizing these estimates."),
        h4("Part 3 provides interactive data visualization for each resolvable waterbody."),
        
        h4("All data presented in this report are provisional and subject to change. Estimates of cyanobacterial abundance may be skewed by cloud cover, ",
           "ice cover, sun glint, water surface roughness, dry lake beds, algal mats and shoreline effects. We suggest examining additional imagery from ",
           "the ", a("Sentinel 2 website", href="https://www.sentinel-hub.com/explore/sentinelplayground/"),
           ", visiting ", a("OHA's current cyanobacteria advisory website", href="https://www.oregon.gov/oha/ph/healthyenvironments/recreation/harmfulalgaeblooms/pages/blue-greenalgaeadvisories.aspx"),
           ", using ", a("EPA’s CyAN app", href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-application-cyan-app"),
           ", or following up with site visits to confirm on the ground conditions. ")
        
        ),
      
      # _ 2. Table and Oregon map ----
      shinydashboardPlus::box(
        width = 12,
        title = "2. Waterbodies with very high cynabacteria estimates (>100,000 cells/mL)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        #dropdownMenu = boxDropdown(),
        
        # ___ Table 7DADM ----
        shinydashboard::box(
          width = 5,
          #title = "Table",
          solidHeader = FALSE,
          
          tags$h4(p(strong((paste0("Waterbodies are ranked by the 7DADM of cyanobacteria abundance (cells/mL) from ",format(as.Date(max(dta2$Date))-7, "%B %d, %Y")," to ",format(max(dta2$Date),'%B %d, %Y'),"."))))),
          
          shinycssloaders::withSpinner(DT::dataTableOutput("tbl7dadm"))
          
        ),
        
        # ___ Oregon map ----
        shinydashboard::box(
          width = 7,
          #title = "ORMap",
          solidHeader = FALSE,
          
          tags$h4(p(strong("Waterbodies with very high cyanobacteria estimates from ",format(as.Date(max(dta2$Date))-7, "%B %d, %Y")," to ",format(max(dta2$Date),'%B %d, %Y')," are outlined in red. Other resolvable waterbodies are in blue."))),
          
          shinycssloaders::withSpinner(leaflet::leafletOutput("map", height = "650px"))
          
        )
      ),
      
      # _ 3. Maps and Time series plots ----
      shinydashboardPlus::box(
        width = 12,
        title = "3. Maps and time series plot of cyanobacteria estimates by waterbody",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        
        # ___ 7maps ----
        shinydashboard::box(
          width = 12,
          #title = "map",
          solidHeader = TRUE,
          
          shinydashboard::box(
            width = 3,
            #title = "left",
            solidHeader = FALSE,
            
            # ____ Select a waterbody ----
            shinyWidgets::pickerInput(inputId = "waterbody",
                                      label = tags$h3("Select a Waterbody:"),
                                      choices = list(
                                        "Oregon",
                                        "Waterbody Name_GNISID" = sort(unique(lakes.resolvable$GNISIDNAME))
                                      ),
                                      multiple = FALSE),
            # ____ Drinking water area ----
            shiny::textOutput("dw"),
            
            # ____ Lake images ----
            tags$br(),
            textOutput("non_select_image"),
            shiny::imageOutput("lakeImage", width = "100%", height = "100%",inline = TRUE),
            tags$h5("Source:")
            
          ),
          
          shinydashboard::box(
            width = 9,
            #title = "right",
            solidHeader = FALSE,
            
            # ____ 7maps ----
            #shinydashboard::box(
            #  width = 12,
              #title = "maps7",
            #  solidHeader = TRUE,
              
              tags$h4(p(strong(paste0("Satellite estimates of cyanobacterial abundance (cells/mL) of the selected waterbody from ",format(as.Date(max(dta2$Date))-7, "%B %d, %Y")," to ",format(max(dta2$Date),'%B %d, %Y'),".")))),
              
              textOutput("non_select"),
              shiny::imageOutput("maps7")
              
            #)
            
            )
          
        ),
        
        # ___ Plot and Table ----
        shinydashboard::box(
          width = 12,
          #title = "plot+table",
          solidHeader = TRUE,
          
          shinydashboard::box(
            width = 3,
            #title = "left",
            solidHeader = FALSE,
            
            h3("Time Series Plot and Data:"),
            # ____ Date range ----
            shiny::radioButtons(
              inputId = "ploty",
              label = tags$h4("Date Range:"),
              choices = c("Current Year: 2022",
                          "Select a Date Range"),
              selected = "Current Year: 2022"
            ),
            
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
            
            tags$br(),
            tags$br(),
            
            # ____ Summary statistics ----
            checkboxGroupInput(
              inputId = "matrix",
              label = tags$h4("Summary Statistics:"),
              choices = c("Maximum" = "Maximum",
                          "Mean" = "Mean",
                          "Minimum" = "Minimum"),
              selected = "Mean"),
            
            tags$br(),
            tags$br(),
            
            # ____ Plot types ----
            checkboxGroupInput(
              inputId = "plot_log",
              label = tags$h4("y-axis:"),
              choices = c("Log Scale" = "log"))
            
          ),
          
          shinydashboard::box(
            width = 9,
            #title = "right",
            solidHeader = FALSE,
            
            # ____ Time series plot ----
              tags$h4(p(strong("Time series plot of cyanobacterial abundance (cells/mL) of the selected waterbody. Red dashed line: World Health Organization (WHO) Recreational Use Value (RUV) Guideline for moderate probability of adverse health effects, which is 100,000 cyanobacteria cells/mL."))),
              
              tags$br(),
              
              plotlyOutput("plot_cell"),
              
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),

            # ____ Data table ----
              tags$h4(p(strong(("Time series data of the selected waterbody during the selected date range.")))),
              
              DT::dataTableOutput("table")
            
          ),
          
          # _ 4. Copyright and Contacts ----
          shinydashboard::box(
            width = 12,
            #title = "copyright",
            solidHeader = FALSE,
            
            h4("The report is provided by Oregon DEQ Watershed Management. Copyright (C) 2020-2022, ODEQ."),
            h4("The source code of this report is publicly available at GitHub repository: ", 
               a("Satellite Estimates of Cyanobacteria in Oregon Lakes and Reservoirs",
                 href="https://github.com/OR-Dept-Environmental-Quality/CyAN_imagery_update"),"."),
            h4("For more information on this report, please contact"),
            h4("Dan Sobota, ", a("dan.sobota@deq.oregon.gov",href="mailto:dan.sobota@deq.oregon.gov")),
            h4("Erin Costello, ", a("erin.costello@deq.oregon.gov",href="mailto:erin.costello@deq.oregon.gov")),
            h4("Yuan Grund, ", a("yuan.grund@deq.oregon.gov",href="mailto:yuan.grund@deq.oregon.gov"))
            
          )
          
        )
        
      )

    )
  ),
  
  server = function(input, output, session) {
    
    # (1) Plot ----
    # _ Time series plot ----
    pal.plot <- c("orange","blue","green","white","white","white")
    pal.plot <- setNames(pal.plot,unique(sort(dta$`Summary Statistics`)))

    yr <- reactive({ 
      
      if(input$ploty == "Current Year: 2022"){"2022"}else{sort(unique(dta$Year))}
      
    })
    
    df <- reactive({
      
      dta %>% 
        dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
        dplyr::filter(`Summary Statistics` %in% input$matrix) %>% 
        dplyr::filter(Year %in% c(yr())) %>% 
        dplyr::filter(Date >= input$date_plot[1],Date <= input$date_plot[2])
      
    })
    
    type <- reactive({
      
      input$plot_log
      
    })
    
    yaxis <- reactive({
      
      if_else(length(input$plot_log)>0,
              "Cyanobacteria (cells/mL)",
              "Cyanobacteria (cells/mL)")
      
    })
    
    output$plot_cell <- renderPlotly({
      
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
        plotly::layout(xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date))),
                       # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                       title = as.character(unique(df()$GNISIDNAME))) %>% 
        plotly::layout(yaxis = list(type = type(),
                                    title = yaxis())) %>% 
        plotly::add_trace(y = 100000, mode = "lines",
                          line = list(shape = 'spline', color = 'red', width = 3),
                          name = "WHO Threshold",
                          legendgroup = "who",
                          showlegend = FALSE) %>% 
        plotly::layout(annotations = list(x = max(df()$Date),
                                          y = 100000,
                                          text = "WHO Threshold",
                                          font = list(size = 12),
                                          xref = "x",
                                          yref = "y",
                                          showarrow = TRUE,
                                          arrowhead = 3,
                                          arrowsize = 1,
                                          ax = -60,
                                          ay = -20))
      
    })
    
    # (2) Tables ----
    # _ Data table ----
    df_tbl <- reactive({
      
      df() %>% 
        dplyr::select(GNISIDNAME,Date,`Cyanobacteria (cells/mL)`,`Summary Statistics`) %>% 
        dplyr::mutate(`Cyanobacteria (cells/mL)` = ifelse(`Cyanobacteria (cells/mL)` <= 6310, "Non-detect",
                                                          scales::comma(`Cyanobacteria (cells/mL)`))) %>%
        dplyr::rename(Waterbody_GNISID = GNISIDNAME)
    })
    
    output$table <- DT::renderDataTable({
      
      DT::datatable(
        data = df_tbl(),
        style = 'bootstrap',
        extensions = 'Buttons',
        options = list(dom = 'Bfrtilp',
                       pageLength = 10,
                       compact = TRUE,
                       nowrap = TRUE,
                       scorllX = TRUE,
                       buttons = list(#'print',
                         list(extend = 'collection',
                              buttons = c('csv','excel','pdf'),
                              text = 'Download')
                       )),
        rownames = FALSE,
        filter = 'bottom'
      ) #%>% 
      #DT::formatDate("Date","toLocaleString")
    }, server = FALSE)
    
    # _ 7DADM ----
    tbl.data.7days <- reactive({
      
      dta2 %>% 
        dplyr::arrange(GNISIDNAME, desc(Date)) %>% 
        dplyr::filter((as.Date(Date) <= as.Date(max(dta2$Date))) & (as.Date(Date) >= as.Date(max(dta2$Date))-7))
      
    })
    
    tbl.data <- reactive({
      
      tbl.data.7days() %>% 
        dplyr::group_by(GNISIDNAME) %>% 
        dplyr::summarise(mean_7DayMax = mean(MAX_cellsml)) %>% 
        dplyr::ungroup() %>% 
        #dplyr::left_join(tbl.data.7days(),by="GNISIDNAME") %>% 
        #dplyr::filter(mean_7DayMax == MAX_cellsml) %>% 
        dplyr::arrange(desc(mean_7DayMax)) %>% 
        dplyr::left_join(lakes.resolvable, by = "GNISIDNAME") %>% 
        dplyr::mutate(Basin = ifelse(`HU_6_NAME` == "Willamette",`HU_8_NAME`,`HU_6_NAME`)) %>% 
        dplyr::select(GNISIDNAME,Basin,mean_7DayMax) %>% 
        dplyr::distinct(GNISIDNAME, .keep_all = TRUE) %>% 
        dplyr::mutate(mean_7DayMax = ifelse(mean_7DayMax<= 6310, "Non-detect",
                                            format(round(mean_7DayMax,0),big.mark=",",scientific = FALSE))) %>% 
        #dplyr::mutate(Date = as.Date(Date,format="%Y-%b-%d")) %>% 
        dplyr::rename(Waterbody_GNISID = GNISIDNAME,
                      `7DADM (cells/mL)` = mean_7DayMax)
    })
    
    output$tbl7dadm <- DT::renderDataTable({
      
      DT::datatable(
        data = tbl.data(),
        style = 'bootstrap',
        extensions = 'Buttons',
        options = list(dom = 'Bfrtilp',
                       pageLength = 10,
                       compact = TRUE,
                       nowrap = TRUE,
                       scorllX = TRUE,
                       buttons = list(#'print',
                         list(extend = 'collection',
                              buttons = c('csv','excel','pdf'),
                              text = 'Download')
                       )),
        rownames = FALSE,
        filter = 'bottom'
      ) #%>% 
      #DT::formatDate("Date","toLocaleString")
    }, server = FALSE)
    
     # (3) Text: Drinking Water Area ----
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
    
    # (4) Image: Lake images ----
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$non_select_image <- renderText({ 
          
          "Select a waterbody to show the lake image."
          
        })
        
      } else {
        
        output$non_select_image <- renderText({})
        
        output$lakeImage <- renderImage({
          
          list(src = paste0("./Lake_Images/",input$waterbody,".jpg"),
               width = 400,
               height = 300)
          
        }, deleteFile = FALSE)
        
      }
      
    })
    
    # (5) Maps ----
    # _ initial map ----
    
    map.tbl.data <- reactive({
      
      tbl.data() %>% 
        dplyr::mutate(`7dadm` = gsub(",","",`7DADM (cells/mL)`)) %>% 
        dplyr::filter(as.numeric(`7dadm`) > 100000)
      
    })
    
    lakes.resolvable.red <- reactive({
      
      lakes.resolvable %>% dplyr::filter(GNISIDNAME %in% map.tbl.data()$`Waterbody_GNISID`)
      
    })
    
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addMapPane("OpenStreetMap", zIndex = -40) %>% 
        leaflet::addMapPane("National Geographic World Map", zIndex = -40) %>%
        leaflet::addMapPane("state.boundary", zIndex = -30) %>%
        leaflet::addMapPane("HUC6",zIndex = -20) %>% 
        leaflet::addMapPane("lakes.resolvable.red", zIndex = 400) %>%
        leaflet::addMapPane("lakes.resolvable", zIndex = 500) %>%
        leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap",
                                  options = pathOptions(pane = "OpenStreetMap")) %>% 
        leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap,group = "National Geographic World Map",
                                  options = pathOptions(pane = "National Geographic World Map")) %>% 
        leaflet::setView(lng = -120, lat = 44, zoom=7) %>% 
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addScaleBar(position = c("bottomright"),
                             options = scaleBarOptions()) %>% 
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
                             labelOptions = labelOptions(style = list("font-size" = "18px",
                                                                      "color" = "blue")),
                             options = pathOptions(pane = "lakes.resolvable")) %>% 
        leaflet::addPolygons(data = lakes.resolvable.red(), 
                             color = "red",
                             weight = 2,
                             layer = ~lakes.resolvable.red()$GNISIDNAME,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             label = ~lakes.resolvable.red()$GNIS_Name,
                             labelOptions = labelOptions(style = list("font-size" = "18px",
                                                                      "color" = "blue")),
                             options = pathOptions(pane = "lakes.resolvable.red")) %>% 
        leaflet::addPolygons(data = huc6, 
                             group = "Basins (HUC6)",
                             color = "grey",
                             weight = 2,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = ~pal.huc6(HU_6_NAME),
                             fillOpacity = 0.2,
                             label = ~huc6$HU_6_NAME,
                             labelOptions = labelOptions(noHide = TRUE,
                                                         textOnly = TRUE,
                                                         style = list("font-size" = "12px",
                                                                      "color" = "black")),
                             options = pathOptions(pane = "HUC6")) %>% 
        leaflet::addPolygons(data = state.boundary, 
                             color = "black",
                             weight = 2,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             options = pathOptions(pane = "state.boundary")) %>% 
        #leaflet::addRasterImage(rst, colors = pal.map, opacity = 1) %>% 
        leaflet::addLayersControl(baseGroups = c("OpenStreetMap","National Geographic World Map"),
                                  overlayGroups = c("Basins (HUC6)"),
                                  position = "topleft",
                                  options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
        leaflet::hideGroup(c("Basins (HUC6)"))
      
    })
    
    # _ map reactive @ waterbody picker ----
    observeEvent(input$waterbody,{
      
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
      
    })
    
    # _ 7 maps ----
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        output$non_select <- renderText({ 
          
          "Select a waterbody to show the maps."
          
        })
        
      } else {
        
        output$non_select <- renderText({})
        
        output$maps7 <- renderImage({
          
          list(src = paste0("./Report_Images/",input$waterbody,".png"))
          
        }, deleteFile = FALSE)
        
      }
      
    })
    
    }
  
) # shinyApp END
