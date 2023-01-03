library(tidyverse)
library(lubridate)
library(readxl)
library(rgdal) 
library(sf)
library(raster)
library(leaflet)
library(RColorBrewer)
library(rasterVis)
library(zoo)
library(shiny)

# Get update of NASA data----
# Need to have ArcPro on your machine; modify path in the script to point to the correct version of python
source("Update_NASA_imagery.R")
source("dbf2excel.R")

# ---
pause = function()
{
  if (interactive())
  {
    invisible(readline(prompt = "Press <Enter> to generate thumbnail maps, which will take about 2 hours; Or press <Esc> to stop."))
  }
  else
  {
    cat("Press <Enter> to generate thumbnail maps, which will take about 2 hours; Or press <Esc> to stop.")
    invisible(readLines(file("stdin"), 1))
  }
}
#---

# Get update of NASA data----
# Need to have ArcPro on your machine; modify path in the script to point to the correct version of python
# source("Update_NASA_imagery.R")

# Project data folder @ sharedrive:
data.path <- "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app"

# (1) Timeseries Data Table ----
dta1 <- readxl::read_xlsx(paste0(data.path,"./data/Resolvable_Lakes.xlsx"), sheet = "cyan_resolvable_lakes")

dta2 <- readxl::read_xlsx(paste0(data.path,"./data/HAB_resolvablelakes_2023.xlsx"), sheet = "HAB_resolvable_lake_data") %>% 
  dplyr::filter(GNISIDNAME %in% dta1$inApp) # filter out saline lakes

dta3 <- readxl::read_xlsx(paste0(data.path,"./data/HAB_resolvablelakes_2016_2022.xlsx"), sheet = "HAB_resolvablelakes_2016_2022") %>% 
  dplyr::filter(GNISIDNAME %in% dta1$inApp) # filter out saline lakes

dta <- rbind(dta2[,c(1:11)],dta3[,c(1:11)]) %>% 
  dplyr::rename(Mean = MEAN_cellsml,
                Maximum = MAX_cellsml,
                Minimum = MIN_cellsml) %>% 
  tidyr::gather(`Summary Statistics`, `Cyanobacteria (cells/mL)`, -GNISIDNAME,-COUNT,-AREA,-Day,-Year,-Date) %>% 
  tidyr::separate(GNISIDNAME,c("GNISNAME","GNISID"), sep="_") %>% 
  dplyr::mutate(GNISIDNAME = paste0(GNISNAME,"_",GNISID)) %>% 
  dplyr::mutate(Date = lubridate::ymd(Date)) %>% 
  dplyr::arrange(desc(Date)) %>% 
  dplyr::mutate(wi_DWSA = ifelse(GNISIDNAME %in% dta1$wi_DWSA, "Yes", "No"))

# (2) Date Lookup Table ----
fulldays <- readxl::read_xlsx(paste0(data.path,"./data/calendar-dates.xlsx"),
                              sheet = "calendar-dates") %>% 
  dplyr::mutate(Date = lubridate::ymd(Date))

lookup.date <- dta %>% 
  dplyr::group_by(Date, Year, Day) %>% 
  dplyr::summarise(n=n()) %>% 
  dplyr::right_join(fulldays, by="Date") %>% 
  dplyr::rename(Year.dta = Year.x,
                Day.dta = Day.x,
                Year.fulldays = Year.y,
                Day.fulldays = Day.y)

missing.dates <- lookup.date %>% 
  dplyr::filter(is.na(Day.dta))

# (3) Map: shapefiles ----
lakes.resolvable <- sf::st_read(dsn = paste0(data.path,"/data/CyAN_Waterbodies.shp"),
                                layer = "CyAN_Waterbodies") %>% 
  sf::st_zm() %>% 
  st_transform(crs = 4326) %>% 
  dplyr::filter(GNISIDNAME %in% dta1$inApp) # filter out saline lakes

state.boundary <- sf::st_read(paste0(data.path,"/data/state_boundary_blm.shp")) %>% 
  st_transform(crs = 4326)

huc6 <- sf::st_read(dsn = paste0(data.path,"/data/WBD_HU6.shp"), layer = "WBD_HU6") %>% 
  st_transform(crs = 4326)

pal.huc6 <- leaflet::colorFactor(palette = c(RColorBrewer::brewer.pal(name="BrBG", n = 9), RColorBrewer::brewer.pal(name="Paired", n = 9)), domain = unique(sort(huc6$HU_6_NAME)))

# (4) Map: raster ----
# Raster color 
thevalues <- c(0,6310,20000,100000,7000000)
#paletteFunc <- grDevices::colorRampPalette(c('#bdbdbd','#66c2a4','#2ca25f','#006d2c'))
#palette     <- paletteFunc(4)
palette <- c('#bdbdbd','#66c2a4','#2ca25f','#006d2c')

pal.map <- leaflet::colorBin(palette = palette,
                             bins = c(0,6310,20000,100000,7000000),
                             domain = c(0,6310,20000,100000,7000000),
                             na.color = "transparent")
# Legend labels
labels = c("Non-detect","Low: 6,311 - 20,000","Moderate: 20,000 - 100,000","High: >100,000")

# (5) 7DADM Table  ----
tbl.data.7days <- dta2 %>% 
  dplyr::arrange(GNISIDNAME, desc(Date)) %>% 
  dplyr::filter((as.Date(Date) <= as.Date(max(dta2$Date))) & (as.Date(Date) >= as.Date(max(dta2$Date))-6))

no.data <- dta1 %>% 
  dplyr::filter(!is.na(inApp)) %>% 
  dplyr::filter(!GNIS_Name_ID %in% tbl.data.7days$GNISIDNAME) %>% 
  dplyr::mutate(mean_7DayMax = "No Data Available") %>% 
  dplyr::rename(GNISIDNAME = GNIS_Name_ID) %>% 
  dplyr::select(GNISIDNAME,mean_7DayMax)

tbl.data <- tbl.data.7days %>% 
  dplyr::group_by(GNISIDNAME) %>% 
  dplyr::summarise(mean_7DayMax = mean(MAX_cellsml),
                   n=n()) %>%    # 7 day average daily maximum
  #dplyr::summarise(mean_7DayMax = exp(mean(log(MAX_cellsml)))) %>% # geomean
  #dplyr::summarise(`7DMC` = max(MAX_cellsml)) %>%          # 7 day maximum composite
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(mean_7DayMax)) %>% 
  dplyr::mutate(mean_7DayMax = ifelse(mean_7DayMax<= 6310, "Non-detect",
                                      format(round(mean_7DayMax,0),big.mark=",",scientific = FALSE))) %>% 
  #rbind(no.data) %>% 
  dplyr::left_join(lakes.resolvable, by = "GNISIDNAME") %>% 
  dplyr::mutate(Basin = ifelse(`HU_6_NAME` == "Willamette",`HU_8_NAME`,`HU_6_NAME`)) %>% 
  dplyr::select(GNISIDNAME,Basin,mean_7DayMax,n) %>% 
  dplyr::distinct(GNISIDNAME, .keep_all = TRUE) %>% 
  #dplyr::mutate(Date = as.Date(Date,format="%Y-%b-%d")) %>% 
  dplyr::rename(`Waterbody_GNISID*` = GNISIDNAME,
                `7DADM (cells/mL)` = mean_7DayMax,
                `Days of Data` = n)

map.tbl.data <- tbl.data %>% 
  dplyr::filter(!`7DADM (cells/mL)` %in% c("Non-detect","No Data Available")) %>% 
  dplyr::mutate(`7dadm` = gsub(",","",`7DADM (cells/mL)`)) %>% 
  dplyr::filter(as.numeric(`7dadm`) > 100000) %>% 
  dplyr::select(-`7dadm`)

# (6) 7DADM Map  ----
lakes.resolvable.7dadm <- lakes.resolvable %>% 
  dplyr::mutate(`7dadm` = ifelse(GNISIDNAME %in% map.tbl.data$`Waterbody_GNISID*`,"Waterbody with high cyanobacteria abundance","Others")) %>% 
  dplyr::filter(!`7dadm` == "Others")

#Create a palette function, using the selected color
#palette7dadm <- leaflet::colorFactor(palette = c('red','blue'), 
#                                     domain = unique(sort(lakes.resolvable.7dadm$`7dadm`)))

palette7dadm <- leaflet::colorFactor(palette = c('#006d2c'), 
                                     domain = unique(sort(lakes.resolvable.7dadm$`7dadm`)))

palette7dadm_lg <- leaflet::colorFactor(palette = c('#00441b'), 
                                        domain = unique(sort(lakes.resolvable.7dadm$`7dadm`)))

# Save data ----
#rm(dta1); rm(dta2); rm(dta3)
save.image(file = "data.RData")

#pause()

# (7) Thumbnail maps ----
tag.map.date <- shiny::tags$style(HTML("
  .leaflet-control.map-date { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 0%;
    text-align: left;
    padding-left: 5px; 
    padding-right: 5px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 40px;
  }
"))

# require: thevalues; paletteFunc; palette; pal.map

# Data subset for testing:
#subset <- lakes.resolvable[which(lakes.resolvable$GNISIDNAME %in% c("Upper Klamath Lake_01151685", "Detroit Lake_01639301")),]
waterbody.list <- sort(unique(lakes.resolvable$GNISIDNAME))

# Dates:
# require: fulldays; lookup.date; tbl.data.7days
last7days <- lookup.date %>%
  dplyr::filter(Date %in% as.Date(c((today()-7):(today()-1)))) %>%
  dplyr::arrange(Day.fulldays) %>%
  dplyr::pull(Date)

# when last7days need to be manually defined. !! Remember to check/save map_file_name.csv.
# last7days <- c("2022-05-23","2022-05-24","2022-05-25","2022-05-26","2022-05-27","2022-05-28","2022-05-29")
# last7days <- c("2022-07-10")

map.file.name <- data.frame(File_waterbody = character(),
                            File_name = character())

# _ images ----
# This for loop will take more than 2 hours to run.
for (x in 1:length(waterbody.list)){
  
  # test: x <- 1
  # test: y <- 1
  
  print(waterbody.list[x])
  
  map.file.name.x <- data.frame(File_waterbody = character(),
                                File_name = character())
  
  for(y in 1:length(last7days)){
    
    print(last7days[y])
    
    title.date <- tags$div(tag.map.date, HTML(paste0(last7days[y])))
    
    one.lake <- lakes.resolvable[which(lakes.resolvable$GNISIDNAME == waterbody.list[x]),]
    
    bounds <- sf::st_bbox(one.lake)
    
    df.map.date <- lookup.date %>% 
      dplyr::filter(Date %in% as.Date(last7days[y])) %>% 
      dplyr::mutate(Day.dta = ifelse(Day.fulldays < 10, paste0("00",as.character(Day.fulldays)),
                                     ifelse((Day.fulldays >= 10 & Day.fulldays < 100), paste0("0",as.character(Day.fulldays)), Day.fulldays))) %>% 
      dplyr::mutate(map_day = paste0(Year.fulldays,Day.dta))
    
    tif.dir <- paste0("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/", df.map.date$Year.fulldays, "/")
    file.name <- paste0(df.map.date$map_day,".tif")
    
    rst <- raster::raster(paste0(tif.dir,file.name))
    
    map <- leaflet::leaflet() %>% 
      leaflet::addControl(title.date, position = "topleft", className="map-date")%>% 
      leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap") %>% 
      leaflet::addRasterImage(rst, layerId = "Value", project = FALSE, colors=pal.map, opacity = 1) %>% 
      #leaflet::addLegend(pal = pal.map, values = thevalues, title = "Cyanobacteria (cells/mL)", position = "bottomright") %>% 
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
                                                                    "color" = "blue"))) %>% 
      leaflet::fitBounds(lng1=bounds[[1]], lat1=bounds[[2]], lng2=bounds[[3]], lat2=bounds[[4]])
    
    #mapview::mapshot(map, file = paste0("./Images/",waterbody.list[x],"-",last7days[y],".png")) # somehow mapshot stops working
    
    htmlwidgets::saveWidget(map, file = paste0("./Images/",waterbody.list[x],"-",last7days[y],".html"),selfcontained=TRUE)
    webshot::webshot(url = paste0("./Images/",waterbody.list[x],"-",last7days[y],".html"), 
                     file = paste0("./Images/",waterbody.list[x],"-",last7days[y],".jpg"))
    
    map.file.name.y <- paste0("./Images/",waterbody.list[x],"-",last7days[y],".jpg")
    
    map.file.name.x <- map.file.name.x %>% 
      dplyr::add_row(File_waterbody = waterbody.list[x],
                     File_name = map.file.name.y)
    
  }
  
  map.file.name <- rbind(map.file.name,map.file.name.x)
  
}

#write.csv(map.file.name,"map_file_name_Sunday.csv")
write.csv(map.file.name,"map_file_name.csv")

file.remove(file.path("./Images/",dir(path = "./Images",pattern="*.html")))
unlink(file.path("./Images/",dir(path = "./Images",pattern="_files")),recursive = TRUE)

# _ report images ----
library(magick)

map.file.name <- read.csv("map_file_name.csv")

for(i in sort(unique(map.file.name$File_waterbody))){
  
  # test: i <- "Odell Lake_01147159"
  
  df.imgs <- map.file.name %>% dplyr::filter(File_waterbody == i)
  img.files <- c(df.imgs$File_name[1:7],"./Report_Images/legend/legend.png")
  imgs <- magick::image_read(img.files)
  #imgs.comb <- image_montage(imgs, tile = '3x3', geometry = "x200+3+5")
  imgs.comb <- magick::image_montage(imgs, tile = '4x2', geometry = "300x200+1+1")
  magick::image_write(imgs.comb, path = paste0("./Report_Images/",i,".jpg"), format = "jpg")
  print(paste0("./Report_Images/",i,".jpg"))
  
}

# Save data ----
#rm(dta1); rm(dta2); rm(dta3)
save.image(file = "data.RData")
