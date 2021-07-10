library(tidyverse)

lakes.resolvable <- rgdal::readOGR(dsn = "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/NHDwaterbody_resolvable_lakes_dissolved_oregon_clean_huc6.shp",
                                   layer = "NHDwaterbody_resolvable_lakes_dissolved_oregon_clean_huc6")

dta2 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/HAB_resolvablelakes_2021.xlsx",
                          sheet = "HAB_resolvable_lake_data") %>% 
  dplyr::filter(!GNISIDNAME == "Goose Lake_01520146") %>% # located in the WA state
  dplyr::filter(GNISIDNAME %in% unique(sort(lakes.resolvable@data$GNISIDNAME)))

dta3 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/Resolvable_Lakes.xlsx",
                          sheet = "cyan_resolvable_lakes")

GNISNameID <- unique(sort(dta3$wi_DWSA))

dta <- dta2 %>% 
  dplyr::rename(Mean = MEAN_cellsml,
                Maximum = MAX_cellsml) %>% 
  tidyr::gather(`Summary Statistics`, `Cyanobacteria (cells/mL)`, -GNISIDNAME,-COUNT,-AREA,-PercentArea_Value, -Day,-Year,-Date) %>% 
  tidyr::separate(GNISIDNAME,c("GNISNAME","GNISID"), sep="_") %>% 
  dplyr::mutate(GNISIDNAME = paste0(GNISNAME,"_",GNISID)) %>% 
  dplyr::mutate(Date = lubridate::ymd(Date)) %>% 
  dplyr::arrange(desc(Date)) %>% 
  dplyr::mutate(wi_DWSA = ifelse(GNISIDNAME %in% GNISNameID, "Yes", "No")) %>% 
  dplyr::filter(`Summary Statistics` %in% c("Mean","Maximum"))

# OR report ----
tbl.data.7days <- dta2 %>% 
  dplyr::arrange(GNISIDNAME, desc(Date)) %>% 
  dplyr::filter(as.Date(Date) <= as.Date(max(dta2$Date)) & as.Date(Date) >= as.Date(max(dta2$Date))-7)

#tbl.max.of.daily.mean <- tbl.data.7days %>% 
#  dplyr::group_by(GNISIDNAME) %>% 
#  dplyr::summarise(max_7DayMean = max(MEAN_cellsml)) %>% 
#  dplyr::ungroup() %>% 
#  dplyr::left_join(tbl.data.7days,by="GNISIDNAME") %>% 
#  dplyr::filter(max_7DayMean == MEAN_cellsml) %>% 
#  dplyr::arrange(desc(max_7DayMean)) %>% 
#  dplyr::left_join(lakes.resolvable@data, by = "GNISIDNAME") %>% 
#  dplyr::mutate(Basin = ifelse(Name_1 == "Willamette",Name,Name_1)) %>% 
#  dplyr::select(GNISIDNAME,Basin,Date,max_7DayMean) %>% 
#  dplyr::distinct(GNISIDNAME, .keep_all = TRUE) %>% 
#  dplyr::mutate(max_7DayMean = format(round(max_7DayMean,0),big.mark=",",scientific = FALSE)) %>% 
#  dplyr::rename(Waterbody_GNISID = GNISIDNAME,
#                `Maximum 7 Daily Mean (cells/mL)` = max_7DayMean)

tbl.mean.of.daily.max <- tbl.data.7days %>% 
  dplyr::group_by(GNISIDNAME) %>% 
  dplyr::summarise(mean_7DayMax = mean(MAX_cellsml)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(desc(mean_7DayMax)) %>% 
  dplyr::left_join(lakes.resolvable@data, by = "GNISIDNAME") %>% 
  dplyr::mutate(Basin = ifelse(Name_1 == "Willamette",Name,Name_1)) %>% 
  dplyr::select(GNISIDNAME,Basin,mean_7DayMax) %>% 
  dplyr::distinct(GNISIDNAME, .keep_all = TRUE)  

num <- nrow(tbl.mean.of.daily.max[which(tbl.mean.of.daily.max$mean_7DayMax>=100000),])

tbl.7dmdm <- tbl.mean.of.daily.max %>%
  dplyr::mutate(mean_7DayMax = ifelse(mean_7DayMax<= 6310, "Non-detect",
                                      format(round(mean_7DayMax,0),big.mark=",",scientific = FALSE))) %>% 
  dplyr::rename(Waterbody_GNISID = GNISIDNAME,
                `7-Day Average Daily Maximum (cells/mL)` = mean_7DayMax)

#plot.dta <- dta2 %>% 
#  dplyr::rename(Mean = MEAN_cellsml,
#                Maximum = MAX_cellsml,
#                Minimum = MIN_cellsml) %>% 
#  tidyr::gather(`Summary Statistics`, `Cyanobacteria (cells/mL)`, -GNISIDNAME,-COUNT,-AREA,-PercentArea_Value,-RANGE_cellsml,-STD_cellsml,-Day,-Year,-Date) %>% 
#  tidyr::separate(GNISIDNAME,c("GNISNAME","GNISID"), sep="_") %>% 
#  dplyr::mutate(GNISIDNAME = paste0(GNISNAME,"_",GNISID)) %>% 
#  dplyr::mutate(Date = lubridate::ymd(Date)) %>% 
#  dplyr::arrange(desc(Date)) %>% 
#  dplyr::mutate(wi_DWSA = ifelse(GNISIDNAME %in% GNISNameID, "Yes", "No"))

# cyano.max <- max(plot.dta$log_cyano)

gnisidname <- unique(sort(dta2$GNISIDNAME))


caption_1 <- paste0("Top 10 waterbodies ranked by the 7-Day Average Daily Maximum of cyanobacteria abundance (cells/mL) during the 7 days from ", 
                    as.Date(max(dta2$Date))-7, " to ",max(dta2$Date), 
                    ". The basin names are shown in the table.")

caption_2 <- paste0("Waterbodies ranked by the 7-Day Average Daily Maximum of cyanobacteria abundance (cells/mL) ",
                    "that are above the WHO guideline (100,000 cells/mL) for cyanobacteria in recreational freshwater during the 7 days from ", 
                    as.Date(max(dta2$Date))-7, " to ",max(dta2$Date), 
                    ". The basin names are shown in the table.")

caption_3 <- paste0("Waterbodies ranked by the 7-Day Average Daily Maximum of cyanobacteria abundance (cells/mL) ",
                    "that are above the WHO guideline (100,000 cells/mL) for cyanobacteria in recreational freshwater during the 7 days from ", 
                    as.Date(max(dta2$Date))-7, " to ",max(dta2$Date), 
                    ". The basin names are shown in the table. ",
                    "The waterbodies, which 7-Day Average Daily Maximum of cyanobacteria abundance are less than 6310 cells/mL ",
                    "(the satellite detection threshold value), are not included in the table.")

email.address_OR <- "DEQ [WQ] Harmful Algal Blooms Coordination Team <_WQ_HABsCoordinationTeam-1-471932883@deq.state.or.us>; 
Hillwig Rebecca <Rebecca.Hillwig@dhsoha.state.or.us>; 
Cude Curtis G <CURTIS.G.CUDE@dhsoha.state.or.us>; 
Hofeld Evan E <EVAN.E.HOFELD@dhsoha.state.or.us>; 
Baird Gregg C <GREGG.C.BAIRD@dhsoha.state.or.us>; 
Labiosa, Rochelle <labiosa.rochelle@epa.gov>; 
Compton, Jana <Compton.Jana@epa.gov>; 
Handler, Amalia <Handler.Amalia@epa.gov>; 
Carpenter, Kurt <kdcar@usgs.gov>; 
Brian Fulfrost <bfaconsult@gmail.com>;
Lundell, Tina M CIV USARMY CENWP (USA) <Tina.M.Lundell@usace.army.mil>;
Buccola, Norman L (Norm) CIV USARMY CENWP (USA) <Norman.L.Buccola@usace.army.mil>;
Bellringer, Holly H CIV USARMY CENWP (USA) <Holly.H.Bellringer@usace.army.mil>"

email.address_ER <- "BUTCHER Don * DEQ <don.butcher@deq.state.or.us>; 
DADOLY John * DEQ <john.dadoly@deq.state.or.us>; 
HIATT Mike * DEQ <mike.hiatt@deq.state.or.us>; 
MEHTA Smita * DEQ <smita.mehta@deq.state.or.us>; 
STOKEN Olivia * DEQ <olivia.stoken@deq.state.or.us>"

email.address_NWR <- "MATZKE Andrea * DEQ <andrea.matzke@deq.state.or.us>; 
CREUTZBURG Brian * DEQ <brian.creutzburg@deq.state.or.us>; 
NAYAR Roxy * DEQ <roxy.nayar@deq.state.or.us>; 
JOHNSON York * DEQ <york.johnson@deq.state.or.us>"

email.address_WR <- "MEYERS Bill * DEQ <bill.meyers@deq.state.or.us>; 
DUGGAN Bryan * DEQ <bryan.duggan@deq.state.or.us>; 
WALTZ David * DEQ <david.waltz@deq.state.or.us>; 
TUGAW Heather * DEQ <heather.tugaw@deq.state.or.us>; 
GRAMLICH Nancy H * DEQ <nancy.h.gramlich@deq.state.or.us>; 
WOOLVERTON Priscilla * DEQ <priscilla.woolverton@deq.state.or.us>;
GIRARD Kendra * DEQ <kendra.girard@deq.state.or.us>;
SAUTER Sarah * DEQ <sarah.sauter@deq.state.or.us>;
FERN Jacqueline * DEQ <jacqueline.fern@deq.state.or.us>"
  
email.address_cc <- "GRUND Yuan * DEQ <yuan.grund@deq.state.or.us>; 
COSTELLO Erin * DEQ <erin.costello@deq.state.or.us>; 
ADHAR Ratnanjali * DEQ <Ratnanjali.ADHAR@deq.state.or.us>; 
SOBOTA Daniel * DEQ <daniel.sobota@deq.state.or.us>"

save(lakes.resolvable,
     dta,
     dta2,
     tbl.data.7days,
     #tbl.max.of.daily.mean,
     tbl.mean.of.daily.max,
     num,
     tbl.7dmdm,
     gnisidname,
     caption_1,
     caption_2,
     caption_3,
     email.address_OR,
     email.address_ER,
     email.address_NWR,
     email.address_WR,
     email.address_cc,
     file = "report.RData")
