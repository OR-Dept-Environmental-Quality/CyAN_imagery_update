library(rmarkdown)
library(tidyverse)

# OR Report ----
rmarkdown::render(input="reportOR.Rmd",
                  output_format = "word_document",
                  output_dir = "./Reports/",
                  output_file= "OR_Report.docx")

# Region Reports ----
region <- c("Eastern Region","Northwest Region","Western Region")
bc <- read.csv("bc.csv")

for (r in region) {
  
  # test: 
  # r <- "Eastern Region"
  # r <- "Northwest Region"
  # r <- "Western Region"
  
  print(r)
  load("report.RData")
  
  tbl.data <- tbl.data.7days %>% 
    dplyr::group_by(GNISIDNAME) %>% 
    dplyr::summarise(max_7DayMean = max(MEAN_cellsml)) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(tbl.data.7days,by="GNISIDNAME") %>% 
    dplyr::filter(max_7DayMean == MEAN_cellsml) %>% 
    dplyr::arrange(desc(max_7DayMean)) %>% 
    dplyr::left_join(lakes.resolvable@data, by = "GNISIDNAME") %>% 
    dplyr::filter(Region == r) %>% # Region selection
    dplyr::mutate(Basin = ifelse(Name_1 == "Willamette",Name,Name_1)) %>% 
    dplyr::select(GNISIDNAME,Basin,Date,max_7DayMean) %>% 
    dplyr::distinct(GNISIDNAME, .keep_all = TRUE) %>% 
    dplyr::mutate(max_7DayMean = ifelse(max_7DayMean <= 6310, "Non-detect",
                                        format(round(max_7DayMean,0),big.mark=",",scientific = FALSE))) %>% 
    dplyr::rename(Waterbody_GNISID = GNISIDNAME,
                  `Maximum 7 Daily Mean` = max_7DayMean)
  
  dta2 <- dta2 %>% 
    dplyr::filter(GNISIDNAME %in% tbl.data$Waterbody_GNISID) 
  
  gnisidname <- unique(sort(dta2$GNISIDNAME))
  
  bc_region <- bc %>% dplyr::filter(REGION == r)
  
  save(r,
       bc_region,
       dta2,
       tbl.data,
       gnisidname,
       file = "report_region.RData")
  
  rmarkdown::render(input="reportRegions.Rmd",
                    output_format = "word_document",
                    output_dir = "./Reports/",
                    output_file= paste0(r,"_Report.docx"))
  
}
