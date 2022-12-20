library(dplyr)
library(foreign)
library(readxl)
library(writexl)

thetable <- "\\\\deqhq1\\wq-share\\Harmful Algal Blooms Coordination Team\\HAB_Shiny_app\\data\\HAB_resolvablelakes_2022.xlsx"
stats_dir <- py$stats_dir
thestatsname <- py$thestatsname
hab_days_length <- py$hab_days_length

dbf <- NULL
for(i in 1:hab_days_length){
  print(thestatsname[i])
  # test: i=2
  dbf.i <- foreign::read.dbf(paste0(stats_dir,"\\",thestatsname[i]))
  dbf <- dplyr::bind_rows(dbf,dbf.i)
}

dbf_update <- dbf %>% 
  dplyr::rename(MIN_cellsml = MIN, 
                MAX_cellsml = MAX, 
                RANGE_cellsml = RANGE, 
                MEAN_cellsml = MEAN,
                STD_cellsml = STD) %>% 
  dplyr::mutate(COUNT = as.numeric(COUNT),
                Day = as.numeric(Day),
                Year = as.numeric(Year),
                Date = lubridate::as_date(Date))

tbl <- readxl::read_excel(thetable,sheet = "HAB_resolvable_lake_data") %>% 
  dplyr::mutate(Date = lubridate::as_date(Date))

tbl_update <- dplyr::bind_rows(tbl,dbf_update)

writexl::write_xlsx(list(HAB_resolvable_lake_data = tbl_update), path=thetable)
