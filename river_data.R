library(httr)
library(rvest)
library(readr)
library(lubridate)
library(tidyverse)
library(dataRetrieval)
library(viridis)
library(viridisLite)

#Mean Daily Flow
# 1. Compute date range
# end_date   <- Sys.Date()
# start_date <- end_date - 7
# fmt        <- function(x) format(x, "%m/%d/%Y 12:00:00 AM")
# # 2. Build CSV URL (ensuring zero‑padding and %20)
# flow_url_csv <- paste0(
#   "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
#   "?station_nbr=13317850",
#   "&start_date=", URLencode(fmt(start_date)),
#   "&end_date=",   URLencode(fmt(end_date)),
#   "&dataset=MDF",
#   "&format=csv"
# )
# # 3. Read it in one shot
# flow_tbl <- read.csv(flow_url_csv, sep="\t", row.names=NULL)
# 4. Inspect
#print(head(flow_tbl))



#instantaneous flow Data

#Set moving target date for the past week
end_date   <- Sys.Date()
start_date <- end_date - 7
fmt        <- function(x) format(x, "%m/%d/%Y 12:00:00 AM")

#UGR Clear Creek
# 2. Build CSV URL (ensuring zero‑padding and %20)
inst_flow_url_ugr <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13317850",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=Instantaneous_Flow",
  "&format=csv"
)

#UGR Clear Creek temperature data
inst_temp_url_ugr <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13317850",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=WTEMP15",
  "&format=html&units=C"
)
ugr_inst_temp <- read.csv(inst_temp_url_ugr, sep="\t", row.names=NULL)

ugr_inst_temp <- ugr_inst_temp |>
  rename(Date = record_date,
         Temp_C = instantaneous_water_temp_C.)
ugr_inst_temp <- ugr_inst_temp |>
  mutate(Date = mdy_hm(Date, tz = "America/Los_Angeles"))

ugr_temp_plot <- ggplot(ugr_inst_temp, aes(x=Date, y=Temp_C, color = Temp_C)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_c(option = "H", name = "Temperature (°C)") +
  theme_minimal()
ugr_temp_plot

# 3. Read it in one shot
ugr_inst_flow <- read.csv(inst_flow_url_ugr, sep="\t", row.names=NULL)
ugr_inst_flow <- ugr_inst_flow |>
  rename(CFS = record_date,
         Date = station_nbr)
ugr_inst_flow <- ugr_inst_flow |>
  mutate(Date = mdy_hm(Date, tz = "America/Los_Angeles"))
ugr_flow_plot <- ggplot(ugr_inst_flow, aes(x=Date, y= CFS)) +
  geom_line(color = "blue")
ugr_flow_plot


#Meadow Creek below Dark Canyon
inst_flow_url_mdc <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13318210",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=Instantaneous_Flow",
  "&format=csv"
)
mdc_inst_flow <- read.csv(inst_flow_url_mdc, sep="\t", row.names=NULL)
mdc_inst_flow <- mdc_inst_flow |>
  rename(CFS = record_date, #Rename flows to CFS
         Date = station_nbr) #Rename station_nbr to Date
mdc_inst_flow <- mdc_inst_flow |>
  mutate(Date = mdy_hm(Date, tz = "America/Los_Angeles")) #Assign date to POSIXct format
mdc_flow_plot <- ggplot(mdc_inst_flow, aes(x=Date, y=CFS)) +
  geom_line(color="blue")
mdc_flow_plot

#Meadow Creek below Dark Canyon temperature data
inst_temp_url_mdc <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13318210",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=WTEMP15",
  "&format=html&units=C"
)
mdc_inst_temp <- read.csv(inst_temp_url_mdc, sep="\t", row.names=NULL)

mdc_inst_temp <- mdc_inst_temp |>
  rename(Date = record_date,
         Temp_C = instantaneous_water_temp_C.)
mdc_inst_temp <- mdc_inst_temp |>
  mutate(Date = mdy_hm(Date, tz = "America/Los_Angeles"))

mdc_temp_plot <- ggplot(mdc_inst_temp, aes(x=Date, y=Temp_C, color = Temp_C)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_c(option = "H", name = "Temperature (°C)") +
  theme_minimal()
mdc_temp_plot

#Lostine at Baker Rd
inst_flow_url_lostine <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13330300",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=Instantaneous_Flow",
  "&format=csv"
)
los_inst_flow <- read.csv(inst_flow_url_lostine, sep="\t", row.names=NULL)
los_inst_flow <- los_inst_flow |>
  rename(CFS = record_date,
         Date = station_nbr,
         Lostine_Baker_Rd = row.names)
los_inst_flow <- los_inst_flow %>%
  mutate(
    Date = mdy_hm(Date, tz = "America/Los_Angeles"))

lostine_flow_plot <-ggplot(los_inst_flow, aes(x=Date,y=CFS)) +
  geom_line(color="blue")
lostine_flow_plot

#Lostine near Lostine River Ranch (Water & Temp data) 
#River data
inst_flow_url_lostine_ranch <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13330000",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=Instantaneous_Flow",
  "&format=csv"
)
los_ranch_inst_flow <- read.csv(inst_flow_url_lostine, sep="\t", row.names=NULL)
los_ranch_inst_flow <- los_ranch_inst_flow |>
  rename(CFS = record_date,
         Date = station_nbr,
         Lostine_Ranch)
los_ranch_inst_flow <- los_ranch_inst_flow %>%
  mutate(
    Date = mdy_hm(Date, tz = "America/Los_Angeles"))
los_ranch_flow_plot <-ggplot(los_ranch_inst_flow, aes(x=Date,y=CFS)) +
  geom_line(color="blue")
los_ranch_flow_plot

#Temperature data
inst_temp_url_los <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13330000",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=WTEMP15",
  "&format=html&units=C"
)
los_ranch_inst_temp <- read.csv(inst_temp_url_los, sep="\t", row.names=NULL)
los_ranch_inst_temp <- los_ranch_inst_temp |>
  rename(Date = record_date,
         Temp_C = instantaneous_water_temp_C.)
los_ranch_inst_temp <- los_ranch_inst_temp |>
  mutate(Date = mdy_hm(Date, tz = "America/Los_Angeles"))

los_ranch_temp <- ggplot(los_ranch_inst_temp, aes(x=Date, y=Temp_C, color = Temp_C)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_c(option = "H", name = "Temperature (°C)") +
  theme_minimal()
los_ranch_temp

#Catherine Creek near Union
inst_flow_url_cc <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13320000",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=Instantaneous_Flow",
  "&format=csv"
)
cc_inst_flow <- read.csv(inst_flow_url_cc, sep="\t", row.names=NULL)
cc_inst_flow <- cc_inst_flow |>
  rename(CFS = record_date,
         Date = station_nbr)
cc_inst_flow <- cc_inst_flow |>
  mutate(Date = mdy_hm(Date, tz = "America/Los_Angeles"))
cc_flow_plot <- ggplot(cc_inst_flow, aes(x=Date, y = CFS)) +
  geom_line(color="blue")
cc_flow_plot

#Catherine Creek near Union temperature data
inst_temp_url_cc <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13320000",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=WTEMP15",
  "&format=html&units=C"
)
cc_inst_temp <- read.csv(inst_temp_url_cc, sep="\t", row.names=NULL)
cc_inst_temp <- cc_inst_temp |>
  rename(Date = record_date,
         Temp_C = instantaneous_water_temp_C.)
cc_inst_temp <- cc_inst_temp |>
  mutate(Date = mdy_hm(Date, tz = "America/Los_Angeles"))
cc_temp_plot <- ggplot(cc_inst_temp, aes(x=Date, y=Temp_C, color = Temp_C)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_c(option = "H", name = "Temperature (°C)") +
  theme_minimal()
cc_temp_plot

#Minam - Mean Daily Flow only
site_id <-13331500
parameter_code <- c(00060,00010)
usgs_time <- as.character(Sys.Date()-7)
inst_flow_url_minam <- read_waterdata_latest_continuous(
  monitoring_location_id =  "USGS-13331500",
  parameter_code =  c("00060","00010"),
  time = "P7D",
  skipGeometry = TRUE)
head(inst_flow_url_minam)

inst_flow_url_minam <- readNWISuv(
  siteNumbers = "13331500",
  parameterCd = c("00060","00010"),
  startDate = start_date,
  endDate = end_date
)
inst_flow_url_minam <- renameNWISColumns(inst_flow_url_minam)
inst_flow_url_minam <- inst_flow_url_minam |>
  rename(Date = dateTime,
         Temp_C = Wtemp_Inst,
         CFS = Flow_Inst)
minam_flow <- inst_flow_url_minam |>
  select(site_no, Date, CFS)
minam_temp <- inst_flow_url_minam |>
  select(site_no, Date, Temp_C)

minam_flow_plot <- ggplot(minam_flow, aes(x=Date, y = CFS)) +
  geom_line(color="blue")
minam_flow_plot

minam_temp_plot <- ggplot(minam_temp, aes(x=Date, y=Temp_C, color = Temp_C)) +
  geom_line(linewidth = 1) +
  scale_color_viridis_c(option = "H", name = "Temperature (°C)") +
  theme_minimal()
minam_temp_plot



saveRDS(ugr_inst_flow, "data/ugr_flow.rds")
saveRDS(ugr_inst_temp, "data/ugr_temp.rds")
saveRDS(mdc_inst_flow, "data/mdc_flow.rds")
saveRDS(mdc_inst_temp, "data/mdc_temp.rds")
saveRDS(cc_inst_flow, "data/cc_flow.rds")
saveRDS(cc_inst_temp, "data/cc_temp.rds")
saveRDS(los_inst_flow, "data/los_baker_rd_flow.rds")
saveRDS(los_ranch_inst_flow, "data/los_ranch_flow.rds")
saveRDS(los_ranch_inst_temp, "data/los_ranch_temp.rds")
saveRDS(minam_flow, "data/minam_flow.rds")
saveRDS(minam_temp, "data/minam_temp.rds")


