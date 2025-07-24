library(httr)
library(rvest)
library(readr)
library(lubridate)

#Mean Daily Flow
# 1. Compute date range
end_date   <- Sys.Date()
start_date <- end_date - 7
fmt        <- function(x) format(x, "%m/%d/%Y 12:00:00 AM")

# 2. Build CSV URL (ensuring zero‑padding and %20)
flow_url_csv <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13317850",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=MDF",
  "&format=csv"
)

# 3. Read it in one shot
flow_tbl <- read.csv(flow_url_csv, sep="\t", row.names=NULL)

# 4. Inspect
print(head(flow_tbl))



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
         Date = station_nbr)
los_inst_flow <- los_inst_flow %>%
  mutate(
    Date = mdy_hm(Date, tz = "America/Los_Angeles"))

lostine_flow_plot <-ggplot(los_inst_flow, aes(x=Date,y=CFS)) +
  geom_line(color="blue")
lostine_flow_plot

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


#Minam - Mean Daily Flow only
inst_flow_url_minam <- paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx",
  "?station_nbr=13331500",
  "&start_date=", URLencode(fmt(start_date)),
  "&end_date=",   URLencode(fmt(end_date)),
  "&dataset=MDF",
  "&format=csv"
)
minam_mean_daily_flow <- read.csv(inst_flow_url_minam, sep="\t", row.names=NULL)
minam_mean_daily_flow <- minam_mean_daily_flow |>
  rename(Date = record_date,
         CFS = mean_daily_flow_cfs)
minam_mean_daily_flow <- minam_mean_daily_flow |>
  mutate(Date = mdy(Date, tz = "America/Los_Angeles"))
minam_flow <- ggplot(minam_mean_daily_flow, aes(x=Date, y=CFS)) +
  geom_line(color="blue") +
  labs(x = "Date", y = "Mean Daily Flow (CFS)")
minam_flow
