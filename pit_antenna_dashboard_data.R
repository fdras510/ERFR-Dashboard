#Daily PIT antenna data
library(httr)
library(tidyverse)
library(janitor)
library(openxlsx)
library(plotly)
library(reactable)

getwd()

#Interrogation data
int_data <- 'https://api.ptagis.org/reporting/reports/fglas25/file/erfr_interrogation_summary.csv'#use

# interrogation_sum <- read_csv(
#   file = int_data,
#   locale = locale(encoding = "UTF-16"),
#   col_types = cols(.default = "c"))


interrogation_call <- GET(int_data)
interrogation_sum <- content(interrogation_call, encoding = "UTF-16")

# interrogation_sum <- read_csv(
#   file      = rawConnection(content(interrogation_sum_t, "raw")),
#   locale    = locale(encoding = "UTF-16"),
#   col_types = cols(.default = "c")
# )

interrogation_sum <- interrogation_sum[,-c(13)]
interrogation_sum <- clean_names(interrogation_sum)

interrogation_sum <- interrogation_sum %>%
  mutate(first_time = as.Date(mdy_hms(first_time)),
         last_time = as.Date(mdy_hms(last_time)))
monthly_summary <- interrogation_sum %>%
  filter(first_time >= Sys.Date()-30) %>%
  rename(species = species_name,
         rear_type = rear_type_name)

daily_summary <- monthly_summary %>%
  mutate(date = first_time) %>% 
  distinct(site, date, tag, species, rear_type) %>%
  group_by(site, date, species, rear_type) %>%
  summarise(total_tags = n_distinct(tag), .groups = "drop")

daily_summary <- daily_summary|> 
  mutate(species = case_when(
    species == "Chinook" & rear_type == "Wild Fish or Natural Production" ~ "Wild Chinook",
    species == "Chinook" & rear_type == "Hatchery Reared" ~ "Hatchery Chinook",
    species == "Chinook" & rear_type == "Unknown" ~ "Unk. origin Chinook",
    species == "Steelhead" & rear_type == "Wild Fish or Natural Production" ~ "Wild Steelhead",
    species == "Steelhead" & rear_type == "Hatchery Reared" ~ "Hatchery Steelhead",
    TRUE ~ species
  ))
  
daily_summary <- daily_summary |>
  mutate(site = factor(site, levels = c("CC5 - Catherine Creek - State Park",
                                        "UG4 - Grande Ronde-Acclimation Site",
                                        "IR5 - Imnaha Weir Upstream Array",
                                        "WEN - Wenaha River Mouth",
                                        "CC4 - Catherine Creek-Southern Cross",
                                        "UG3 - Grande Ronde - Guard Station",
                                        "IR4 - Imnaha Weir Downstream Array",
                                        "MDC - Meadow Creek mouth",
                                        "CCW - Catherine Creek Ladder/Weir",
                                        "UGS - Upper Grande Ronde Starkey")))

antenna_plot <- ggplot(daily_summary,aes(x = date, y = total_tags, fill = species)) +
  geom_bar(position = "stack", stat = "identity") + 
  labs(x = "Date", y = "Total tags", fill = "Species") +
  scale_fill_viridis_d(direction = -1) +
  facet_wrap(~site)
antenna_plot

antenna_plotly <- ggplotly(antenna_plot)
antenna_plotly

#Timer Tag Data
timer.tags <- 'https://api.ptagis.org/reporting/reports/fglas25/file/erfr_timer_tag.csv'#Read in csv from PTAGIS
timer.call <- GET(timer.tags)
# timer.data <- read_csv(
#   file      = rawConnection(content(timer.tags2, "raw")),
#   locale    = locale(encoding = "UTF-16"),
#   col_types = cols(.default = "c")
# )

timer.data <- content(timer.call, encoding = "UTF-16")
timer.data <- clean_names(timer.data) 
timer.data <- timer.data[-c(1),]
timer.data <- timer.data |>
  rename(
    date = metrics
  ) 

timer.data <- timer.data |>
  mutate(date= as.Date(date, format = "%m/%d/%Y"))
timer.data <- timer.data |>
  mutate(across(timer_tag_count_5:timer_tag_count_28, as.numeric))
timers2 <- timer.data %>%
  group_by(site_code, antenna,date) %>%  #antenna, 
  summarize(tag.cts = sum(rowSums(across(timer_tag_count_5:timer_tag_count_28))), .groups = "drop")
timers3 <- timers2 |>
  mutate(date = floor_date(date, "day")) |>
  group_by(site_code, antenna, date) |>
  summarize(
    total_hours_up = sum(tag.cts),
    expected_hours = 24,
    prop_up = pmin(total_hours_up / expected_hours, 1.0), #caps proportions at 1
    .groups = "drop"
  )
#Assign sites, antennas, and antenna group
antenna_map <- tribble(
  ~site_code, ~antenna, ~array,
  "CC5", "01", "Upstream Array",
  "CC5", "02", "Downstream Array",
  
  "CC4", "01", "Upstream Array",
  "CC4", "02", "Upstream Array",
  "CC4", "03", "Downstream Array",
  
  "CCW", "01", "Top Ladder Antenna",
  "CCW", "02", "Bottom Laddder Antenna",
  
  "CCU", "01", "Upstream Array",
  "CCU", "02", "Upstream Array",
  "CCU", "03", "Downstream Array",
  "CCU", "04", "Downstream Array",
  
  "UG4", "01", "Upstream Array",
  "UG4", "02", "Downstream Array",
  
  "UG3", "01", "Upstream Array",
  "UG3", "02", "Downstream Array",
  
  "UGS", "02", "Upstream Array",
  "UGS", "03", "Upstream Array",
  "UGS", "04", "Middle Array",
  "UGS", "05", "Middle Array",
  "UGS", "06", "Middle Array",
  "UGS", "07", "Downstream Array",
  "UGS", "08", "Downstream Array",
  
  "MDC", "01", "Upstream Array",
  "MDC", "02", "Middle Array",
  "MDC", "03", "Downstream Array",
  
  "IR4", "01", "Upstream Array",
  "IR4", "02", "Upstream Array",
  "IR4", "03", "Upstream Array",
  "IR4", "04", "Downstream Array",
  "IR4", "05", "Downstream Array",
  "IR4", "06", "Downstream Array",
  
  "IR5", "01", "Upstream Array",
  "IR5", "02", "Upstream Array",
  "IR5", "03", "Upstream Array",
  "IR5", "04", "Upstream Array",
  "IR5", "05", "Downstream Array",
  "IR5", "06", "Downstream Array",
  "IR5", "07", "Downstream Array",
  "IR5", "08", "Downstream Array",
  
  "WEN", "01", "Upstream Array",
  "WEN", "02", "Upstream Array",
  "WEN", "03", "Downstream Array",
  "WEN", "04", "Downstream Array"
)
#Join Timer tag data and antenna map
timers3 <- timers3 %>%
  left_join(antenna_map, by = c("site_code","antenna")) %>%
  relocate(array, .after = antenna)
uptime <- timers3 |>
  select(site_code,antenna,array,date,prop_up)
uptime <- uptime |>
  mutate(prop_up = round(prop_up,2))
uptime <- uptime |>
  pivot_wider(
    names_from = date,
    values_from = prop_up,
    values_fill = 0,
    names_expand = TRUE)
Timer_tags <- reactable(uptime, defaultPageSize = 25)
Timer_tags

saveRDS(Timer_tags, "data/uptime_data.rds")
saveRDS(antenna_plot, "data/antenna_plot.rds")