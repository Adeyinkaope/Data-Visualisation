##############################################
#  IJC445 DATA VISUALIZATION
#   AIR QUALITY DATASET FROM DEFRA
##############################################

#-------------------
#   IMPORT LIBRARY
#-------------------

library(tidyverse)
library(lubridate)
library(readr)
library(stringr)

#---------------------------------------------------------------
#    IMPORT AND READ CSV FILE FOR BOTH GLASGOW AND SHEFFIELD
#       ALSO SKIPPING THE FIRST 17 LINE OF THE DATASET
#---------------------------------------------------------------

glasgow <- read_csv(
  "C:/Msc Data Science/Introduction to Data science/Week 1/Intro Coursework/Introduction of Data Science/Glasgow.csv",
  skip = 17,
  show_col_types = FALSE
)

sheffield <- read_csv(
  "C:/Msc Data Science/Introduction to Data science/Week 1/Intro Coursework/Introduction of Data Science/Sheffiled.csv",
  skip = 17,
  show_col_types = FALSE
)

View(glasgow)
View(sheffield)

out_dir <- "outputs_step_by_step_style"
dir.create(out_dir, showWarnings = FALSE)


head(glasgow)
glimpse(glasgow)
names(glasgow)

head(sheffield)
glimpse(sheffield)
names(sheffield)

################################################################################


#-------------------------------------------
#            CLEANING MISSING DATA
#       CONVERTING THE "NO DATA" TO NA
#------------------------------------------- 

glasgow <- glasgow %>%
  mutate(across(everything(), ~ na_if(as.character(.x), "No data"))) %>%
  mutate(across(everything(), ~ na_if(as.character(.x), "---"))) %>%
  mutate(across(everything(), ~ na_if(as.character(.x), "N/A"))) %>%
  mutate(across(everything(), ~ na_if(as.character(.x), ""))) %>%
  mutate(across(everything(), ~ trimws(.x)))   # remove extra spaces

sheffield <- sheffield %>%
  mutate(across(everything(), ~ na_if(as.character(.x), "No data"))) %>%
  mutate(across(everything(), ~ na_if(as.character(.x), "---"))) %>%
  mutate(across(everything(), ~ na_if(as.character(.x), "N/A"))) %>%
  mutate(across(everything(), ~ na_if(as.character(.x), ""))) %>%
  mutate(across(everything(), ~ trimws(.x)))

head(glasgow)
head(sheffield)

################################################################################

#---------------------------
#   CONVERTING THE DATE
#---------------------------

# CHECK THE DATE
head(glasgow$Date, 10)
head(sheffield$Date, 10)

# CONVERT IT TO YMD 
glasgow <- glasgow %>% mutate(Date = ymd(Date))
sheffield <- sheffield %>% mutate(Date = ymd(Date))

# many NAs,  Date is probably dd/mm/yyyy;
# glasgow <- glasgow %>% mutate(Date = dmy(Date))
# sheffield <- sheffield %>% mutate(Date = dmy(Date))

class(glasgow$Date)
class(sheffield$Date)

range(glasgow$Date, na.rm = TRUE)
range(sheffield$Date, na.rm = TRUE)

################################################################################
# CONVERT TIME 

glasgow <- glasgow %>% mutate(Time = as.character(Time))
sheffield <- sheffield %>% mutate(Time = as.character(Time))

head(glasgow$Time, 10)
head(sheffield$Time, 10)

################################################################################
#-----------------------------
#  FIX THE DATETIME TO 24:00
#-----------------------------

glasgow <- glasgow %>%
  mutate(
    is_24h = str_detect(Time, "^24:"),
    Time_fixed = if_else(is_24h, str_replace(Time, "^24:", "00:"), Time),
    Time_fixed = if_else(str_detect(Time_fixed, "^\\d{2}:\\d{2}$"),
                         paste0(Time_fixed, ":00"),
                         Time_fixed),
    Date_fixed = Date + days(is_24h),
    datetime = Date_fixed + hms(Time_fixed)
  ) %>%
  arrange(datetime)

sheffield <- sheffield %>%
  mutate(
    is_24h = str_detect(Time, "^24:"),
    Time_fixed = if_else(is_24h, str_replace(Time, "^24:", "00:"), Time),
    Time_fixed = if_else(str_detect(Time_fixed, "^\\d{2}:\\d{2}$"),
                         paste0(Time_fixed, ":00"),
                         Time_fixed),
    Date_fixed = Date + days(is_24h),
    datetime = Date_fixed + hms(Time_fixed)
  ) %>%
  arrange(datetime)

head(glasgow %>% select(Date, Time, datetime))
head(sheffield %>% select(Date, Time, datetime))

range(glasgow$datetime, na.rm = TRUE)
range(sheffield$datetime, na.rm = TRUE)

################################################################################

# 6)SAVE CLEAN CSV

write_csv(glasgow, file.path(out_dir, "glasgow_clean.csv"))
write_csv(sheffield, file.path(out_dir, "sheffield_clean.csv"))


################################################################################


#----------------------------------------------------------
#      SELECTION OF VARIABLE NEEDED FROM BOTH CITIES 
#         CONVERTING VARIABLE TO NUMERIC 
#----------------------------------------------------------

glasgow_core <- glasgow %>%
  transmute(
    city = "Glasgow (Townhead)",
    datetime,
    pm25 = parse_number(`PM2.5 particulate matter (Hourly measured)`),
    pm10 = parse_number(`PM10 particulate matter (Hourly measured)`),
    no2  = parse_number(`Nitrogen dioxide`),
    o3   = parse_number(`Ozone`),
    wdir = parse_number(`Modelled Wind Direction`),
    wspd = parse_number(`Modelled Wind Speed`),
    temp = parse_number(`Modelled Temperature`)
  )

sheffield_core <- sheffield %>%
  transmute(
    city = "Sheffield (Devonshire Green)",
    datetime,
    pm25 = parse_number(`PM2.5 particulate matter (Hourly measured)`),
    pm10 = parse_number(`PM10 particulate matter (Hourly measured)`),
    no2  = parse_number(`Nitrogen dioxide`),
    o3   = parse_number(`Ozone`),
    wdir = parse_number(`Modelled Wind Direction`),
    wspd = parse_number(`Modelled Wind Speed`),
    temp = parse_number(`Modelled Temperature`)
  )

head(glasgow_core)
glimpse(glasgow_core)

head(sheffield_core)
glimpse(sheffield_core)

################################################################################

# COMBINE THE TWO CITIES TO MAKE ONE CSV

both_hourly <- bind_rows(glasgow_core, sheffield_core) %>%
  filter(!is.na(datetime)) %>%
  arrange(city, datetime)

head(both_hourly)
range(both_hourly$datetime, na.rm = TRUE)

write_csv(both_hourly, file.path(out_dir, "both_hourly.csv"))

################################################################################

#----------------------------------------------------------------------------
#     CREATE DAILY, MONLTY AND YEARLY SMMUARIES FO THE ANALYSES AND PLOTS
#----------------------------------------------------------------------------

## DAILY 

both_daily <- both_hourly %>%
  mutate(date = as.Date(datetime), year = year(datetime)) %>%
  group_by(city, date, year) %>%
  summarise(
    pm25 = mean(pm25, na.rm = TRUE),
    pm10 = mean(pm10, na.rm = TRUE),
    no2  = mean(no2,  na.rm = TRUE),
    o3   = mean(o3,   na.rm = TRUE),
    wdir = mean(wdir, na.rm = TRUE),
    wspd = mean(wspd, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    .groups = "drop"
  )

## MONLTY

both_monthly <- both_hourly %>%
  mutate(month = floor_date(datetime, "month"), year = year(datetime)) %>%
  group_by(city, month, year) %>%
  summarise(
    pm25 = mean(pm25, na.rm = TRUE),
    pm10 = mean(pm10, na.rm = TRUE),
    no2  = mean(no2,  na.rm = TRUE),
    o3   = mean(o3,   na.rm = TRUE),
    wspd = mean(wspd, na.rm = TRUE),
    temp = mean(temp, na.rm = TRUE),
    .groups = "drop"
  )


## YEARLY

both_yearly <- both_hourly %>%
  mutate(year = year(datetime)) %>%
  group_by(city, year) %>%
  summarise(
    pm25 = mean(pm25, na.rm = TRUE),
    pm10 = mean(pm10, na.rm = TRUE),
    no2  = mean(no2,  na.rm = TRUE),
    o3   = mean(o3,   na.rm = TRUE),
    .groups = "drop"
  )

head(both_daily)
head(both_monthly)
head(both_yearly)

write_csv(both_daily,   file.path(out_dir, "both_daily.csv"))
write_csv(both_monthly, file.path(out_dir, "both_monthly.csv"))
write_csv(both_yearly,  file.path(out_dir, "both_yearly.csv"))


#---------------------------------------------
#     CREATE FOR SEASON AND WIND SECTOR
#        NEEDED FOR METEROLOGY PLOTS
#---------------------------------------------

both_daily <- both_daily %>%
  mutate(
    season = case_when(
      month(date) %in% c(12, 1, 2) ~ "Winter",
      month(date) %in% c(3, 4, 5)  ~ "Spring",
      month(date) %in% c(6, 7, 8)  ~ "Summer",
      TRUE ~ "Autumn"
    ),
    wind_sector = case_when(
      wdir >= 337.5 | wdir < 22.5 ~ "N",
      wdir < 67.5  ~ "NE",
      wdir < 112.5 ~ "E",
      wdir < 157.5 ~ "SE",
      wdir < 202.5 ~ "S",
      wdir < 247.5 ~ "SW",
      wdir < 292.5 ~ "W",
      TRUE         ~ "NW"
    )
  )

head(both_daily)

################################################################################

#------------------------------
#   PLOTS FRO VISUALISATION
#------------------------------

#--------------------------------------------------
#    PLOT 1 MUTLIPOLLUTANT DAILY TIME SERIES
#            FACETED BY YEAR AND CITY
#--------------------------------------------------

# USING THE DAILYCSV
#
# Keep only years with enough daily points (e.g., at least 300 days)
valid_years <- both_daily %>%
  count(city, year) %>%
  filter(n >= 300)

both_daily_plot <- both_daily %>%
  semi_join(valid_years, by = c("city", "year"))

ts_long <- both_daily_plot %>%
  select(city, date, year, pm25, pm10, no2, o3) %>%
  pivot_longer(cols = c(pm25, pm10, no2, o3),
               names_to = "pollutant",
               values_to = "value") %>%
  mutate(pollutant = recode(pollutant,
                            pm25="PM2.5", pm10="PM10", no2="NO2", o3="O3")) %>%
  filter(!is.na(value))


p1 <- ggplot(ts_long, aes(x = date, y = value, colour = pollutant)) +
  geom_line(alpha = 0.6, linewidth = 0.35) +
  geom_smooth(se = FALSE, linewidth = 0.9) +
  facet_grid(city ~ year, scales = "free_x") +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b",
    guide = guide_axis(n.dodge = 2)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    title = "Multi-pollutant trends (daily means)",
    x = "Month",
    y = "Concentration",
    colour = "Pollutant"
  )

p1

################################################################################
# 2024 IS FILTERED OUT IN THE DATASET
################################################################################


#--------------------------------
#       FIGURE 2 PM2.5 VS NO2
#         METEROLOGY ENCODED
#--------------------------------


fig2_df <- both_daily_plot %>%
  filter(!is.na(pm25), !is.na(no2), 
         !is.na(wind_sector), 
         !is.na(wspd),
         !is.na(temp))

head(fig2_df)

p2 <- ggplot(fig2_df, aes(x = pm25, y = no2)) +
  geom_point(aes(colour = wind_sector, size = wspd), alpha = 0.6) +
  geom_smooth(se = FALSE, linewidth = 0.9) +
  facet_wrap(~ city, ncol = 1) +
  scale_size_continuous(range = c(0.8, 3.5)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "PM2.5 vs NO2 (daily means)",
    subtitle = "Colour = wind sector; point size = wind speed",
    x = "PM2.5 (daily mean)",
    y = "NO2 (daily mean)",
    colour = "Wind sector",
    size = "Wind speed"
  )

p2


################################################################################


#-------------------------------------------------
#          FIGURE 3  POLLUTANT VS TEMPERATURE 
#            FACETED COLOUR = SEASON  
#-------------------------------------------------

temp_long <- both_daily_plot %>%
  select(city, season, temp, pm25, pm10, no2, o3) %>%
  pivot_longer(cols = c(pm25, pm10, no2, o3),
               names_to = "pollutant",
               values_to = "value") %>%
  mutate(
    pollutant = recode(pollutant,
                       pm25 = "PM2.5", pm10 = "PM10", no2 = "NO2", o3 = "O3")
  ) %>%
  filter(!is.na(temp), !is.na(value))

head(temp_long)


# PLOTS FOR FIG 3

p3 <- ggplot(temp_long, aes(x = temp, y = value, colour = season)) +
  geom_point(alpha = 0.35, size = 1.1) +
  geom_smooth(se = FALSE, linewidth = 0.9) +
  facet_grid(pollutant ~ city, scales = "free_y") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Pollutant concentration vs Temperature (daily means)",
    x = "Temperature",
    y = "Concentration",
    colour = "Season"
  )

p3

################################################################################


#-------------------------------------------------------------------------
#          FIGURE 4;  A WIND ROSE HEATMAP WITH PM2.5
#            WITH DIRECTION BINS  X  SPEED BINS
#        HOURLY DATA ARE USED FOR STRONGER WIND DIRECTION SINGNAL
#-------------------------------------------------------------------------

fig4_df <- both_hourly %>%
  filter(!is.na(pm25), !is.na(wdir), !is.na(wspd)) %>%
  mutate(
    year = year(datetime)
  ) %>%
  semi_join(valid_years, by = c("city", "year")) %>%   # remove 2024 here too
  mutate(
    dir_bin = factor(floor((wdir %% 360) / 10) * 10, levels = seq(0, 350, 10)),
    speed_bin = cut(
      wspd,
      breaks = c(0, 1, 3, 5, 8, 12, Inf),
      labels = c("0–1", "1–3", "3–5", "5–8", "8–12", "12+"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(city, dir_bin, speed_bin) %>%
  summarise(pm25_mean = mean(pm25, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(speed_bin))

head(fig4_df)

### PLOT FOR FIG 4

p4 <- ggplot(fig4_df, aes(x = dir_bin, y = speed_bin, fill = pm25_mean)) +
  geom_tile() +
  coord_polar(theta = "x") +
  facet_wrap(~ city, ncol = 2) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 7),
    legend.position = "right"
  ) +
  labs(
    title = "Wind-rose heatmap (multi-year)",
    subtitle = "Direction (10° bins) × wind speed bins, filled by mean PM2.5",
    x = "Wind direction",
    y = "Wind speed bin",
    fill = "Mean PM2.5"
  )

p4
