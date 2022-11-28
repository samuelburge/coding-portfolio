# ================================================================================
# EVSE Quarterly Reporting
# Author: Samuel Burge
# Date Created: May 5, 2021
# Date Last Modified: November 18, 2021
#
# Description: Script pulls in the detailed session summary table (exported from
# the ChargePoint dashboard) and summarizes the data for reporting to the Utah 
# Department of Environmental Quality. The data is then written out to csv files 
# that can be compiled in an Excel spreadsheet to make the report.
# ================================================================================

# Load the required packages (you may need to use install.packages() function if they
# are missing or uninstalled)
library("tidyverse")
library("lubridate")
library("kableExtra")

# Define some variables for use in reporting periods to subset data and plot labeling
reporting_end_date <- mdy("09/30/2021")

#Function that takes seconds (integer) and converts to readable representation of time (hh:mm:ss)

hms_display <- function (int) {
  x <- lubridate::as.period(as.duration(int))
  hours <- (as.integer(day(x)) * 24 + hour(x))
  minutes <- as.integer(minute(x))
  seconds <- as.integer(second(x))
  sprintf("%02d:%02d:%02d", hours, minutes, seconds)
}


#Set the working directory and import the initial data set
setwd("S:/USERS/MIS/WKS/EVSE PROGRAM/Analysis & Reporting/")

#Creates column names with better syntax and imports the data from the csv file
ev_column_labels <- c("station_name","mac_address","org","start_date",
                      "start_tz", "end_date", "end_tz", "trans_date",
                      "duration","charge_time","energy","ghg_saving_kg", "ghg_saving_gal",
                      "port_type", "port_no", "plug_type", "evse_id",
                      "address1", "address2", "city", "state", "postal_code",
                      "country", "lat", "long", "currency", "fee", "ended_by",
                      "plugin_event_id", "driver_postal_code", "user_id",
                      "start_soc", "end_soc", "county", "system", "model_no")

sessions <- read_csv("S:/USERS/MIS/WKS/EVSE PROGRAM/Analysis & Reporting/Data/charging sessions.csv", col_names = ev_column_labels)
sessions <- sessions[-1,] #removes the original headers

# The station names are spaced slightly different; this just fixed that for readability
sessions$station_name <- recode(sessions$station_name,
                                "SANDY CITY / CITY HALL DC2" = "SANDY CITY / CITY HALL DC 2",
                                "SANDY CITY / CITY HALL DC3" = "SANDY CITY / CITY HALL DC 3",
                                "SANDY CITY / STATION 6" = "SANDY CITY / CITY HALL 6",
                                "SANDY CITY / STATION 5" = "SANDY CITY / CITY HALL 5",
                                "SANDY CITY / STATION 4" = "SANDY CITY / CITY HALL 4",
                                "SANDY CITY / STATION 3" = "SANDY CITY / CITY HALL 3",
                                "SANDY CITY / STATION 2" = "SANDY CITY / CITY HALL 2",
                                "SANDY CITY / STATION 1" = "SANDY CITY / CITY HALL 1",)

#Filters for fast-charging stations and converts each variable/field to the appropriate data type
sessions$energy <- as.numeric(sessions$energy)
sessions$port_type <- as.factor(sessions$port_type)
sessions$start_date <- mdy_hm(sessions$start_date)
sessions$end_date <- mdy_hm(sessions$end_date)
sessions$charge_time <- hms(sessions$charge_time)
sessions$duration <- hms(sessions$duration)
sessions$station_name <- as.character(sessions$station_name)
sessions$fee <- as.numeric(sessions$fee)

dc_sessions <- sessions %>%
  filter(port_type == "DC Fast" & start_date <= reporting_end_date)

ac_sessions <- sessions %>%
  filter(port_type == "Level 2" & start_date <= reporting_end_date)

#Converts the date string in duration into a dttm object, then to seconds (duration)
dc_sessions$duration <- as.duration(dc_sessions$duration)
dc_sessions$charge_time <- as.duration(dc_sessions$charge_time)

#Aggregate the data into daily totals
daily_data_all <- dc_sessions %>%
  select(station_name, energy, fee, duration, start_date, end_date, port_type, charge_time) %>%
  mutate(calendar_date = as_date(start_date)) %>%
  group_by(calendar_date) %>%
  summarize(kwh = sum(energy),
            fees = sum(fee),
            session_time = sum(duration),
            charge_time = sum(charge_time),
            sessions = n())

# Daily Utilization
start_date <- mdy("06/12/2019")
end_date <- as_date(max(dc_sessions$end_date))
date_range <- c(start_date, end_date)


daily_plot <- ggplot(data = daily_data_all) + 
  
  geom_area(aes(x = calendar_date, y = charge_time), size = 0.5, color = "green4",
            fill = "green4", alpha = 0.25) +
  
  geom_area(aes(x = calendar_date, y = session_time), fill = "gray", alpha = 0.25,
            color = "gray", linetype = "dotted") +
  
  xlab("Date") + ylab("Utilization (HH:MM:SS)") +
  
  scale_x_date(date_break = "3 months", date_label = "%b-%Y",
               limits = date_range, expand = c(0,0)) +
  
  scale_y_time(limits = c(0,max(daily_data_all$charge_time) + 300),expand = c(0,0)) +
  
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey", size = 0.5),
        panel.grid.minor.y = element_line(color = "lightgrey", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color = "grey"),
        axis.ticks.y = element_line(color = "grey"),
        axis.title = element_text(face="bold"),
        axis.text = element_text(face="bold"),
        plot.title = element_text(face="bold"))

# Adds a horizontal blue line to indicate the mean charging time?
daily_plot
ggsave("daily_line_chart.jpg")

?ggsave
# Creates a table summarizing daily utilization

daily_data <- dc_sessions %>%
  select(station_name, energy, fee, duration, start_date, end_date, port_type, charge_time) %>%
  mutate(calendar_date = as_date(start_date)) %>%
  group_by(calendar_date, station_name) %>%
  summarize(kwh = sum(energy),
            fees = sum(fee),
            session_time = sum(duration),
            charge_time = sum(charge_time),
            sessions = n())

daily_util_stats <- daily_data %>%
  group_by(station_name) %>%
  summarize(n = n(),
            min = min(charge_time),
            mean = mean(charge_time),
            median = median(charge_time),
            max = max(charge_time),
            sd = sd(charge_time),
            sum = sum(charge_time))

total_util_stats <- daily_data_all %>%
  group_by() %>%
  summarize(station_name = "SANDY CITY / CITY HALL TOTAL",
            n = n(), 
            min = min(charge_time),
            mean = mean(charge_time),
            median = median(charge_time),
            max = max(charge_time),
            sd = sd(charge_time),
            sum = sum(charge_time))

util_stats_full <- rbind(daily_util_stats, total_util_stats)

# Converts the duration (seconds) into period (hh:mm:ss) for readability
util_stats_full$min <- c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
util_stats_full$mean <- hms_display(util_stats_full$mean)
util_stats_full$median <- hms_display(util_stats_full$median)
util_stats_full$max <- hms_display(util_stats_full$max)
util_stats_full$sd <- hms_display(util_stats_full$sd)
util_stats_full$sum <- hms_display(util_stats_full$sum)

# Creates the table of utilization time for the charging stations

table_labels <- c("Station(s)" ,"Days (N)", "Min.", "Mean", "Median",
                  "Max.", "Standard Dev.", "Total")

colnames(util_stats_full) <- table_labels

kbl(util_stats_full, digits = 2, caption = 'Table 1. "Daily Utilization (HH:MM:SS)"') %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "responsive")) %>%
  row_spec(row = 0:4, align = "center")

#Displays a table of the daily energy statistics

daily_data <- dc_sessions %>%
  select(station_name, energy, duration, start_date, end_date, port_type, charge_time) %>%
  mutate(calendar_date = as_date(start_date)) %>%
  group_by(calendar_date, station_name) %>%
  summarize(kwh = sum(energy),
            session_time = sum(duration),
            charge_time = sum(charge_time),
            sessions = n())

daily_energy_stats <- daily_data %>%
  group_by(station_name) %>%
  summarize(n = n(),
            min = min(kwh),
            mean = mean(kwh),
            median = median(kwh),
            max = max(kwh),
            sd = sd(kwh),
            sum = sum(kwh))

total_energy_stats <- daily_data_all %>%
  group_by() %>%
  summarize(station_name = "SANDY CITY / CITY HALL TOTAL",
            n = n(), 
            min = min(kwh), 
            mean = mean(kwh),
            median = median(kwh), 
            max = max(kwh), 
            sd = sd(kwh), 
            sum = sum(kwh))

daily_energy_hist <- ggplot(daily_data, mapping = aes(x = kwh)) +
  geom_histogram(binwidth = 5, fill = "seagreen")

daily_util_hist <- ggplot(daily_data, mapping = aes(x = charge_time)) +
  geom_histogram(binwidth = 60*5, fill = "seagreen")

energy_stats_full <- rbind(daily_energy_stats, total_energy_stats)


#Creates presentable tables for the summary statistics of the stations

table_labels <- c("Station(s)" ,"Days (N)", "Min.", "Mean", "Median",
                  "Max.", "Standard Dev.", "Total")

colnames(energy_stats_full) <- table_labels

kbl(energy_stats_full, digits = 2, caption= 'Table 2. "Daily Energy Consumption (kWh)"') %>%
  kable_styling(bootstrap_options = c("striped","condensed", "responsive")) %>%
  row_spec(row = 0:4, align = "center")


#Creates a histogram of session charging duration
session_histogram <- ggplot(data = dc_sessions) +
  
  geom_histogram(aes(x = charge_time),
                 binwidth = (5 * 60),
                 alpha = 0.75,
                 fill = "seagreen",
                 color = "seagreen") + 
  
  xlab("Time Spent Charging (HH:MM:SS)") + ylab("Sessions") +
  
  ggtitle("Duration of Charging Sessions (n = 2,082)") +
  
  scale_x_time(expand = c(0,0)) +
  
  scale_y_continuous(expand = c(0,0), limit = c(0,500)) +
  
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey", size = 0.5),
        panel.grid.minor.y = element_line(color = "lightgrey", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color = "grey"),
        axis.ticks.y = element_line(color = "grey"),
        axis.title = element_text(face="bold"))

#Prints the histogram

session_histogram

#Creates session summary statistics for utilization
dc_sessions$charge_time <- as.duration(dc_sessions$charge_time)

session_util_stats <- dc_sessions %>%
  group_by(station_name) %>%
  summarize(n = n(),
            min = min(charge_time),
            mean = mean(charge_time),
            median = median(as.double(charge_time)),
            max = max(charge_time),
            sd = sd(charge_time),
            sum = sum(charge_time))

#Creates a summary of the all the sessions for every station (i.e. it isn't grouped by station like above)
total_session_util_stats <- dc_sessions %>%
  group_by() %>%
  summarize(n = n(), 
            min = min(charge_time), 
            mean = mean(charge_time),
            median = median(charge_time), 
            max = max(charge_time), 
            sd = sd(charge_time), 
            sum = sum(charge_time))

total_session_util_stats <- mutate(total_session_util_stats, station_name = "SANDY CITY / CITY HALL TOTAL")
session_util_stats_full <- rbind(session_util_stats, total_session_util_stats)

# Converts the duration (seconds) into period (hh:mm:ss) for readability
session_util_stats_full$min <- c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
session_util_stats_full$mean <- hms_display(session_util_stats_full$mean)
session_util_stats_full$median <- hms_display(session_util_stats_full$median)
session_util_stats_full$max <- hms_display(session_util_stats_full$max)
session_util_stats_full$sd <- hms_display(session_util_stats_full$sd)
session_util_stats_full$sum <- hms_display(session_util_stats_full$sum)

# Creates the table of utilization time for the charging stations

table_labels <- c("Station(s)" ,"Days (N)", "Min.", "Mean", "Median",
                  "Max.", "Standard Dev.", "Total")

colnames(session_util_stats_full) <- table_labels

session_util_stats_table <-session_util_stats_full %>%
  kbl(digits = 2, caption='Table 3. "Session Utilization (HH:MM:SS)"') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(row = 0:4, align = "center")

#Prints the table of utilization time for the charging stations

session_util_stats_table 

#Creates session energy descriptive statistics
session_energy_stats <- dc_sessions %>%
  group_by(station_name) %>%
  summarize(n = n(),
            min = min(energy),
            mean = mean(energy),
            median = median(energy),
            max = max(energy),
            sd = sd(energy),
            sum = sum(energy))

total_session_energy_stats <- dc_sessions %>%
  group_by() %>%
  summarize(station_name = "SANDY CITY / CITY HALL TOTAL",
            n = n(), 
            min = min(energy), 
            mean = mean(energy),
            median = median(energy), 
            max = max(energy), 
            sd = sd(energy), 
            sum = sum(energy))

session_energy_stats_full <- rbind(session_energy_stats, total_session_energy_stats)

#Creates a table to display the statistics on the kWh used in each session

table_labels <- c("Station(s)" ,"Days (N)", "Min.", "Mean", "Median",
                  "Max.", "Standard Dev.", "Total")

colnames(session_energy_stats_full) <- table_labels

session_energy_stats_table <- session_energy_stats_full %>%
  kbl(digits = 2, caption='Table 4. "Session Energy Consumption (kWh)"') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(row = 0:4, align = "center")

#Prints the table on session energy (kWh) utilized during charging sessions

session_energy_stats_table


# Write the tables to csv files to create the report
write_csv(session_util_stats_full,
         "S:/USERS/MIS/WKS/EVSE PROGRAM/Analysis & Reporting/Exported Tables/session_util_stats_full.csv")

write_csv(session_energy_stats_full,
          "S:/USERS/MIS/WKS/EVSE PROGRAM/Analysis & Reporting/Exported Tables/session_energy_stats_full.csv")

write_csv(util_stats_full,
          "S:/USERS/MIS/WKS/EVSE PROGRAM/Analysis & Reporting/Exported Tables/util_stats_full.csv")

write_csv(energy_stats_full,
          "S:/USERS/MIS/WKS/EVSE PROGRAM/Analysis & Reporting/Exported Tables/energy_stats_full.csv")


# Write the daily aggregates after adjusting the charging time (charge_time) to display as a time
daily_data_export <- daily_data %>%
  select(calendar_date, kwh, session_time, charge_time, sessions, fee) %>%
  mutate(charge_time = hms_display(charge_time),
         session_time = hms_display(session_time))

write_csv(daily_data_export,
          "S:/USERS/MIS/WKS/EVSE PROGRAM/Analysis & Reporting/Exported Tables/daily_data.csv")

# Some additional aggregations for analyzing stuff
driver_stats <- meter_data %>%
  #filter(`Port Type` == 'DC Fast') %>%
  select(`EVSE ID`, `User ID`, `Plug In Event ID`,`Fee`,`Driver Zip/Postal Code`, `Port Type`) %>%
  drop_na(`Driver Zip/Postal Code`,`Port Type`) %>%
  group_by(`User ID`, `Driver Zip/Postal Code`) %>%
  summarize(sessions = n(),
            fees_paid = sum(`Fee`)) %>%
  arrange(desc(`fees_paid`))

usage_by_station <- sessions %>%
  select(station_name, duration, charge_time, energy, ghg_saving_kg, ghg_saving_gal, fee, port_type) %>%
  group_by(station_name) %>%
  summarize(total_sessions = n(),
            total_time = sum(charge_time),
            total_energy = sum(energy),
            total_fees = sum(fee),
            sessions_share = round(n()/length(sessions$station_name),2),
            time_share= round(total_time/sum(sessions$charge_time),2),
            energy_share = round(total_energy/sum(sessions$energy),2),
            fees_share = round(total_fees/sum(sessions$fee),2),) %>%
  arrange(desc(total_sessions))

(sum(usage_by_station$total_energy)/60)/60

length(unique(as.character(sessions$station_name)))
length(unique(as.character(usage_by_station$station_name)))
str(sessions$duration)
