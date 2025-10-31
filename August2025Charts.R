
rm(list=ls()) #clear workspace

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(data.table)


######################################
## GENERAL OUTLINE OF PROCEDURE ###### 
## (updated Aug 2024 ) ###############
# 1. Read, clean, format data from UT DNR - wells and production data
# 2. Compile production data for the year from 01/2023 through 12/2023
# 3. Create flags for each well characteristic relevant to bonding schedule:
#      - BOE/day production average (determining marginal well status); 
#      - status/type factors for "Active Well" and "At-risk Well" classification;
#      - depth of boreholes for liability calculation
# 4. By operator, sum over wells to figure out how many wells of each type they own, 
#      - this + status factors above is used to assign tiers for blanket+indiv. liabilities
# 5. Calculate size of bond responsibility for each operator based on assigned tier
# 6. Calculate predicted plugging liabilities for each well using a couple different assumptions
# 7. Make plots to compare bonds required under schedule to potential plugging liability
# 8. Calculate some final stats on small operators
# 9. Write operator data to csv
#######################################

# Set directories to reflect Christine's setup
setwd("C:/Users/laure/Documents/Utah")
data_dir = 'Data/'
code_dir = 'UtahDNRAnalytics/'

# Connect to supplementary functions file 'bond_funs.R'
source(paste(code_dir, "bond_funs.R", sep=''))

# Call data files downloaded previously from UT OGM (see main directory + all documentation readmes at: https://oilgas.ogm.utah.gov/oilgasweb/data-center/dc-main.xhtml#download)
prod_data = fread(file.path(data_dir, "Production2020To2024.csv"))      ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Production2020To2024.zip
prod_temp = fread(file.path(data_dir, 'Production2015To2019.csv'))
prod_data = rbind(prod_data, prod_temp)

wells = fread(paste(data_dir, "Wells.csv", sep=''))                         ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Wells.zip
wellhistory = fread(paste(data_dir, "WellHistory.csv", sep=''))             ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/WellHistory.zip

prod_data = data.table(prod_data)
# Change class and/or format for key fields in Production data
prod_data[, `:=`(
  ReportPeriod = as.Date(ReportPeriod, "%m/%d/%Y"),
  Received = as.Date(Received, "%m/%d/%Y"),
  year = year(ReportPeriod),
  API = as.character(format(API, scientific = FALSE))
)]

prod_data[, producing_flag := WellStatus %in% c("P", "PAI", "PII")]

prod_data <- prod_data[, .(
  Oil = sum(Oil, na.rm = TRUE),
  Gas = sum(Gas, na.rm = TRUE),
  Water = sum(Water, na.rm = TRUE),
  producing_flag = max(producing_flag),
  shutin_flag = 1 - max(producing_flag)
), by = .(API, ReportPeriod, Operator)]


## Weatherly
prod_data[order(ReportPeriod),time_shutin:=rowid(rleid(shutin_flag)),by=API]
prod_data[,time_producing:=time_shutin*producing_flag]
prod_data[,time_shutin:=time_shutin*shutin_flag]
prod_data[,weststart:=Operator=='Weststar Exploration Co']
prod_data[,BOEpd:=(Oil+Gas/5.8)/30]
prod_data[,atrisk:=BOEpd<=1]

operator_dat = prod_data[,.(
  Oil=sum(Oil,na.rm=T),
  Gas=sum(Gas,na.rm=T),
  BOEpd=sum(BOEpd, na.rm=T),
  n=.N,
  atrisk=sum(atrisk)
), by=.(ReportPeriod, Operator)]

operator_dat[,pct_atrisk:=atrisk/n]

library(scales)  # for percent_format()

ggplot(operator_dat[Operator == 'Weststar Exploration Co' & ReportPeriod >= '2017-03-01']) +
  geom_line(aes(x = ReportPeriod, y = BOEpd, col = "BOE per day")) +
  geom_line(aes(x = ReportPeriod, y = pct_atrisk * 500, col = "Percent of wells\n<1 BOE pd")) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "black") +
  annotate("text",
           x = as.Date("2017-03-01"),
           y = 170,
           label = "Tier 3 BOE Threshold",
           vjust = -0.3,
           hjust = 0,
           size = 3.5) +
  scale_y_continuous(
    name = "BOE per day",
    sec.axis = sec_axis(~ . / 500,
                        name = "Percent of wells <1 BOE pd",
                        labels = percent_format(accuracy = 1))
  ) +
  scale_color_manual(values = c("BOE per day" = "blue", "Percent of wells\n<1 BOE pd" = "red")) +
  labs(title = "Weststar Exploration Co", x = "Report Period", color = "") +
  theme_bw()

ggsave(file.path('UtahDNRAnalytics', 'Figures', 'Weststar.png'),
       width=6,
       height=4)

