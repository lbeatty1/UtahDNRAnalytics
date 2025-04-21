###########################################
## Code to analyze new Utah bonding rule ##
## Written by Lauren Beatty, EDF ##########
## lbeatty@edf.org ########################
###########################################

rm(list=ls()) #clear workspace

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(data.table)

# Set directories to reflect Christine's setup
setwd("C:/Users/laure/Documents/Utah")
source('C:/Users/laure/Documents/edf_theme.R')

data_dir = 'Data/'
code_dir = 'UtahDNRAnalytics/'

# Connect to supplementary functions file 'bond_funs.R'
source(paste(code_dir, "bond_funs.R", sep=''))

# Call data files downloaded previously from UT OGM (see main directory + all documentation readmes at: https://oilgas.ogm.utah.gov/oilgasweb/data-center/dc-main.xhtml#download)

prod_data=NULL
for(j in list.files(data_dir, full.names=T)[grepl('Production', list.files(data_dir))]){
  tempdata = fread(j)
  prod_data = rbind(prod_data, tempdata)
}
wells = fread(paste(data_dir, "Wells.csv", sep=''))                         ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Wells.zip
wellhistory = fread(paste(data_dir, "WellHistory.csv", sep=''))             ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/WellHistory.zip

prod_data[, `:=`(
  ReportPeriod = as.Date(ReportPeriod, "%m/%d/%Y"),
  Received = as.Date(Received, "%m/%d/%Y"),
  year = year(ReportPeriod),
  API = as.character(format(API, scientific = FALSE))
)]

# Format key fields in Wells.csv
wells[, `:=`(
  API = as.character(format(API, scientific = FALSE)),
  API10 = substr(API, 1, 10),
  AbandonDate = as.Date(AbandonDate, "%Y-%m-%d")
)]

# Format key fields in Well_History.csv; select working cols for depth calc step below
wellhistory[, `:=`(
  API = as.character(WellID),
  FirstProdDate = as.Date(FirstProdDate, "%m/%d/%Y"),
  MD = as.numeric(MD),
  TVD = as.numeric(TVD),
  APDReceivedDate = as.Date(APDReceivedDate, '%m/%d/%Y'),
  WCRCompletionDate = as.Date(WCRCompletionDate, '%m/%d/%Y'),
  DrySpud = as.Date(DrySpud, "%m/%d/%Y"),
  RotarySpud = as.Date(RotarySpud, "%m/%d/%Y")
)]

welldepth <- wellhistory[, .(
  depth = max(TVD, na.rm = TRUE),
  FirstProdDate = min(FirstProdDate, na.rm = TRUE),
  FirstCompletionDate = min(WCRCompletionDate,na.rm=T),
  SpudDate = min(pmin(DrySpud, RotarySpud,na.rm=T),na.rm=T)
), by = API]
welldepth[FirstCompletionDate==Inf, FirstCompletionDate:=NA]
welldepth[SpudDate==Inf, SpudDate:=NA]
welldepth[is.infinite(depth), depth := NA_real_]
welldepth[is.infinite(FirstProdDate), FirstProdDate := NA]

wells[,FirstProdDate:=NULL]
wells <- merge(wells, welldepth, by.x = "API10", by.y = "API", all.x = TRUE)

wells[,fee_state_flag := (LeaseType=="FEE"|LeaseType=="STATE")]

######
## Filter
wells = wells[welltype%in%c('OW', 'GW', 'OGW', 'OWI', 'GWI', 'GGI', 'OWD', 'GWD', 'D')]
wells = wells[!wellstatus%in%c('APD', 'LA', 'DRL', 'RET')]


prod_data[,year:=year(ReportPeriod)]
wells[,FirstProdYear:=year(FirstProdDate)]
wellcounts = wells[,.(
  avg_depth  = mean(depth, na.rm=T),
  n=.N
), by=FirstProdYear]

ggplot(wellcounts[FirstProdYear>2000&FirstProdYear<=2024])+
  geom_line(aes(x=FirstProdYear, y=n))+
  xlab('Year')+
  ylab('Number of New Wells')+
  theme_bw()
ggsave(filename = paste0(code_dir, 'Figures/NewWellsTimeseries.jpg'),
       width=6,
       height=4)

ggplot(wellcounts[FirstProdYear>2000&FirstProdYear<=2024])+
  geom_line(aes(x=FirstProdYear, y=avg_depth))+
  xlab('Year')+
  ylab('Mean Depth of New Wells')+
  theme_bw()

ggsave(filename = paste0(code_dir, 'Figures/NewWellsDepth.jpg'),
       width=6,
       height=4)

#Production timeseries
prod_timeseries = prod_data[,.(
  Gas=sum(Gas,na.rm=T),
  Oil=sum(Oil,na.rm=T)
), by=year]
ggplot(prod_timeseries[year>=2000&year<=2024])+
  geom_line(aes(x=year, y=Gas/1e6))+
  xlab('Year')+
  ylab('mMCF')+
  theme_minimal()
ggsave(filename = paste0(code_dir, 'Figures/GasProduction.jpg'),
       width=6,
       height=4)

ggplot(prod_timeseries[year>=2000&year<=2024])+
  geom_line(aes(x=year, y=Oil/1e6))+
  xlab('Year')+
  ylab('mBBL')+
  theme_minimal()
ggsave(filename = paste0(code_dir, 'Figures/OilProduction.jpg'),
       width=6,
       height=4)

###########
## 
# Collapse production data to API10 level, capturing all wells in the borehole
prod_data[, producing_flag := WellStatus %in% c("P", "PAI", "PII")]

prod_data <- prod_data[, .(
  Oil = sum(Oil, na.rm = TRUE),
  Gas = sum(Gas, na.rm = TRUE),
  Water = sum(Water, na.rm = TRUE),
  producing_flag = max(producing_flag),
  shutin_flag = 1 - max(producing_flag)
), by = .(API, ReportPeriod)]

# Find length of production and shut-in periods
# This section creates a running counter for time shutin and time producing, in months
# Note: no tidyverse solution - switch to data.table
prod_data = data.table(prod_data)
prod_data[order(ReportPeriod),time_shutin:=rowid(rleid(shutin_flag)),by=API]
prod_data[,time_producing:=time_shutin*producing_flag]
prod_data[,time_shutin:=time_shutin*shutin_flag]


##########
## Make full panel
### 
panel = CJ(API=unique(wells$API10), date=seq.Date(as.Date('1990-01-01'), as.Date('2023-12-01'), by='month'))
panel = merge(panel, prod_data, by.x=c('API', 'date'), by.y=c('API', 'ReportPeriod'), all.x=T)
panel = merge(panel, wells, by.x='API', by.y='API10', all.x=T)

panel[,year:=year(date)]
panel = panel[is.na(AbandonDate)|date<AbandonDate,]
panel = panel[date>SpudDate|is.na(SpudDate)]
panel = panel[date>=FirstCompletionDate|is.na(FirstCompletionDate)]

panel[Oil>0|Gas>0,FirstProdDate_imputed:=min(date), by='API']
panel[,FirstProdDate_imputed:=min(FirstProdDate_imputed,na.rm=T), by='API']
panel[,FirstProdDate:=coalesce(FirstProdDate, FirstProdDate_imputed)]
panel[,vintage:=coalesce(FirstProdDate, FirstCompletionDate, SpudDate)]
panel[,vintage:=year(vintage)]
panel[,vintage:=floor(vintage/10)*10]
panel = panel[date>=FirstProdDate|is.na(FirstProdDate)]

#Production timeseries
panel[vintage<2000, vintage:=1990]
panel[,vintage:=paste(as.character(vintage), as.character(vintage+9), sep='-')]
panel[vintage=='1990-1999', vintage:='Before 2000']
panel[vintage=='2020-2029', vintage:='Since 2020']
panel[,vintage:=factor(vintage, levels=c('Since 2020', '2010-2019', '2000-2009', 'Before 2000'))]

prod_timeseries = panel[,.(
  Gas=sum(Gas,na.rm=T),
  Oil=sum(Oil,na.rm=T)
), by=.(date, vintage)]

prod_timeseries_total <- panel[, .(
  Gas = sum(Gas, na.rm = TRUE),
  Oil = sum(Oil, na.rm = TRUE)
), by = .(date)]

base_theme <- theme_minimal() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)  # Consistent margins
  )

# Plot 1: Total oil production (no fill)
plot_total <- ggplot(prod_timeseries_total[year(date) >= 2000 & year(date) <= 2024]) +
  geom_area(aes(x = date, y = Oil / 1e6)) +
  ylim(0, max(prod_timeseries_total$Oil / 1e6)) +
  xlab("Year") +
  ylab("mBBL") +
  base_theme

# Plot 2: Oil production by vintage
plot_vintage <- ggplot(prod_timeseries[year(date) >= 2000 & year(date) <= 2024 & !is.na(vintage)]) +
  geom_area(aes(x = date, y = Oil / 1e6, fill = vintage, group = vintage)) +
  ylim(0, max(prod_timeseries_total$Oil / 1e6)) +  # Keep axis identical
  xlab("Year") +
  ylab("mBBL") +
  scale_fill_edf(name = "Well Vintage") +
  base_theme

ggsave(paste0(code_dir, "Figures/OilProduction.jpg"), plot_total, width = 6, height = 4, dpi = 300)
ggsave(paste0(code_dir, "Figures/OilProduction_vintage.jpg"), plot_vintage, width = 7.25, height = 4, dpi = 300)


############
## Number of Wells

prod_timeseries = panel[,.(
  n=.N
), by=.(date, vintage)]

prod_timeseries_total <- panel[, .(
  n=.N
), by = .(date)]

plot_total <- ggplot(prod_timeseries_total[date >= '2000-01-01' & year(date)<=2024]) +
  geom_area(aes(x = date, y = n)) +
  ylim(0, max(prod_timeseries_total$n)) +
  labs(color = "") +
  xlab("Year") +
  ylab("Number of Wells")+
  base_theme

# Plot 2: Oil production by vintage
plot_vintage <- ggplot(prod_timeseries[year(date) >= 2000 & year(date) <= 2024 & !is.na(vintage)]) +
  geom_area(aes(x = date, y = n, fill = vintage, group = vintage)) +
  ylim(0, max(prod_timeseries_total$n)) +  # Keep axis identical
  xlab("Year") +
  ylab("Number of Wells") +
  scale_fill_edf(name = "Well Vintage") +
  base_theme


ggsave(paste0(code_dir, "Figures/N_wells.jpg"), plot_total, width = 6, height = 4, dpi = 300)

ggsave(paste0(code_dir, "Figures/N_wells_vinage.jpg"), plot_vintage, width = 7.25, height = 4, dpi = 300)


