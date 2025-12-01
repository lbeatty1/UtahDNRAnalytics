rm(list=ls()) #clear workspace

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(data.table)

# Set directories to reflect Christine's setup
setwd("C:/Users/laure/Documents/Utah")
data_dir = 'Data'
code_dir = 'UtahDNRAnalytics'

source(file.path(code_dir, 'CurrentReg/bond_funs.R'))

wells = fread(file.path(data_dir, "Wells.csv"))                         ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Wells.zip
wellhistory = fread(file.path(data_dir, "WellHistory.csv"))             ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/WellHistory.zip

#####################
## FORMAT DATA ######
#####################

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
  TVD = as.numeric(TVD)
)]

####################################################
## Calculate max depth for each borehole by API10 ##
####################################################

# Occasionally some work is done to deepen boreholes, yielding duplicate entries
# We need the final depths, so we can just find max depth by API10:
# Step 1: Summarise well depth and first production date
welldepth <- wellhistory[, .(
  depth = max(TVD, na.rm = TRUE),
  FirstProdDate = min(FirstProdDate, na.rm = TRUE)
), by = API]

# Step 2: Replace -Inf and Inf with NA
welldepth[is.infinite(depth), depth := NA_real_]
welldepth[is.infinite(FirstProdDate), FirstProdDate := NA]

# Step 3: Join to wells by API10
wells[,FirstProdDate:=NULL]
wells <- merge(wells, welldepth, by.x = "API10", by.y = "API", all.x = TRUE)

# Step 4: Replace missings with average
#save average well depth
avgdepth = mean(wells$depth, na.rm=T)
#populate missing well depth values with average depth value
wells[is.na(depth), depth:=avgdepth]

####################################
###### CALCULATE BONDS #############
####################################
#filter out plugged etc wells
# Capture well status as of 12/2023 --  lets think of this as a snapshot of what the policy would look like as of that date
wells = wells[is.na(AbandonDate)|AbandonDate>'2024-12-31']
wells = wells[FirstProdDate<=as.Date("2023-12-31")|is.na(FirstProdDate)]
wells = wells[welltype%in%c("OW", "GW", "OGW", "OWI", "GWI", "OGI", "GGI", "OWD", "GWD", "GI", "WS", "WI", "WD", "GS")]
wells = wells[wellstatus%in%c("DRL", "OPS", "P", "S", "TA", "PAI", "PII", "SAI", "SII", "A", "I")]

wells = wells[(LeaseType=="FEE"|LeaseType=="STATE")]


#calculate individual bond
wells[,ind_bond:=individual_well_bond(depth)]

#calculate blanket bonds by operator
#separate by wells that are less and more than 1000 feet
blanket_bonds=wells
blanket_bonds = blanket_bonds[,.N, by=.(Operator, depth<1000)]
blanket_bonds[,blanket_bond:=fcase(
  depth==T, 15000,
  depth==F, 120000
)]

ind_bonds = wells[,.(ind_bond=sum(ind_bond)), by=.(Operator, depth<1000)]

bonds = merge(blanket_bonds, ind_bonds, by=c('Operator', 'depth'), all.x=T, all.y=T)
bonds[,bond:=pmin(blanket_bond, ind_bond, na.rm=T)]
bonds[,pays_blanket:=bond==blanket_bond]
bonds[is.na(bonds)] = 0
bonds[,depth:=fcase(
  depth==T, "less1000",
  depth==F, "more1000"
)]

bonds_wide <- dcast(
  bonds,
  Operator ~ depth,
  value.var = c("N", "ind_bond", "blanket_bond", "bond", "pays_blanket")
)

bonds_wide[is.na(bonds_wide)]=0
#filter out firms with no fee/state
bonds_wide = bonds_wide[N_less1000+N_more1000>0]
bonds_wide[,current_bond:=bond_more1000+bond_less1000]
fwrite(bonds_wide, file.path(code_dir, 'CurrentReg', 'current_bonds.csv'))
