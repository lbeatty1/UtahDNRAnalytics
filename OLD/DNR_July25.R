###########################################
## Code to analyze new Utah bonding rule ##
## Written by Lauren Beatty, EDF ##########
## lbeatty@edf.org ########################


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
prod_data = fread(paste(data_dir, "Production2020To2024.csv", sep=''))      ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Production2020To2024.zip
wells = fread(paste(data_dir, "Wells.csv", sep=''))                         ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Wells.zip
wellhistory = fread(paste(data_dir, "WellHistory.csv", sep=''))             ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/WellHistory.zip

depth_bins=c(0,500,1000,3000,6000,9000,12000,Inf)
cost_per_foot = 9

##################################################
## Format Data a little, and filter out garbage ##
##################################################
prod_data = data.table(prod_data)
# Change class and/or format for key fields in Production data
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
  TVD = as.numeric(TVD)
)]

# Keep only selected columns
wellhistory <- wellhistory[, .(API, FirstProdDate, SideTrack, MD, TVD, WorkType, CurrentWellStatus, CurrentWellType)]


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

# Step 4: Clean up
rm(wellhistory, welldepth)


####################################################################
## Aggregate production data and explore producing/shut-in status ##
####################################################################

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
prod_data[order(ReportPeriod),time_shutin:=rowid(rleid(shutin_flag)),by=API]
prod_data[,time_producing:=time_shutin*producing_flag]
prod_data[,time_shutin:=time_shutin*shutin_flag]


# Filter to 2023-01-01 to 2023-12-01, assuming data is available
past_12_prod = prod_data[ReportPeriod>='2023-01-01'&ReportPeriod<='2023-12-01']
past_12_prod = past_12_prod[,.(
  Oil = sum(Oil),
  Gas = sum(Gas),
  Water = sum(Water),
  n_reports = .N
), by=API]

status_122023 = prod_data[ReportPeriod=='2023-12-01', c('API', 'time_shutin', 'time_producing')]
# Find producing/shut-in status as of 12-2023
#save length of running (in/)activity at 12-2023

# In case there are missing records for the month used for this status check, find the most recent max to substitute
status_max <- prod_data[ReportPeriod <= as.Date("2023-12-01")][
  order(API, ReportPeriod),
  rownumber := seq_len(.N),
  by = API
][
  , .SD[.N], by = API
][
  , .(API, time_shutin_imputed = time_shutin, time_producing_imputed = time_producing)
]

# Join the two attempts to find the last status
past_12_prod = merge(past_12_prod, status_122023, by="API", all.x=T)
past_12_prod = merge(past_12_prod, status_max, by="API", all.x=T)

# Replace missing status with imputed values (if any are missing. Aas of 8/2024 none of the 2023 records appear to be missing, but this may not be true for more recent data)
past_12_prod[,time_shutin:=coalesce(time_shutin, time_shutin_imputed)]
past_12_prod[,time_producing:=coalesce(time_producing, time_producing)]

# Join finalized status columns to dataset
well_data=merge(wells, past_12_prod, by.x='API10', by.y='API', all.x=T)

######################################
## Filter out already-plugged wells ##
######################################

# Capture well status as of 12/2023 --  lets think of this as a snapshot of what the policy would look like as of that date
well_data = well_data[is.na(AbandonDate)|AbandonDate>'2023-12-31']
well_data = well_data[FirstProdDate<=as.Date("2023-12-31")|is.na(FirstProdDate)]
well_data = well_data[welltype%in%c("OW", "GW", "OGW", "OWI", "GWI", "OGI", "GGI", "OWD", "GWD", "GI", "WS", "WI", "WD", "GS")]
well_data = well_data[wellstatus%in%c("DRL", "OPS", "P", "S", "TA", "PAI", "PII", "SAI", "SII", "A", "I")]

################################
## Well depth plot and cleanup##
################################

#save average well depth
avgdepth = mean(well_data$depth, na.rm=T)
#populate missing well depth values with average depth value
well_data[is.na(depth), depth:=avgdepth]

####################################
## Produce flags, figure out bonds##
####################################
##See README.md for breakdown of the schedule classifications flagged below: 

well_data[is.na(Oil), Oil:=0]
well_data[is.na(Gas), Gas:=0]

well_data[,BOEtot:=Oil+Gas/5.8]
well_data[,BOEperday:=BOEtot/(n_reports*30)]
well_data[is.na(n_reports), BOEperday:=0]

# create flag for marginal well condition -- reduced from 2 BOE/day to 1 in new schedule
well_data[,marginal_flag:=(BOEperday<=1&welltype%in%c("OW", "GW", "OGW","OWI","OGI","GWI","GGI","OWD","GWD")&(!wellstatus%in%c("APD", "DRL")))]

# Get Depth Bin
well_data[,depth_bin:=get_bin(depth, depth_bins)]

# create flag if well is of state or fee lease type
well_data[,fee_state_flag := (LeaseType=="FEE"|LeaseType=="STATE")]

#shutin flag -- months shutin has to be greater than 12 (note: law is a little ambiguous here but this is a common condition, you don't for example, want to flag a well that was shut off for one month as shutin)
#wells that don't report or are labeled shutin are shutin, wells that are labeled active service wells aren't labeled shutin
well_data[,shutin_flag:=time_shutin>12]
well_data[is.na(shutin_flag)&is.na(n_reports), shutin_flag:=T]
well_data[is.na(shutin_flag), shutin_flag:=F]
well_data[wellstatus%in%c('DRL', 'A'), shutin_flag:=F]

#producing, non-marginal, nonservice wells
well_data[,producing_flag := marginal_flag==0&shutin_flag==0&welltype%in%c("GW","OW", "OGW", "OWI", "GWI", "GWD", "OWD")]

#active production and service wells
well_data[,active_flag := (wellstatus%in%c("DRL"))|(welltype%in%c("WI", "WD", "WS", "GS", "GI")&wellstatus%in%c("A"))|producing_flag==1]

well_data[,atrisk_flag:=pmax(marginal_flag, shutin_flag, wellstatus%in%c("TA", "OPS"), welltype%in%c("WI", "WD", "WS", "GI", "GS")&wellstatus%in%c("S", "TA", "I"))]

########################
## Find Operator Tier ##
########################
#need total production, percentage inactive wells
#also save avg depth, total inactive fee/state wells for use in bond calculation later

#Save total wells, average depth of all wells, and total daily production BOE (for use in calculating tier)
operator_stats = well_data[,.(
  tot_operator_wells=.N,
  avg_depth=mean(depth,na.rm=T),
  tot_BOE=sum(BOEtot)
), by=Operator]

# save average depth of at-risk wells
operator_atrisk_depth = well_data[fee_state_flag==1&atrisk_flag==1,.(
  avg_atrisk_depth=mean(depth,na.rm=T)
), by=Operator]

#save number of fee/state wells (used to calculate blanket bond amount for tiered firms)
operator_feestate = well_data[fee_state_flag==1, .(tot_feestate_wells=.N), by=Operator]

operator_stats = merge(operator_stats, operator_feestate, by="Operator", all.x=T)
operator_stats = merge(operator_stats, operator_atrisk_depth, by="Operator", all.x=T)

# Calculate percentage at-risk wells for each operator (used to determine tier)
atrisk_operator_dat = well_data[,.(n_wells=.N), by=.(Operator, atrisk_flag)]
atrisk_operator_dat[,tot_wells:=sum(n_wells), by=Operator]
atrisk_operator_dat[, pct_atrisk:=n_wells/tot_wells]
atrisk_operator_dat = atrisk_operator_dat[atrisk_flag==1]
atrisk_operator_dat[,c('atrisk_flag', 'tot_wells'):=NULL]
names(atrisk_operator_dat) = c('Operator', 'tot_atrisk', 'pct_atrisk')
operator_stats = merge(operator_stats, atrisk_operator_dat, by="Operator", all.x=T)


### Calculate tier
operator_stats[is.na(tot_atrisk), tot_atrisk:=0]
operator_stats[is.na(pct_atrisk), pct_atrisk:=0]
operator_stats[,BOEperday:=tot_BOE/365]
operator_stats[,tier1:=BOEperday>=1000&pct_atrisk<=0.2]
operator_stats[,tier2:=BOEperday>=500&pct_atrisk<=0.2]
operator_stats[,tier3:=(BOEperday>=200&pct_atrisk<=0.25)|(BOEperday>=1000)]
operator_stats[,tier:=4-tier1-tier2-tier3]

#old tiers
operator_stats[,tier1_old:=BOEperday>=1000&pct_atrisk<=0.15]
operator_stats[,tier2_old:=BOEperday>=500&pct_atrisk<=0.2]
operator_stats[,tier3_old:=(BOEperday>=200&pct_atrisk<=0.25)|BOEperday>=1000]
operator_stats[,tier_old:=4-tier1_old-tier2_old-tier3_old]
######################
## CALCULATE BONDS ##
######################
# group by operator save wells of each depth by whether fee/state
operator_dat= well_data[,.(n=.N), by=.(Operator, fee_state_flag, depth_bin)]

operator_dat <- dcast(
  operator_dat,
  Operator + fee_state_flag ~ depth_bin,
  value.var = "n",
  fill = 0
)

# Rename depth_bin columns to have prefix "depth_bin_"
setnames(operator_dat,
         old = setdiff(names(operator_dat), c("Operator", "fee_state_flag")),
         new = paste0("depth_bin_", setdiff(names(operator_dat), c("Operator", "fee_state_flag"))))

# Create tot_wells as row sum of depth_bin_* columns
depth_bin_cols <- grep("^depth_bin_", names(operator_dat), value = TRUE)
operator_dat[, tot_wells := rowSums(.SD), .SDcols = depth_bin_cols]

#save at-risk wells on fee/state land
atrisk_operator_dat = well_data[,.(tot_atrisk_feestate=.N), by=.(Operator, atrisk_flag, fee_state_flag)]
atrisk_operator_dat = atrisk_operator_dat[atrisk_flag==1&fee_state_flag==1]
atrisk_operator_dat[, c('atrisk_flag', 'fee_state_flag'):=NULL]

operator_stats=merge(operator_stats, atrisk_operator_dat, by="Operator", all.x=T)


#some operators don't have feestate inactive wells
operator_stats[is.na(tot_atrisk_feestate), tot_atrisk_feestate:=0]
operator_stats[is.na(tot_feestate_wells), tot_feestate_wells:=0]

#calculate tier bonds
#upa change allows for deduction to at-risk count
operator_stats[tier==1, at_risk_threshold:=tot_feestate_wells*0.2]
operator_stats[tier==2, at_risk_threshold:=tot_feestate_wells*0.13]
operator_stats[tier==3, at_risk_threshold:=tot_feestate_wells*0.08]
operator_stats[, marginal_bond_wells_alt:= pmax(tot_atrisk_feestate-at_risk_threshold,0)]
operator_stats[,marginal_bond_wells_alt:=floor(marginal_bond_wells_alt/10)*10]

operator_stats[, `:=`(
  tier1blanket  = sapply(tot_feestate_wells, tier1_blanket),
  tier1marginal = sapply(avg_depth, tier1_marginalbond) * marginal_bond_wells_alt,  #change here they round down to nearest 10
  tier2blanket  = sapply(tot_feestate_wells, tier2_blanket),
  tier2marginal = sapply(avg_depth, tier2_marginalbond) * marginal_bond_wells_alt,
  tier3blanket  = sapply(tot_feestate_wells, tier3_blanket),
  tier3marginal = sapply(avg_depth, tier3_marginalbond) * marginal_bond_wells_alt
)]
#do same for old
operator_stats[, `:=`(
  tier1blanket_old  = sapply(tot_feestate_wells, tier1_blanket_old),
  tier1marginal_old = sapply(avg_depth, tier1_marginalbond_old) * floor(tot_atrisk_feestate/10)*10,  #change here they round down to nearest 10
  tier2blanket_old  = sapply(tot_feestate_wells, tier2_blanket_old),
  tier2marginal_old = sapply(avg_depth, tier2_marginalbond_old) * floor(tot_atrisk_feestate/10)*10,
  tier3blanket_old  = sapply(tot_feestate_wells, tier3_blanket_old),
  tier3marginal_old = sapply(avg_depth, tier3_marginalbond_old) * floor(tot_atrisk_feestate/10)*10
)]


## Calculate Individual Well Depth bonds
#per-well bond numbers come from July 2024 Bond Schedule draft
operator_individualbonds = operator_dat[fee_state_flag==1]
ind_bonds = c(5000,10000,20000,40000,65000,85000,110000)
cols = paste0('depth_bin_', 1:7)
operator_individualbonds[, (cols) := Map(`*`, .SD, ind_bonds), .SDcols = cols]

depth_cols = grep("^depth_bin_", names(operator_individualbonds), value = TRUE)
operator_individualbonds[, individual_well_bond := rowSums(.SD), .SDcols = depth_cols]

operator_stats = merge(operator_stats, operator_individualbonds[,c('Operator', 'individual_well_bond')], by="Operator", all.x=T)


#For tiered operators total bond will be sum of blanket bond for tier, at-risk fee/state bond, and individual well bonds if applicable
#for operators not meeting tiers, its simply the sum of individual well bonds
operator_stats[, `:=`(
  tier1marginal = fifelse(is.na(tier1marginal), 0, tier1marginal),
  tier2marginal = fifelse(is.na(tier2marginal), 0, tier2marginal),
  tier3marginal = fifelse(is.na(tier3marginal), 0, tier3marginal),
  tier1marginal_old = fifelse(is.na(tier1marginal_old), 0, tier1marginal_old),
  tier2marginal_old = fifelse(is.na(tier2marginal_old), 0, tier2marginal_old),
  tier3marginal_old = fifelse(is.na(tier3marginal_old), 0, tier3marginal_old),
  individual_well_bond = fifelse(is.na(individual_well_bond), 0, individual_well_bond)
)]

operator_stats[,bond := (tier1blanket + tier1marginal) * (tier == 1) +
                 (tier2blanket + tier2marginal) * (tier == 2) +
                 (tier3blanket + tier3marginal) * (tier == 3) +
                 individual_well_bond * (tier == 4)]

operator_stats[,bond_old := (tier1blanket_old + tier1marginal_old) * (tier_old == 1) +
                 (tier2blanket_old + tier2marginal_old) * (tier_old == 2) +
                 (tier3blanket_old + tier3marginal_old) * (tier_old == 3) +
                 individual_well_bond * (tier_old == 4)]

# Filter out operators with no fee/state wells
operator_stats <- operator_stats[tot_feestate_wells > 0]

operator_stats_old=fread('UtahDNRAnalytics/Operator_dat.csv')
operator_stats = operator_stats[Operator!='Orphan-No Responsible Operator']


#make bar chart of average percentage of feestate/atrisk covered
operator_stats[,feestate_liability:=75000*tot_feestate_wells]
operator_stats[,pct_covered:=bond/feestate_liability]
operator_stats[,pct_covered_old:=bond_old/feestate_liability]
barchart = operator_stats[,.(
  pct_covered = mean(bond/feestate_liability),
  pct_covered_old=mean(bond_old/feestate_liability),
  sum_bonds = sum(bond),
  sum_bonds_old = sum(bond_old)
), by=tier_old]

df_long <- barchart %>%
  pivot_longer(
    cols = c(sum_bonds, sum_bonds_old, pct_covered, pct_covered_old),
    names_to = "type",
    values_to = "value"
  )

# optional: clean up labels
df_long$type <- recode(df_long$type,
                       pct_covered = "Current",
                       pct_covered_old = "Old",
                       sum_bonds = "Sum of Bonds",
                       sum_bonds_old = "Sum of Old Bonds")
df_long=data.table(df_long)

# bar plot
ggplot(df_long[type=='Current'|type=='Old'], aes(x = factor(tier_old), y = value, fill = type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Original Tier",
    y = "Percent Fee/State Liabilities Covered",
    fill = "Regulation"
  ) +
  theme_minimal()

ggplot(df_long[type %in% c('Sum of Old Bonds', 'Sum of Bonds')], 
       aes(x = factor(tier_old), y = value, fill = type)) +
  geom_col(position = "dodge") +
  scale_y_continuous(
    labels = function(x) dollar(x / 1e6, suffix = "M")
  ) +
  labs(
    x = "Original Tier",
    y = "Sum of Total Bonds (Millions of $)",
    fill = "Regulation"
  ) +
  theme_minimal()

