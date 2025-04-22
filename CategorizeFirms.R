###########################################
## Code to analyze new Utah bonding rule ##
## Written by Lauren Beatty, EDF ##########
## lbeatty@edf.org ########################
###########################################
## Updated 2024 Aug 06 ####################
## to reflect new July 2024 proposed tier##
## definitions and fee schedule ###########
## by Christine Gerbode, EDF###############
## cgerbode@edf.org #######################
###########################################

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

#############################
## Get descriptive numbers ##
#############################

# Create a CSV file in code directory to store descriptive info as needed
write.table(data.frame(), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=F)

write.table(t(c("Number of unique operators in production data:", length(unique(prod_data$Oper_No)))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(t(c("Number of unique wells in production data: ", length(unique(prod_data$API)))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)

write.table(t(c("Number of unique operators in well data:", length(unique(wells$OperatorNo)))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(t(c("Number of unique wells in well data: ", length(unique(wells$API)))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)

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

# Export summary numbers to descriptive_facts.csv
write.table(t(c("Updated number of unique operators in well data:", length(unique(wells$OperatorNo)))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(t(c("Updated number of unique wells in well data: ", length(unique(wells$API)))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)


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

##produce histogram of depths
ggplot(data=well_data%>%filter(depth<25000))+
  geom_histogram(aes(x=depth))+
  ggtitle("Histogram of Well Depths")+
  ylab("Count")+
  xlab("Depth (ft)")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/DepthHistogram.jpg",
       device="jpg",
       height=5,
       width=7)

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


#print table to .csv
write.table(c("", "", "", "Overview of Well Flag Classifications and Counts"), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(well_data%>%group_by(fee_state_flag, shutin_flag, producing_flag, active_flag, atrisk_flag)%>%summarise(n=n()), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)

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
operator_atrisk_depth = well_data[fee_state_flag==1&atrisk_flag==1,.(avg_atrisk_depth=mean(depth,na.rm=T)), by=Operator]

#save number of fee/state wells (used to calculate blanket bond amount for tiered firms)
operator_feestate = well_data[fee_state_flag==1, .(tot_feestate_wells=.N), by=Operator]

operator_stats = merge(operator_stats, operator_feestate, by="Operator", all.x=T)
operator_stats = merge(operator_stats, operator_atrisk_depth, by="Operator", all.x=T)

# Calculate percentage at-risk wells for each operator (used to determine tier)
atrisk_operator_dat = well_data[,.(n_wells=.N), by=.(Operator, atrisk_flag)]
atrisk_operator_dat[,tot_wells:=sum(n_wells), by=Operator]
atrisk_operator_dat[, pct_atrisk:=n_wells/tot_wells]
atrisk_operator_dat = atrisk_operator_dat[atrisk_flag==1]
atrisk_operator_dat[,c('atrisk_flag', 'n_wells'):=NULL]
names(atrisk_operator_dat) = c('Operator', 'tot_atrisk', 'pct_atrisk')
operator_stats = merge(operator_stats, atrisk_operator_dat, by="Operator", all.x=T)


### Calculate tier
operator_stats[is.na(tot_atrisk), tot_atrisk:=0]
operator_stats[is.na(pct_atrisk), pct_atrisk:=0]
operator_stats[,BOEperday:=tot_BOE/365]
operator_stats[,tier1:=BOEperday>=1000&pct_atrisk<=0.15]
operator_stats[,tier2:=BOEperday>=500&pct_atrisk<=0.2]
operator_stats[,tier3:=(BOEperday>=200&pct_atrisk<=0.25)|(BOEperday>=1000)]
operator_stats[,tier:=4-tier1-tier2-tier3]
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

## Calculate new blanket bonds (functions in bonds_funs.R; updated to reflect July 2024 draft schedule)
operator_stats[, `:=`(
  tier1blanket  = sapply(tot_feestate_wells, tier1_blanket),
  tier1marginal = sapply(avg_atrisk_depth, tier1_marginalbond) * (tot_atrisk_feestate/10)*10,  #change here they round down to nearest 10
  tier2blanket  = sapply(tot_feestate_wells, tier2_blanket),
  tier2marginal = sapply(avg_atrisk_depth, tier2_marginalbond) * (tot_atrisk_feestate/10)*10,
  tier3blanket  = sapply(tot_feestate_wells, tier3_blanket),
  tier3marginal = sapply(avg_atrisk_depth, tier3_marginalbond) * (tot_atrisk_feestate/10)*10
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
  individual_well_bond = fifelse(is.na(individual_well_bond), 0, individual_well_bond),
  bond = (tier1blanket + tier1marginal) * (tier == 1) +
    (tier2blanket + tier2marginal) * (tier == 2) +
    (tier3blanket + tier3marginal) * (tier == 3) +
    individual_well_bond * (tier == 4)
)]

# Filter out operators with no fee/state wells
operator_stats <- operator_stats[tot_feestate_wells > 0]

####################################################
#####  Calculate Projected Plugging Liabilities ####
####################################################

#liability1 is from the spreadsheet on average decomissioning costs by state
#liability2 is the median decomissioning cost in Raimi
#liability3 assumes $6 per foot of depth
#liability4 assumes $12 per foot of depth
well_data = well_data%>%
  mutate(liability1 = 37500,
         liability2 = 75000,
         liability3 = depth*6,
         liability4 = depth*12,
         liability5 = depth*9)
# sum liabilities by operator
operator_liabilities = well_data%>%
  group_by(Operator)%>%
  summarise(liability1=sum(liability1),
            liability2=sum(liability2),
            liability3=sum(liability3),
            liability4=sum(liability4),
            liability5=sum(liability5))

operator_stats = left_join(operator_stats, operator_liabilities, by="Operator")
operator_stats=operator_stats%>%
  arrange(desc(tier))%>%
  mutate(tier=as.character(tier),
         tier=replace(tier, tier=="4", "No tier"))


## Sum of at-risk well liabilities across the 4 cost assumptions
operator_liabilities_atrisk = well_data%>%
  filter(atrisk_flag==1)%>%
  group_by(Operator)%>%
  summarise(liability1_atrisk=sum(liability1),
            liability2_atrisk=sum(liability2),
            liability3_atrisk=sum(liability3),
            liability4_atrisk=sum(liability4),
            liability5_atrisk=sum(liability5))

operator_stats = left_join(operator_stats, operator_liabilities_atrisk, by="Operator")

operator_liabilities_feestate = well_data%>%
  filter(fee_state_flag==1)%>%
  group_by(Operator)%>%
  summarise(liability1_feestate=sum(liability1),
            liability2_feestate=sum(liability2),
            liability3_feestate=sum(liability3),
            liability4_feestate=sum(liability4),
            liability5_feestate=sum(liability5))
operator_stats = left_join(operator_stats, operator_liabilities_feestate, by="Operator")

operator_liabilities_feestate_atrisk = well_data%>%
  filter(fee_state_flag==1,
         atrisk_flag==1)%>%
  group_by(Operator)%>%
  summarise(liability1_feestate_atrisk=sum(liability1),
            liability2_feestate_atrisk=sum(liability2),
            liability3_feestate_atrisk=sum(liability3),
            liability4_feestate_atrisk=sum(liability4),
            liability5_feestate_atrisk=sum(liability5))
operator_stats = left_join(operator_stats, operator_liabilities_feestate_atrisk, by="Operator")

########################################################
## Compare & plot bonds against projected liabilities ##
########################################################
## Assumption 1
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability1_atrisk, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 37500 to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/AtRiskLiabilities1.jpg",
       device="jpg",
       height=5,
       width=7)

## Assumption 2
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability2_atrisk, colour=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds don't cover marginal and inactive well plugging liability \n if plugging costs are high")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $75,000 to plug.")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/AtRiskLiabilities2.jpg",
       device="jpg",
       height=5,
       width=7)

## Assumption 3
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability3_atrisk, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $6/foot to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/AtRiskLiabilities3.jpg",
       device="jpg",
       height=5,
       width=7)

## Assumption 4
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability4_atrisk, colour=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("If plugging costs are high, then firms which don't meet tier \n requirements (tier 4) look covered.")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for AtRisk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12/foot to plug.")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/AtRiskLiabilities4.jpg",
       device="jpg",
       height=5,
       width=7)

## Assumption 5
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability5_atrisk, colour=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("If plugging costs are high, then firms which don't meet tier \n requirements (tier 4) look covered.")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for AtRisk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $9/foot to plug.")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/AtRiskLiabilities5.jpg",
       device="jpg",
       height=5,
       width=7)

##########################################
## Stats for fee/state liabilities only ##
##########################################
# Assumption 1
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability1_feestate, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 37500 to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/FeeStateLiability1.jpg",
       device="jpg",
       height=5,
       width=7)

# Assumption 2
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability2_feestate, color=tier))+
  geom_abline(slope=1, intercept=0)+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $75,000 to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/FeeStateLiability2.jpg",
       device="jpg",
       height=5,
       width=7)

# Assumption 3
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability3_feestate, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $6 per foot to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/FeeStateLiability3.jpg",
       device="jpg",
       height=5,
       width=7)

# Assumption 4
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability4_feestate, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/FeeStateLiability4.jpg",
       device="jpg",
       height=5,
       width=7)

# Assumption 5
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability5_feestate, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $9 per foot to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/FeeStateLiability5.jpg",
       device="jpg",
       height=5,
       width=7)





#######################
## Fee/state At-risk###
#######################
#Assumption 1
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability1_feestate_atrisk, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 37500 to plug.")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeStateAtRiskLiability1.jpg",
       device="jpg",
       height=5,
       width=7)

#Assumption 2
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability2_feestate_atrisk, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $75,000 to plug.")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeStateAtRiskLiability2.jpg",
       device="jpg",
       height=5,
       width=7)

#Assumption 3
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability3_feestate_atrisk, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $6 per foot to plug.")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeStateAtRiskLiability3.jpg",
       device="jpg",
       height=5,
       width=7)

#Assumption 4
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability4_feestate_atrisk, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeStateAtRiskLiability4.jpg",
       device="jpg",
       height=5,
       width=7)


#Assumption 5
ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability5_feestate_atrisk, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $9 per foot to plug.")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeStateAtRiskLiability5.jpg",
       device="jpg",
       height=5,
       width=7)

###################################################################
## Calculate stats on how bonds compare to projected liabilities ##
###################################################################

operator_stats = operator_stats%>%
  mutate(liability2_pct = bond/liability2,
         liability2fc_pct = bond/liability2_feestate,
         liability2ar_pct = bond/liability2_atrisk,
         liability2arfs_pct = bond/liability2_feestate_atrisk)

ggplot(operator_stats)+
  geom_density(aes(x=liability2arfs_pct, fill=tier), alpha=0.4)+
  xlim(0,3)+
  xlab("Percent of Fee/State At-Risk Liabilities Covered")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeStateAtRisk_PCT_Density.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(operator_stats)+
  geom_density(aes(x=liability2fc_pct, fill=tier), alpha=0.4)+
  xlim(0,2)+
  xlab("Percent of Fee/State Liabilities Covered")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeState_PCT_Density.jpg",
       device="jpg",
       height=5,
       width=7)


suff_stats_total = operator_stats%>%
  group_by(tier)%>%
  summarise(sum_liability2=sum(liability2,na.rm=T),
            sum_liability2_feestate=sum(liability2_feestate,na.rm=T),
            sum_liability2_atrisk = sum(liability2_atrisk,na.rm=T),
            sum_liability2_feestate_atrisk = sum(liability2_feestate_atrisk, na.rm=T),
            sum_bonds = sum(bond,na.rm=T))%>%
  mutate(liability2_pct = sum_bonds/sum_liability2,
         liability2_feestate_pct = sum_bonds/sum_liability2_feestate,
         liability2_atrisk_pct = sum_bonds/sum_liability2_atrisk,
         liability2_feestate_atrisk_pct = sum_bonds/sum_liability2_feestate_atrisk)


ggplot(suff_stats_total)+
  geom_col(aes(tier,liability2_feestate_pct), fill='darkblue')+
  ylab("Mean Proportion of Fee/State Liabilities Covered")+
  xlab("Tier")+
  labs(caption="Costs are calculated assuming each well costs $75,000 to plug")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeState_PCT_Bar.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(suff_stats_total)+
  geom_col(aes(tier,liability2_feestate_atrisk_pct), fill='darkblue')+
  ylab("Mean Proportion of Fee/State At-Risk Liabilities Covered")+
  labs(caption="Costs are calculated assuming each well costs $75,000 to plug")+
  xlab("Tier")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeState_Atrisk_PCT_Bar.jpg",
       device="jpg",
       height=5,
       width=7)

#print table to .csv
write.table(c("", "", "", "Average % of liability covered by bond, by Tier Group"), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(suff_stats_total, file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)

##########################################################################
## Explore wells operated by small firms where liabilities exceed bonds ##
##########################################################################

small_risky_operators = operator_stats%>%
  filter(tot_BOE<1000000)

write.table(t(c("Small operators (tot_BOE<1000000) N Wells:", sum(small_risky_operators$tot_operator_wells))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(t(c("Small operators (tot_BOE<1000000) State Well PCT:", sum(small_risky_operators$tot_operator_wells)/sum(operator_stats$tot_operator_wells))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)

write.table(t(c("Small operators (tot_BOE<1000000) N At-Risk Wells:", sum(small_risky_operators$tot_atrisk,na.rm=T))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(t(c("Small operators (tot_BOE<1000000) State Well Inactive PCT:", sum(small_risky_operators$tot_atrisk,na.rm=T)/sum(operator_stats$tot_atrisk, na.rm=T))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)

sum_small_risky = small_risky_operators%>%
  summarise(liability1 = sum(liability1,na.rm=T),
            liability2 = sum(liability2,na.rm=T),
            liability3 = sum(liability3,na.rm=T),
            liability4 = sum(liability4,na.rm=T),
            liability2 = sum(liability2,na.rm=T))

sum_small_risky=cbind("Sum of liability from small operators", sum_small_risky)

write.table(c("", "", "", "Sum of liabilities"), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(sum_small_risky, file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)


small_risky_underbonded = small_risky_operators%>%
  filter(bond<liability2_atrisk)%>%
  summarise(n_firms=n(),
            sum_bonds = sum(bond,na.rm=T),
            sum_liability = sum(liability2_atrisk))

write.table(c("", "", "", "Small firms where marginal well liabilities exceed bonds (assuming $9/foot)"), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(small_risky_underbonded, file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)

write.csv(operator_stats%>%select(Operator, tot_operator_wells, avg_depth, avg_atrisk_depth, tot_feestate_wells, tot_atrisk, tot_atrisk_feestate, BOEperday, tier, bond, tot_atrisk_feestate, starts_with("liability")), "UtahDNRAnalytics/Operator_dat.csv")


