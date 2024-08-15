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
setwd("C:/Users/lbeatty/Documents/")
data_dir = 'UtahData/'
code_dir = 'UtahDNRAnalytics/'

# Connect to supplementary functions file 'bond_funs.R'
source(paste(code_dir, "bond_funs.R", sep=''))

# Call data files downloaded previously from UT OGM (see main directory + all documentation readmes at: https://oilgas.ogm.utah.gov/oilgasweb/data-center/dc-main.xhtml#download)
prod_data = read.csv(paste(data_dir, "Production2020To2024.csv", sep=''))      ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Production2020To2024.zip
wells = read.csv(paste(data_dir, "Wells.csv", sep=''))                         ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Wells.zip
wellhistory = read.csv(paste(data_dir, "WellHistory.csv", sep=''))             ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/WellHistory.zip

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

# Change class and/or format for key fields in Production data
prod_data = prod_data%>%
  mutate(ReportPeriod=as.Date(ReportPeriod, "%m/%d/%Y"),
         Received = as.Date(Received, "%m/%d/%Y"),
         year = data.table::year(ReportPeriod),
         API=format(API, scientific=F),
         API=as.character(API))

# Format key fields in Wells.csv
wells = wells%>%
  mutate(API=format(API, scientific=F),
         API10=substr(API, 1, 10),
         AbandonDate=as.Date(AbandonDate,"%Y-%m-%d"))

# Format key fields in Well_History.csv; select working cols for depth calc step below
wellhistory = wellhistory%>%
  mutate(API=as.character(WellID),
         FirstProdDate=as.Date(FirstProdDate, "%m/%d/%Y"),
         MD=as.numeric(MD),            # Measured Depth field
         TVD=as.numeric(TVD))%>%       # True Vertical Depth field 
  select(API, FirstProdDate, SideTrack, MD, TVD, WorkType, CurrentWellStatus, CurrentWellType)


# Export summary numbers to descriptive_facts.csv
write.table(t(c("Updated number of unique operators in well data:", length(unique(wells$OperatorNo)))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(t(c("Updated number of unique wells in well data: ", length(unique(wells$API)))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)


####################################################
## Calculate max depth for each borehole by API10 ##
####################################################

# Occasionally some work is done to deepen boreholes, yielding duplicate entries
# We need the final depths, so we can just find max depth by API10:
welldepth=wellhistory%>%
  group_by(API)%>%
  summarise(depth=max(TVD,na.rm=T),   # Takes max TVD where duplicates exist per API10
            FirstProdDate=min(FirstProdDate, na.rm=T)) # Takes earliest production date

# Where data isn't available, substitute NA for now
welldepth=welldepth%>%
  mutate(depth=replace(depth, depth==-Inf, NA),
         FirstProdDate=replace(FirstProdDate, FirstProdDate==Inf, NA))

# Join depth info to Wells record and delete depth and history dataframes
wells=left_join(wells, welldepth, by=c("API10"="API"))
rm(wellhistory, welldepth)

####################################################################
## Aggregate production data and explore producing/shut-in status ##
####################################################################

# Collapse production data to API10 level, capturing all wells in the borehole
prod_data=prod_data%>%
  mutate(producing_flag = WellStatus%in%c("P", "PAI", "PII"))%>%
  group_by(API, Operator, ReportPeriod)%>% # in case of operator switch during sample period
  summarise(Oil=sum(Oil),
            Gas=sum(Gas),
            Water=sum(Water),
            producing_flag=max(producing_flag),
            shutin_flag=1-producing_flag) # instead of creating field based on S, SAI, and SII statuses

# Find length of production and shut-in periods

# This section creates a running counter for time shutin and time producing, in months
# Note: no tidyverse solution - switch to data.table
prod_data = data.table(prod_data)
prod_data[order(ReportPeriod),time_shutin:=rowid(rleid(shutin_flag)),by=API]

prod_data = prod_data%>%
  mutate(time_producing=time_shutin*(producing_flag),
         time_shutin=time_shutin*shutin_flag)

# Filter to 2023-01-01 to 2023-12-01, assuming data is available
past_12_prod = prod_data%>%
  filter(ReportPeriod>=as.Date("2023-01-01"),
         ReportPeriod<=as.Date("2023-12-01"))%>%
  group_by(API)%>%
  summarise(Oil=sum(Oil),
            Gas=sum(Gas),
            Water=sum(Water),
            n_reports=n())

# Find producing/shut-in status as of 12-2023
status_122023 = prod_data%>%
  dplyr::filter(ReportPeriod=="2023-12-01")%>%
  select(API, time_shutin, time_producing)  #save length of running (in/)activity at 12-2023

# In case there are missing records for the month used for this status check, find the most recent max to substitute
status_max = prod_data%>%
  group_by(API)%>%
  dplyr::filter(ReportPeriod<=as.Date("2023-12-01"))%>%
  arrange(ReportPeriod)%>%
  mutate(rownumber=row_number())%>%
  slice_max(n=1, order_by=rownumber)%>%
  select(API, time_shutin, time_producing)%>%
  rename(time_shutin_imputed = time_shutin,
         time_producing_imputed = time_producing)

# Join the two attempts to find the last status
past_12_prod=left_join(past_12_prod, status_122023, by="API")
past_12_prod = left_join(past_12_prod, status_max, by="API")

# Replace missing status with imputed values (if any are missing. Aas of 8/2024 none of the 2023 records appear to be missing, but this may not be true for more recent data)
past_12_prod = past_12_prod%>%
  mutate(time_shutin=coalesce(time_shutin, time_shutin_imputed),
         time_producing = coalesce(time_producing, time_producing_imputed))%>%
  select(-c(time_shutin_imputed, time_producing_imputed))

# Join finalized status columns to dataset
well_data=left_join(wells, past_12_prod, by=c("API10"="API"))

######################################
## Filter out already-plugged wells ##
######################################

# Capture well status as of 12/2023 --  lets think of this as a snapshot of what the policy would look like as of that date

well_data = well_data%>%
  filter(is.na(AbandonDate)|AbandonDate>'2023-12-31', # keeps everything not listed as abandoned, or things abandoned after 2023
         FirstProdDate<=as.Date("2023-12-31")|is.na(FirstProdDate), # keeps everything producing before 12-2023, or NA
         welltype%in%c("OW", "GW", "OGW", "OWI", "GWI", "OGI", "GGI", "OWD", "GWD", "GI", "WS", "WI", "WD", "GS"), 
         wellstatus%in%c("APD", "DRL", "OPS", "P", "S", "TA", "PAI", "PII", "SAI", "SII", "A", "I"))
         

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
well_data = well_data%>%
  mutate(depth=replace(depth, is.na(depth), avgdepth))

####################################
## Produce flags, figure out bonds##
####################################
##See README.md for breakdown of the schedule classifications flagged below: 
well_data=well_data%>%
  mutate(Oil=replace(Oil, is.na(Oil), 0),
         Gas=replace(Gas, is.na(Gas), 0),
         
         # calculate BOE per day average
         BOEtot=Oil+Gas/6, #1000 cfs gas ~= 6 barrel oil 
         BOEperday = BOEtot/(n_reports*30), # number of production months * 30 days/month
         BOEperday = replace(BOEperday, is.na(n_reports), 0), #replace NAs with 0
         
         # create flag for marginal well condition -- reduced from 2 BOE/day to 1 in new schedule
         marginal_flag=(BOEperday<=1&welltype%in%c("OW", "GW", "OGW","OWI","OGI","GWI","GGI","OWD","GWD")&(!wellstatus%in%c("APD", "DRL"))),
         
         # Get Depth Bin
         depth_bin=get_bin(depth, depth_bins),
         
         # OTHER STATUS FLAGS ----
         # create flag if well is of state or fee lease type
         fee_state_flag = (LeaseType=="FEE"|LeaseType=="STATE"), 
    
         # Shut-in flags - shutin time based on running production/shutin check above
         shutin_flag = time_shutin>0,   #in new schedule, all shut-in wells are classified 'at risk', so time changed from 12 to 0 
         shutin_flag = replace(shutin_flag, is.na(shutin_flag)&is.na(n_reports), 1),
         shutin_flag = replace(shutin_flag, is.na(shutin_flag), 0),  #bunch of missing shutin flags -- looks like these are mostly injection wells but I'll fill all the missings in with 0 to be conservative
         shutin_flag = replace(shutin_flag, wellstatus%in%c("APD", "DRL", "A"), 0),
          
         # Active Well criteria
         #producing, non-marginal wells
         producing_flag = marginal_flag==0&shutin_flag==0&welltype%in%c("GW","OW", "OGW", "OWI", "GWI", "GWD", "OWD"),
         
         #get all approved and drilled, active water and injection wells, and producing, non-marginal
         active_flag = (wellstatus%in%c("DRL", "APD"))|(welltype%in%c("WI", "WD", "WS", "GS", "GI")&wellstatus%in%c("A"))|producing_flag==1,
          
         # At-Risk Well criteria
         atrisk_flag=pmax(marginal_flag, shutin_flag, wellstatus%in%c("TA", "OPS"), welltype%in%c("WI", "WD", "WS", "GI", "GS")&wellstatus%in%c("S", "TA", "I")))

#print table to .csv
write.table(c("", "", "", "Overview of Well Flag Classifications and Counts"), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(well_data%>%group_by(fee_state_flag, shutin_flag, producing_flag, active_flag, atrisk_flag)%>%summarise(n=n()), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)

########################
## Find Operator Tier ##
########################
#need total production, percentage inactive wells
#also save avg depth, total inactive fee/state wells for use in bond calculation later

#Save total wells, average depth of all wells, and total daily production BOE (for use in calculating tier)
operator_stats = well_data%>%
  group_by(Operator)%>%
  summarise(tot_operator_wells=n(),
            avg_depth=mean(depth,na.rm=T),
            tot_BOE=sum(BOEtot))

# save average depth of at-risk wells
operator_atrisk_depth = well_data%>%
  group_by(Operator)%>%
  filter(fee_state_flag==1,
         atrisk_flag==1)%>%
  summarise(avg_atrisk_depth = mean(depth, na.rm=T))

#save number of fee/state wells (used to calculate blanket bond amount for tiered firms)
operator_feestate = well_data%>%
  filter(fee_state_flag==1)%>%
  group_by(Operator)%>%
  summarise(tot_feestate_wells = n())

operator_stats = left_join(operator_stats, operator_feestate, by="Operator")
operator_stats = left_join(operator_stats, operator_atrisk_depth, by="Operator")

# Calculate percentage at-risk wells for each operator (used to determine tier)
atrisk_operator_dat=well_data%>%
  group_by(Operator, atrisk_flag)%>%
  summarise(tot_atrisk=n())%>%
  group_by(Operator)%>%
  mutate(tot_wells = sum(tot_atrisk),
         pct_atrisk=tot_atrisk/tot_wells)%>%
  filter(atrisk_flag==1)%>%
  select(Operator, tot_atrisk, pct_atrisk)

operator_stats = left_join(operator_stats, atrisk_operator_dat, by="Operator")


### Calculate tier
operator_stats = operator_stats%>%
  mutate(tot_atrisk = replace(tot_atrisk, is.na(tot_atrisk), 0),
         pct_atrisk = replace(pct_atrisk, is.na(pct_atrisk), 0),
         BOEperday=tot_BOE/365,
         tier1 = BOEperday>=1000&pct_atrisk<=0.15,
         tier2 = BOEperday>=500&pct_atrisk<=0.2,
         tier3 = (BOEperday>=200&pct_atrisk<=0.25)|(BOEperday>=1000),
         tier=4-tier1-tier2-tier3)

######################
## CALCULATE BONDS ##
######################

# group by operator save wells of each depth by whether fee/state

operator_dat = well_data%>%
  group_by(Operator, fee_state_flag, depth_bin)%>%
  summarise(n=n())

operator_dat=pivot_wider(operator_dat, id_cols=c("Operator", "fee_state_flag"), names_from="depth_bin", values_from="n", names_prefix="depth_bin_")

operator_dat=operator_dat %>%
  mutate(across(starts_with("depth_bin_"), ~replace_na(., 0)),
         tot_wells = rowSums(across(starts_with("depth_bin_"))))


#save at-risk wells on fee/state land
atrisk_operator_dat=well_data%>%
  group_by(Operator, atrisk_flag, fee_state_flag)%>%
  summarise(tot_atrisk_feestate=n())%>%
  filter(atrisk_flag==1,
         fee_state_flag==1)%>%
  ungroup()%>%
  select(Operator, tot_atrisk_feestate)

operator_stats=left_join(operator_stats, atrisk_operator_dat, by="Operator")


#some operators don't have feestate inactive wells
operator_stats = operator_stats%>%
  mutate(tot_atrisk_feestate=replace(tot_atrisk_feestate, is.na(tot_atrisk_feestate),0),
         tot_feestate_wells = replace(tot_feestate_wells, is.na(tot_feestate_wells),0))


## Calculate new blanket bonds (functions in bonds_funs.R; updated to reflect July 2024 draft schedule)
operator_stats = operator_stats%>%
  mutate(tier1blanket = sapply(tot_feestate_wells, tier1_blanket),
         tier1marginal = sapply(avg_atrisk_depth,tier1_marginalbond)*tot_atrisk_feestate,
         tier2blanket = sapply(tot_feestate_wells, tier2_blanket),
         tier2marginal = sapply(avg_atrisk_depth, tier2_marginalbond)*tot_atrisk_feestate,
         tier3blanket = sapply(tot_feestate_wells, tier3_blanket),
         tier3marginal = sapply(avg_atrisk_depth,tier3_marginalbond)*tot_atrisk_feestate)


## Calculate Individual Well Depth bonds
#per-well bond numbers come from July 2024 Bond Schedule draft
operator_individualbonds = operator_dat%>%
  ungroup()%>%
  filter(fee_state_flag==1)%>%
  select(sort(colnames(operator_dat)))%>%
  select(Operator, starts_with("depth_bin"))

depth_bond=operator_individualbonds[,2:8]*c(5000,10000,20000,40000,65000,85000,110000)
individual_well_bond = rowSums(depth_bond)

operator_individualbonds$individual_well_bond = individual_well_bond
operator_stats = left_join(operator_stats, operator_individualbonds, by="Operator")


#For tiered operators total bond will be sum of blanket bond for tier, at-risk fee/state bond, and individual well bonds if applicable
#for operators not meeting tiers, its simply the sum of individual well bonds
operator_stats = operator_stats%>%
  mutate(tier1marginal = replace(tier1marginal, is.na(tier1marginal),0),
         tier2marginal = replace(tier2marginal, is.na(tier2marginal),0),
         tier3marginal = replace(tier3marginal, is.na(tier3marginal),0),
         individual_well_bond=replace(individual_well_bond, is.na(individual_well_bond), 0),
         bond = (tier1blanket+tier1marginal)*(tier==1)+(tier2blanket+tier2marginal)*(tier==2)+(tier3blanket+tier3marginal)*(tier==3)+individual_well_bond*(tier==4))


#get rid of operators without fee/state wells
operator_stats=operator_stats%>%
  filter(tot_feestate_wells>0)

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
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 75000 to plug.")+
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

################################
## Plots for small/Tier4 firms##
################################

ggplot(data=operator_stats%>%filter(tier=="No tier", tot_BOE<1000000))+
  geom_point(aes(x=bond, y=liability5_atrisk))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Tier 4 Firms Which Produce Less than 1,000,000 BOE/yr \n Plugging liabilities exceed bond amounts if plugging costs are high.")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $9/foot to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/AtRiskLiabilities5_Tier4SmallFirms.jpg",
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
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 75000 to plug.")+
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
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 75000 to plug.")+
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
  geom_point(aes(x=bond, y=liability4_feestate_atrisk, color=tier))+
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
  mutate(liability5_pct = bond/liability5,
         liability5fs_pct = bond/liability5_feestate,
         liability5ar_pct = bond/liability5_atrisk,
         liability5arfs_pct = bond/liability5_feestate_atrisk)

ggplot(operator_stats)+
  geom_density(aes(x=liability5arfs_pct, fill=tier), alpha=0.4)+
  xlim(0,3)+
  xlab("Percent of Fee/State At-Risk Liabilities Covered")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeStateAtRisk_PCT_Density.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(operator_stats)+
  geom_density(aes(x=liability5fs_pct, fill=tier), alpha=0.4)+
  xlim(0,2)+
  xlab("Percent of Fee/State Liabilities Covered")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeState_PCT_Density.jpg",
       device="jpg",
       height=5,
       width=7)


suff_stats_total = operator_stats%>%
  group_by(tier)%>%
  summarise(sum_liability5=sum(liability5,na.rm=T),
            sum_liability5_feestate=sum(liability5_feestate,na.rm=T),
            sum_liability5_atrisk = sum(liability5_atrisk,na.rm=T),
            sum_liability5_feestate_atrisk = sum(liability5_feestate_atrisk, na.rm=T),
            sum_bonds = sum(bond,na.rm=T))%>%
  mutate(liability5_pct = sum_bonds/sum_liability5,
         liability5_feestate_pct = sum_bonds/sum_liability5_feestate,
         liability5_atrisk_pct = sum_bonds/sum_liability5_atrisk,
         liability5_feestate_atrisk_pct = sum_bonds/sum_liability5_feestate_atrisk)


ggplot(suff_stats_total)+
  geom_col(aes(tier,liability5_feestate_pct), fill='darkblue')+
  ylab("Percent of Fee/State Liabilities Covered")+
  xlab("Tier")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeState_PCT_Bar.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(suff_stats_total)+
  geom_col(aes(tier,liability5_feestate_atrisk_pct), fill='darkblue')+
  ylab("Percent of Fee/State At-Risk Liabilities Covered")+
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
            liability5 = sum(liability5,na.rm=T))

sum_small_risky=cbind("Sum of liability from small operators", sum_small_risky)

write.table(c("", "", "", "Sum of liabilities"), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(sum_small_risky, file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)


small_risky_underbonded = small_risky_operators%>%
  filter(bond<liability5_atrisk)%>%
  summarise(n_firms=n(),
            sum_bonds = sum(bond,na.rm=T),
            sum_liability = sum(liability2_atrisk))

write.table(c("", "", "", "Small firms where marginal well liabilities exceed bonds (assuming $9/foot)"), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(small_risky_underbonded, file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)

write.csv(operator_stats%>%select(Operator, tot_operator_wells, avg_depth, avg_atrisk_depth, tot_feestate_wells, tot_atrisk, tot_atrisk_feestate, BOEperday, tier, bond, tot_atrisk_feestate, starts_with("liability")), "UtahDNRAnalytics/Operator_dat.csv")

