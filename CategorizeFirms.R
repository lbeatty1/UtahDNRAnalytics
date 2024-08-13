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
setwd("C:/Users/cgerbode/Modeling/gitrepo/")
data_dir = 'UtahDNRAnalytics/UTData/'
code_dir = 'UtahDNRAnalytics/'

# Connect to supplementary functions file 'bond_funs.R'
source(paste(code_dir, "bond_funs.R", sep=''))

# Call data files downloaded previously from UT OGM (see main directory + all documentation readmes at: https://oilgas.ogm.utah.gov/oilgasweb/data-center/dc-main.xhtml#download)
prod_data = read.csv(paste(data_dir, "Production2020To2024.csv", sep=''))      ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Production2020To2024.zip
wells = read.csv(paste(data_dir, "Wells.csv", sep=''))                         ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Wells.zip
wellhistory = read.csv(paste(data_dir, "WellHistory.csv", sep=''))             ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/WellHistory.zip

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
#*********** CG note - This^ section was originally commented as "filter out abandoned wells"; that happens in lines

# Export summary numbers to descriptive_facts.csv
write.table(t(c("Updated number of unique operators in well data:", length(unique(wells$OperatorNo)))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(t(c("Updated number of unique wells in well data: ", length(unique(wells$API)))), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)

# Format key fields in Well_History.csv; select working cols for depth calc step below
wellhistory = wellhistory%>%
  mutate(API=as.character(WellID),
         FirstProdDate=as.Date(FirstProdDate, "%m/%d/%Y"),
         MD=as.numeric(MD),            # Measured Depth field
         TVD=as.numeric(TVD))%>%       # True Vertical Depth field 
  select(API, FirstProdDate, SideTrack, MD, TVD, WorkType, CurrentWellStatus, CurrentWellType)

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

# Update option for the future - check if there's an MD for entries missing TVD, and substitute this?
# For now, sticking with the average substitution used later for missing depths

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

##************FATAL ERROR CHARSXP MAY OCCUR DURING SECTION BELOW
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

##**********FATAL ERROR (Error: Cannot have attributes on a CHARSXP) sometimes, but not always, occurs somewhere in section above

######################################
## Filter out already-plugged wells ##
######################################

# Capture well status as of 12/2023 --  lets think of this as a snapshot of what the policy would look like as of that date

well_data = well_data%>%
  filter(is.na(AbandonDate)|AbandonDate>'2023-12-31', # keeps everything not listed as abandoned, or things abandoned after 2023
         FirstProdDate<=as.Date("2023-12-31")|is.na(FirstProdDate), # keeps everything producing before 12-2023, or NA
         welltype%in%c("OW", "GW", "OGW", "OWI", "GWI", "OGI", "GGI", "OWD", "GWD", "GI", "WS", "WI", "WD", "GS"), 
         wellstatus%in%c("APD", "DRL", "OPS", "P", "S", "TA", "PAI", "PII", "SAI", "SII", "A", "I"))
         
# quick search for missing prod data
missingdata_oilp=well_data%>%
  filter(is.na(Oil))
#lots of injection wells, a few shutin oil,gas wells

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
  mutate(
    # marginal_well flag creation based on BOE per day average: 
    # Replace NAs with O
         Oil=replace(Oil, is.na(Oil), 0),
         Gas=replace(Gas, is.na(Gas), 0),
    # calculate BOE per day average
         BOEtot=Oil+Gas/6, #1000 cfs gas ~= 6 barrel oil 
         BOEperday = BOEtot/(n_reports*30), # number of production months * 30 days/month
         BOEperday = replace(BOEperday, is.na(n_reports), 0), #replace NAs with 0
    # create flag for marginal well condition -- reduced from 2 BOE/day to 1 in new schedule
         marginal_flag=(BOEperday<=1&welltype%in%c("OW", "GW", "OGW","OWI","OGI","GWI","GGI","OWD","GWD")),
    # All depth flags
        depth_500_flag = (depth<500), #updated to include all depth bins in Aug. 2024 schedule
        depth_500_1000_flag = (depth>500&depth<=1000),
        depth_1000_3000_flag = (depth>1000&depth<=3000),
        depth_3000_6000_flag = (depth>3000&depth<=6000),     
        depth_6000_9000_flag = (depth>6000&depth<=9000),
        depth_9000_12000_flag = (depth>9000&depth<=12000),
        depth_12000_flag = (depth>12000),
    
    # OTHER STATUS FLAGS ----
        # create flag if well is of state or fee lease type
        fee_state_flag = (LeaseType=="FEE"|LeaseType=="STATE"), 

        # Shut-in flags - shutin time based on running production/shutin check above
        shutin_flag = time_shutin>0,   #in new schedule, all shut-in wells are classified 'at risk', so time changed from 12 to 0 
        shutin_flag = replace(shutin_flag, is.na(shutin_flag)&is.na(n_reports), 1),
        shutin_flag = replace(shutin_flag, is.na(shutin_flag), 0),  #bunch of missing shutin flags -- looks like these are mostly injection wells but I'll fill all the missings in with 0 to be conservative
        shutin_status_flag = (wellstatus%in%c("S")),
    # correct the shut-in flag for the non-producing active well types (DRL and APD)
        shutin_final = (shutin_flag==1),
        shutin_final = replace(shutin_final, wellstatus%in%c("APD", "DRL"), 0),
    
    # Active Well criteria
        #producing wells - captured by marginal_well = 0
        producing_flag = marginal_flag==0&shutin_flag==0&welltype%in%c("GW","OW", "OGW", "OWI", "GWI", "GWD", "OWD"),
        #drilling wells
        drilling_flag = wellstatus%in%c("DRL"),
        #active EOR
        #approved permit
        approved_flag = wellstatus%in%c("APD"),
        #active water wells (source, injection, disposal)
        active_water_flag = welltype%in%c("WI", "WD", "WS")&wellstatus%in%c("A"),

        active_flag = (pmax(drilling_flag, approved_flag, active_water_flag, producing_flag)),
    
    # At-Risk Well criteria
    
        # inactive flag - includes Shut-in, Temp. Abandoned, and Inactive O/G/OG wells
        prod_atrisk_flag = (marginal_flag==1&shutin_final==0),
        inactive_OG_flag = shutin_final==1&welltype%in%c("OW", "GW", "OGW"),
        temp_ab_flag = (wellstatus%in%c("TA")), # captures all TA wells
        ops_susp_flag = (wellstatus%in%c("OPS")),
        inactive_EOR_flag = (welltype%in%c("OWI", "GWI", "OGI", "GGI")&time_shutin>12),
        inactive_water_flag = (welltype%in%c("WI", "WD", "WS")&wellstatus%in%c("S", "TA", "I")),
        inactive_gas_serv_flag = (welltype%in%c("GI", "GS")&wellstatus%in%c("S","TA","I")),
    
        atrisk_flag=(pmax(prod_atrisk_flag, inactive_OG_flag, ops_susp_flag, temp_ab_flag, inactive_EOR_flag, inactive_water_flag, inactive_gas_serv_flag)>0)&!wellstatus%in%c("APD", "DRL"),
        complementarity = atrisk_flag!=active_flag)
        #end of single long mutate command above

#this flagging system is still missing a calc for whether inactive WI, WD, and WS wells have been inactive for 12 or more months. worth adding? 


well_class_breakout = well_data%>%
  group_by(complementarity, atrisk_flag, active_flag, welltype, wellstatus, shutin_flag, shutin_final, marginal_flag)%>%
  summarise(n=n())

#print table to .csv
write.table(c("", "", "", "Overview of Well Flag Classifications and Counts"), file=paste(code_dir, "/bond_sufficiency_stats.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(well_class_breakout, file=paste(code_dir, "/well_classifications.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)

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
  group_by(Operator, fee_state_flag)%>%
  summarise(tot_wells=n(),
            depth_500_wells = sum(depth_500_flag, na.rm=T),
            depth_500_1000_wells = sum(depth_500_1000_flag,na.rm=T),
            depth_1000_3000_wells = sum(depth_1000_3000_flag,na.rm=T),
            depth_3000_6000_wells = sum(depth_3000_6000_flag,na.rm=T),
            depth_6000_9000_wells = sum(depth_6000_9000_flag, na.rm=T),
            depth_9000_12000_wells = sum(depth_9000_12000_flag,na.rm=T),
            depth_12000_wells = sum(depth_12000_flag,na.rm=T))


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
  filter(fee_state_flag==1)%>%
  mutate(depth_500_bond = depth_500_wells*5000,
         depth_500_1000_bond = depth_500_1000_wells*10000,
         depth_1000_3000_bond = depth_1000_3000_wells*20000,
         depth_3000_6000_bond = depth_3000_6000_wells*40000,
         depth_6000_9000_bond = depth_6000_9000_wells*65000,
         depth_9000_12000_bond = depth_9000_12000_wells*85000,
         depth_12000_bond = depth_12000_wells*110000,
         individual_well_bond = depth_500_bond+depth_500_1000_bond+depth_1000_3000_bond+depth_3000_6000_bond+depth_6000_9000_bond+depth_9000_12000_bond+depth_12000_bond)

operator_stats = left_join(operator_stats, operator_individualbonds, by="Operator")


#For tiered operators total bond will be sum of blanket bond for tier, at-risk fee/state bond, and individual well bonds if applicable
#for operators not meeting tiers, its simply the sum of individual well bonds
operator_stats = operator_stats%>%
  mutate(tier1marginal = replace(tier1marginal, is.na(tier1marginal),0),
         tier2marginal = replace(tier2marginal, is.na(tier2marginal),0),
         tier3marginal = replace(tier3marginal, is.na(tier3marginal),0),
         individual_well_bond=replace(individual_well_bond, is.na(individual_well_bond), 0),
         bond = (tier1blanket+tier1marginal)*(tier==1)+(tier2blanket+tier2marginal)*(tier==2)+(tier3blanket+tier3marginal)*(tier==3)+individual_well_bond*(tier==4),
         )


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
         liability4 = depth*12)
# sum liabilities by operator
operator_liabilities = well_data%>%
  group_by(Operator)%>%
  summarise(liability1=sum(liability1),
            liability2=sum(liability2),
            liability3=sum(liability3),
            liability4=sum(liability4))

operator_stats = left_join(operator_stats, operator_liabilities, by="Operator")
operator_stats=operator_stats%>%
  arrange(desc(tier))%>%
  mutate(tier=as.character(tier),
         tier=replace(tier, tier=="4", "No tier"),
        )

########################################################
## Compare & plot bonds against projected liabilities ##
########################################################

## Sum of at-risk well liabilities across the 4 cost assumptions
operator_liabilities_atrisk = well_data%>%
  filter(atrisk_flag==1)%>%
  group_by(Operator)%>%
  summarise(liability1_atrisk=sum(liability1),
            liability2_atrisk=sum(liability2),
            liability3_atrisk=sum(liability3),
            liability4_atrisk=sum(liability4))

operator_stats = left_join(operator_stats, operator_liabilities_atrisk, by="Operator")
## Make plots of bond size vs. liability size, for each assumption
# operators are color coded by Tier

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

################################
## Plots for small/Tier4 firms##
################################

ggplot(data=operator_stats%>%filter(tier=="No tier", tot_BOE<1000000))+
  geom_point(aes(x=bond, y=liability2_atrisk))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Tier 4 Firms Which Produce Less than 1,000,000 BOE/yr \n Plugging liabilities exceed bond amounts if plugging costs are high.")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 75,000 to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/AtRiskLiabilities2_Tier4SmallFirms.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_stats%>%filter(tier=="No tier", tot_BOE<1000000))+
  geom_point(aes(x=bond, y=liability1_atrisk))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("Tier 4 Firms Which Produce Less than 1,000,000 BOE/yr \n Plugging liabilities are less than bond amounts if plugging costs are low")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for At-Risk Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 37,500 to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/AtRiskLiabilities1_Tier4SmallFirms.jpg",
       device="jpg",
       height=5,
       width=7)

##########################################
## Stats for fee/state liabilities only ##
##########################################

operator_liabilities_feestate = well_data%>%
  filter(fee_state_flag==1)%>%
  group_by(Operator)%>%
  summarise(liability1_feestate=sum(liability1),
            liability2_feestate=sum(liability2),
            liability3_feestate=sum(liability3),
            liability4_feestate=sum(liability4))
operator_stats = left_join(operator_stats, operator_liabilities_feestate, by="Operator")

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



#######################
## Fee/state At-risk###
#######################

operator_liabilities_feestate_atrisk = well_data%>%
  filter(fee_state_flag==1,
         atrisk_flag==1)%>%
  group_by(Operator)%>%
  summarise(liability1_feestate_atrisk=sum(liability1),
            liability2_feestate_atrisk=sum(liability2),
            liability3_feestate_atrisk=sum(liability3),
            liability4_feestate_atrisk=sum(liability4))
operator_stats = left_join(operator_stats, operator_liabilities_feestate_atrisk, by="Operator")

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


###################################################################
## Calculate stats on how bonds compare to projected liabilities ##
###################################################################

operator_stats = operator_stats%>%
  mutate(liability1_pct = bond/liability1,
         liability2_pct = bond/liability2,
         liability3_pct = bond/liability3,
         liability4_pct = bond/liability4,
         liability1fs_pct = bond/liability1_feestate,
         liability2fs_pct = bond/liability2_feestate,
         liability3fs_pct = bond/liability3_feestate,
         liability4fs_pct = bond/liability4_feestate,
         liability1ar_pct = bond/liability1_atrisk,
         liability2ar_pct = bond/liability2_atrisk,
         liability3ar_pct = bond/liability3_atrisk,
         liability4ar_pct = bond/liability4_atrisk, 
         liability1arfs_pct = bond/liability1_feestate_atrisk,
         liability2arfs_pct = bond/liability2_feestate_atrisk,
         liability3arfs_pct = bond/liability3_feestate_atrisk,
         liability4arfs_pct = bond/liability4_feestate_atrisk)


suff_stats_total = operator_stats%>%
  summarize(L1_pct = mean(liability1_pct, na.rm=T),
            L2_pct = mean(liability2_pct, na.rm=T),
            L3_pct = mean(liability3_pct, na.rm=T),
            L4_pct = mean(liability4_pct, na.rm=T),
            L1fs_pct = mean(liability1fs_pct, na.rm=T), 
            L2fs_pct = mean(liability2fs_pct, na.rm=T),
            L3fs_pct = mean(liability3fs_pct, na.rm=T),
            L4fs_pct = mean(liability4fs_pct, na.rm=T),
            L1ar_pct = mean(liability1ar_pct, na.rm=T), 
            L2ar_pct = mean(liability2ar_pct, na.rm=T),
            L3ar_pct = mean(liability3ar_pct, na.rm=T),
            L4ar_pct = mean(liability4ar_pct, na.rm=T),
            L1arfs_pct = mean(liability1arfs_pct, na.rm=T), 
            L2arfs_pct = mean(liability2arfs_pct, na.rm=T),
            L3arfs_pct = mean(liability3arfs_pct, na.rm=T),
            L4arfs_pct = mean(liability4arfs_pct, na.rm=T))
suff_stats_total = cbind(group ="All operators", suff_stats_total)


suff_stats_tier1 = operator_stats%>%
  filter(tier=="1")%>%
  summarize(L1_pct = mean(liability1_pct, na.rm=T),
            L2_pct = mean(liability2_pct, na.rm=T),
            L3_pct = mean(liability3_pct, na.rm=T),
            L4_pct = mean(liability4_pct, na.rm=T),
            L1fs_pct = mean(liability1fs_pct, na.rm=T), 
            L2fs_pct = mean(liability2fs_pct, na.rm=T),
            L3fs_pct = mean(liability3fs_pct, na.rm=T),
            L4fs_pct = mean(liability4fs_pct, na.rm=T),
            L1ar_pct = mean(liability1ar_pct, na.rm=T), 
            L2ar_pct = mean(liability2ar_pct, na.rm=T),
            L3ar_pct = mean(liability3ar_pct, na.rm=T),
            L4ar_pct = mean(liability4ar_pct, na.rm=T),
            L1arfs_pct = mean(liability1arfs_pct, na.rm=T), 
            L2arfs_pct = mean(liability2arfs_pct, na.rm=T),
            L3arfs_pct = mean(liability3arfs_pct, na.rm=T),
            L4arfs_pct = mean(liability4arfs_pct, na.rm=T))

suff_stats_tier1 = cbind(group = "Tier 1", suff_stats_tier1)
  
suff_stats_tier2 = operator_stats%>%
  filter(tier=="2")%>%
  summarize(L1_pct = mean(liability1_pct, na.rm=T),
            L2_pct = mean(liability2_pct, na.rm=T),
            L3_pct = mean(liability3_pct, na.rm=T),
            L4_pct = mean(liability4_pct, na.rm=T),
            L1fs_pct = mean(liability1fs_pct, na.rm=T), 
            L2fs_pct = mean(liability2fs_pct, na.rm=T),
            L3fs_pct = mean(liability3fs_pct, na.rm=T),
            L4fs_pct = mean(liability4fs_pct, na.rm=T),
            L1ar_pct = mean(liability1ar_pct, na.rm=T), 
            L2ar_pct = mean(liability2ar_pct, na.rm=T),
            L3ar_pct = mean(liability3ar_pct, na.rm=T),
            L4ar_pct = mean(liability4ar_pct, na.rm=T),
            L1arfs_pct = mean(liability1arfs_pct, na.rm=T), 
            L2arfs_pct = mean(liability2arfs_pct, na.rm=T),
            L3arfs_pct = mean(liability3arfs_pct, na.rm=T),
            L4arfs_pct = mean(liability4arfs_pct, na.rm=T))

suff_stats_tier2 = cbind(group = "Tier 2", suff_stats_tier2)

suff_stats_tier3 = operator_stats%>%
  filter(tier=="3")%>%
  summarize(L1_pct = mean(liability1_pct, na.rm=T),
            L2_pct = mean(liability2_pct, na.rm=T),
            L3_pct = mean(liability3_pct, na.rm=T),
            L4_pct = mean(liability4_pct, na.rm=T),
            L1fs_pct = mean(liability1fs_pct, na.rm=T), 
            L2fs_pct = mean(liability2fs_pct, na.rm=T),
            L3fs_pct = mean(liability3fs_pct, na.rm=T),
            L4fs_pct = mean(liability4fs_pct, na.rm=T),
            L1ar_pct = mean(liability1ar_pct, na.rm=T), 
            L2ar_pct = mean(liability2ar_pct, na.rm=T),
            L3ar_pct = mean(liability3ar_pct, na.rm=T),
            L4ar_pct = mean(liability4ar_pct, na.rm=T),
            L1arfs_pct = mean(liability1arfs_pct, na.rm=T), 
            L2arfs_pct = mean(liability2arfs_pct, na.rm=T),
            L3arfs_pct = mean(liability3arfs_pct, na.rm=T),
            L4arfs_pct = mean(liability4arfs_pct, na.rm=T))
suff_stats_tier3 = cbind(group = "Tier 3", suff_stats_tier3)

suff_stats_tier4 = operator_stats%>%
  filter(tier=="No tier")%>%
  summarize(L1_pct = mean(liability1_pct, na.rm=T),
            L2_pct = mean(liability2_pct, na.rm=T),
            L3_pct = mean(liability3_pct, na.rm=T),
            L4_pct = mean(liability4_pct, na.rm=T),
            L1fs_pct = mean(liability1fs_pct, na.rm=T), 
            L2fs_pct = mean(liability2fs_pct, na.rm=T),
            L3fs_pct = mean(liability3fs_pct, na.rm=T),
            L4fs_pct = mean(liability4fs_pct, na.rm=T),
            L1ar_pct = mean(liability1ar_pct, na.rm=T), 
            L2ar_pct = mean(liability2ar_pct, na.rm=T),
            L3ar_pct = mean(liability3ar_pct, na.rm=T),
            L4ar_pct = mean(liability4ar_pct, na.rm=T),
            L1arfs_pct = mean(liability1arfs_pct, na.rm=T), 
            L2arfs_pct = mean(liability2arfs_pct, na.rm=T),
            L3arfs_pct = mean(liability3arfs_pct, na.rm=T),
            L4arfs_pct = mean(liability4arfs_pct, na.rm=T))

suff_stats_tier4 = cbind(group = "No tier", suff_stats_tier4)

suff_stats_all = bind_rows(suff_stats_total, suff_stats_tier1, suff_stats_tier2, suff_stats_tier3, suff_stats_tier4)

#print table to .csv
write.table(c("", "", "", "Average % of liability covered by bond, by Tier Group"), file=paste(code_dir, "/bond_sufficiency_stats.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(suff_stats_all, file=paste(code_dir, "/bond_sufficiency_stats.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)

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
            liability4 = sum(liability4,na.rm=T))

sum_small_risky=cbind("Sum of liability from small operators", sum_small_risky)

write.table(c("", "", "", "Sum of liabilities"), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(sum_small_risky, file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)


small_risky_underbonded = small_risky_operators%>%
  filter(bond<liability2_atrisk)%>%
  summarise(n_firms=n(),
            sum_bonds = sum(bond,na.rm=T),
            sum_liability = sum(liability2_atrisk))

write.table(c("", "", "", "Small firms where marginal well liabilities exceed bonds (assuming $75k per well)"), file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = F)
write.table(small_risky_underbonded, file=paste(code_dir, "/descriptive_facts.csv", sep=""), sep=",", row.names=F, append=T, col.names = T)

write.csv(operator_stats%>%select(Operator, tot_operator_wells, avg_depth, avg_atrisk_depth, tot_feestate_wells, tot_atrisk, BOEperday, tier, bond, tot_atrisk_feestate, starts_with("liability")), "UtahDNRAnalytics/Operator_dat.csv")

