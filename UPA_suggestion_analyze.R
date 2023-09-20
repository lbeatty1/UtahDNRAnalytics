##########################################################################
## Code to analyze hypothetical change to percentage cutoffs for tiers####
##########################################################################

#basically just a copy of CategorizeFirms.R

###########################################
## Code to analyze new Utah bonding rule ##
## Written by Lauren Beatty, EDF ##########
## lbeatty@edf.org ########################
###########################################

rm(list=ls())

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(data.table)

setwd("C:/Users/lbeatty/Documents/")

source("UtahDNRAnalytics/bond_funs.R")

prod_data = read.csv("Production2020To2024.csv")      ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Production2020To2024.zip
wells = read.csv("Wells.csv")                         ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Wells.zip
wellhistory = read.csv("WellHistory.csv")             ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/WellHistory.zip

##################################################
## Format Data a little, and filter out garbage ##
##################################################

prod_data = prod_data%>%
  mutate(ReportPeriod=as.Date(ReportPeriod, "%m/%d/%Y"),
         Received = as.Date(Received, "%m/%d/%Y"),
         year = year(ReportPeriod),
         API=format(API, scientific=F),
         API=as.character(API))

# well documentation at https://oilgas.ogm.utah.gov/pub/database_download_read/1-Well_Data_Download_ReadMe.pdf
# filter out abandoned wells, storage/injection wells


wells = wells%>%
  mutate(API=format(API, scientific=F),
         API10=substr(API, 1, 10),
         AbandonDate=as.Date(AbandonDate,"%Y-%m-%d"))%>%
  filter(is.na(AbandonDate),
         #welltype%in%c("OW", "GW", "OGW", "OWI", "GWI", "OGI", "GGI", "OWD", "GWD"),
         !wellstatus%in%c("PA", "LA", "APD", "RET", "NEW", "DRL", "OPS"))

print(paste("Updated number of unique operators in well data:", length(unique(wells$OperatorNo))))
print(paste("Updated number of unique wells in wells data: ", length(unique(wells$API))))

wellhistory = wellhistory%>%
  mutate(API=as.character(WellID),
         FirstProdDate=as.Date(FirstProdDate, "%m/%d/%Y"),
         MD=as.numeric(MD),
         TVD=as.numeric(TVD))%>%
  select(API, FirstProdDate, SideTrack, MD, TVD, WorkType, CurrentWellStatus, CurrentWellType)
#looks like occasionally some work is done to deepen
#just need depths, so I'll take max by API10

welldepth=wellhistory%>%
  group_by(API)%>%
  summarise(depth=max(MD,na.rm=T),
            FirstProdDate=min(FirstProdDate, na.rm=T))

welldepth=welldepth%>%
  mutate(depth=replace(depth, depth==-Inf, NA),
         FirstProdDate=replace(FirstProdDate, FirstProdDate==Inf, NA))

wells=left_join(wells, welldepth, by=c("API10"="API"))
rm(wellhistory, welldepth)

################################
## Aggregate production####
################################

#collapse prod data to API10 level 
prod_data=prod_data%>%
  mutate(producing_flag = WellStatus%in%c("P", "PAI", "PII"))%>%
  group_by(API, Operator, ReportPeriod)%>%
  summarise(Oil=sum(Oil),
            Gas=sum(Gas),
            Water=sum(Water),
            producing_flag=max(producing_flag),
            shutin_flag=1-producing_flag)

#save length shut-in -- wells shut-in longer than 12 don't qualify for blankets
#this line creates a running counter for time shutin and time producing in months
#no tidyverse solution - must switch to data.table
prod_data = data.table(prod_data)
prod_data[order(ReportPeriod),time_shutin:=rowid(rleid(shutin_flag)),by=API]

prod_data = prod_data%>%
  mutate(time_producing=time_shutin*(producing_flag),
         time_shutin=time_shutin*shutin_flag)

#look from 2022-06-01 to 2023-05-01 for data availability
past_12_prod = prod_data%>%
  filter(ReportPeriod>=as.Date("2022-06-01"),
         ReportPeriod<as.Date("2023-06-01"))%>%
  group_by(API)%>%
  summarise(Oil=sum(Oil),
            Gas=sum(Gas),
            Water=sum(Water),
            n_reports=n())

status_052023 = prod_data%>%
  filter(ReportPeriod=="2023-05-01")%>%
  select(API, time_shutin, time_producing)  #save length of inactivity at 052023

status_max = prod_data%>%
  group_by(API)%>%
  arrange(ReportPeriod)%>%
  mutate(rownumber=row_number())%>%
  slice_max(n=1, order_by=rownumber)%>%
  select(API, time_shutin, time_producing)%>%
  rename(time_shutin_imputed = time_shutin,
         time_producing_imputed = time_producing)

past_12_prod=left_join(past_12_prod, status_052023, by="API")
past_12_prod = left_join(past_12_prod, status_max, by="API")

#replace missing with imputed values
past_12_prod = past_12_prod%>%
  mutate(time_shutin=coalesce(time_shutin, time_shutin_imputed),
         time_producing = coalesce(time_producing, time_producing_imputed))

well_data=left_join(wells, past_12_prod, by=c("API10"="API"))

##########
# quick search for missing prod data
missingdata=well_data%>%
  filter(is.na(Oil))
#doesn't seem too bad
rm(missingdata)

####################################
## Produce flags, figure out bonds##
####################################

#remove brand new wells, lets think of this as a snapshot of what the policy would look like on 06/23
well_data = well_data%>%
  filter(FirstProdDate<as.Date("2023-06-01")|is.na(FirstProdDate))

#save average well depth
avgdepth = mean(well_data$depth, na.rm=T)
#populate missings with average
well_data = well_data%>%
  mutate(depth=replace(depth, is.na(depth), avgdepth))


#calculate BOE/day, make flags for status and depth
well_data=well_data%>%
  mutate(Oil=replace(Oil, is.na(Oil), 0),
         Gas=replace(Gas, is.na(Gas), 0),
         BOEtot=Oil+Gas*6,
         BOEperday = BOEtot/(n_reports*30),
         BOEperday = replace(BOEperday, is.na(n_reports), 0),
         marginal_flag=BOEperday<=2,
         marginal_flag2 = BOEperday<=1,   #what if marginal threshold was 1?
         inactive_flag = wellstatus%in%c("S", "TA", "I"),
         inactive_marginal_flag=pmax(marginal_flag, inactive_flag),
         inactive_marginal_flag2 = pmax(marginal_flag2, inactive_flag),
         fee_state_flag=LeaseType=="FEE"|LeaseType=="STATE",
         shutin12_flag = time_shutin>12,                    #saves whether a well needs to be individually bonded
         depth_1000_flag=depth<=1000,
         depth_1000_3000_flag=depth>1000&depth<=3000,
         depth_3000_6000_flag = depth>3000&depth<=6000,     #these are the new depth thresholds
         depth_6000_9000_flag = depth>6000&depth<=9000,
         depth_9000_12000_flag = depth>9000&depth<=12000,
         depth_12000_flag = depth>12000,
         shutin12_flag = replace(shutin12_flag, is.na(shutin12_flag), 0)) #bunch of missing shutin12 flags -- looks like these are mostly injection wells but I'll fill all the missings in with 0 to be conservative

well_data%>%group_by(inactive_marginal_flag)%>%summarise(n=n())

# group by operator, get well counts
operator_dat = well_data%>%
  group_by(Operator, shutin12_flag)%>%
  summarise(avg_depth=mean(depth, na.rm=T),
            tot_BOE=sum(BOEtot),
            tot_wells=n(),
            depth_1000_wells = sum(depth_1000_flag,na.rm=T),
            depth_1000_3000_wells = sum(depth_1000_3000_flag,na.rm=T),
            depth_3000_6000_wells = sum(depth_3000_6000_flag,na.rm=T),
            depth_6000_9000_wells = sum(depth_6000_9000_flag, na.rm=T),
            depth_9000_12000_wells = sum(depth_9000_12000_flag,na.rm=T),
            depth_12000_wells = sum(depth_12000_flag,na.rm=T))

inactive_operator_dat=well_data%>%
  group_by(Operator, inactive_marginal_flag)%>%
  summarise(tot_inactive=n())%>%
  group_by(Operator)%>%
  mutate(tot_wells = sum(tot_inactive),
         pct_inactive=tot_inactive/tot_wells)%>%
  filter(inactive_marginal_flag==1)%>%
  select(Operator, tot_inactive, pct_inactive)

############################
#### CALCULATE OPERATOR TIER
############################
operator_dat=left_join(operator_dat, inactive_operator_dat, by="Operator")


operator_dat_shutin12 = operator_dat%>%
  filter(shutin12_flag==1)

#want to save true tot_wells for later
#note that tot_wells in operator_dat will only count active and inactive for less than 12 months wells for a while
#Since wells inactive for more than 12 months are not being covered by the blanket bond, its reasonable to assume they are not used in the 
#calculation of blanket bond amounts?
operator_n_wells = operator_dat%>%
  group_by(Operator)%>%
  summarise(tot_wells = sum(tot_wells))



#########
## Only real edit is here
########
operator_dat = operator_dat%>%
  filter(shutin12_flag==0)%>%
  mutate(tot_inactive = replace(tot_inactive, is.na(tot_inactive), 0),
         pct_inactive = replace(pct_inactive, is.na(pct_inactive), 0),
         BOEperday=tot_BOE/365,
         tier1 = BOEperday>=1000&pct_inactive<=0.2,
         tier2 = BOEperday>=500&pct_inactive<=0.25,
         tier3 = (BOEperday>=200&pct_inactive<=0.3)|(BOEperday>=1000),
         tier=4-tier1-tier2-tier3)


######################
## CALCULATE BONDS ##
######################

#calculate number of fee/state inactive wells by operator
#inactive well bonds are calculated only based on the fee/state status
#sorry for any confusion here, this is re-writing the data saved as 'inactive_operator_dat' from a few lines ago which was unneeded
inactive_operator_dat=well_data%>%
  group_by(Operator, inactive_marginal_flag, fee_state_flag)%>%
  summarise(tot_inactive_feestate=n())%>%
  filter(inactive_marginal_flag==1,
         fee_state_flag==1)%>%
  ungroup()%>%
  select(Operator, tot_inactive_feestate)

operator_dat=left_join(operator_dat, inactive_operator_dat, by="Operator")

#some operators don't have feestate inactive wells
operator_dat = operator_dat%>%
  mutate(tot_inactive_feestate=replace(tot_inactive_feestate, is.na(tot_inactive_feestate),0))


## Calculate new blanket bonds
operator_dat = operator_dat%>%
  mutate(tier1blanket = sapply(tot_wells, tier1_blanket),
         tier1marginal = sapply(avg_depth,tier1_marginalbond)*tot_inactive_feestate,
         tier2blanket = sapply(tot_wells, tier2_blanket),
         tier2marginal = sapply(avg_depth, tier2_marginalbond)*tot_inactive_feestate,
         tier3blanket = sapply(tot_wells, tier3_blanket),
         tier3marginal = sapply(avg_depth,tier3_marginalbond)*tot_inactive_feestate)


## Calculate per-well bonds
#per-well bond numbers come from April DOGM draft
operator_dat = operator_dat%>%
  mutate(depth_1000_bond = depth_1000_wells*10000,
         depth_1000_3000_bond = depth_1000_3000_wells*20000,
         depth_3000_6000_bond = depth_3000_6000_wells*40000,
         depth_6000_9000_bond = depth_6000_9000_wells*65000,
         depth_9000_12000_bond = depth_9000_12000_wells*85000,
         depth_12000_bond = depth_12000_wells*110000)

operator_dat_shutin12 = operator_dat_shutin12%>%
  mutate(depth_1000_bond = depth_1000_wells*10000,
         depth_1000_3000_bond = depth_1000_3000_wells*20000,
         depth_3000_6000_bond = depth_3000_6000_wells*40000,
         depth_6000_9000_bond = depth_6000_9000_wells*65000,
         depth_9000_12000_bond = depth_9000_12000_wells*85000,
         depth_12000_bond = depth_12000_wells*110000)

#take sum of operator long shutin wells
operator_dat_shutin12 = operator_dat_shutin12%>%
  mutate(shutin_well_bond = depth_1000_bond+depth_1000_3000_bond+depth_3000_6000_bond+depth_6000_9000_bond+depth_9000_12000_bond+depth_12000_bond)%>%
  select(Operator, shutin_well_bond)

#operator total bond will be sum of blanket bond, marginal/inactive fee/state bond, and individual well bonds for shutin wells shutin for more than 12 months
#for operators not meeting tiers, its simply the sum of individual well bonds
operator_dat=left_join(operator_dat, operator_dat_shutin12, by="Operator")
operator_dat = operator_dat%>%
  mutate(shutin_well_bond=replace(shutin_well_bond, is.na(shutin_well_bond), 0),
         bond = (tier1blanket+tier1marginal)*(tier==1)+(tier2blanket+tier2marginal)*(tier==2)+(tier3blanket+tier3marginal)*(tier==3)+(depth_1000_bond+depth_1000_3000_bond+depth_3000_6000_bond+depth_6000_9000_bond+depth_9000_12000_bond+depth_12000_bond)*(tier==4)+shutin_well_bond)


operator_dat=operator_dat%>%
  select(Operator, tier, bond, pct_inactive)%>%
  mutate(tier=as.character(tier),
         tier=replace(tier, tier=="4", "No tier"))%>%
  rename(tier_UPAsuggestion = tier,
         bond_UPAsuggestion = bond)

old = read.csv("UtahDNRAnalytics/Operator_dat.csv")

operator_dat=left_join(old, operator_dat, by="Operator")

histdatformat = pivot_longer(operator_dat, cols=c("tier", "tier_UPAsuggestion"))

ggplot(data=histdatformat)+
  geom_bar(aes(x=value, fill=name), position="dodge")+
  ylab("Number of Firms")+
  xlab("Tier")+
  ggtitle("Histogram of Firm Tiers")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/Histogram_Tiers_UPAsuggestion.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_dat)+
  geom_histogram(aes(x=pct_inactive))+
  ggtitle("Histogram of Inactive/Marginal Well Percentages by Firm")+
  xlab("% Marginal/Inactive")+
  ylab("# of Firms")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/Histogram_pct_inactive.jpg",
       device="jpg",
       height=5,
       width=7)


ggplot(data=operator_dat%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability2_marginal, col="Proposed Rule"))+
  geom_point(aes(x=bond_UPAsuggestion, y=liability2_marginal, col="UPA Suggestion"))+
  geom_segment(data=operator_dat%>%filter(bond!=bond_UPAsuggestion, bond<10000000),aes(x=bond, y=liability2_marginal, xend=bond_UPAsuggestion, yend=liability2_marginal), arrow=arrow(type="closed",length=unit(0.13,"cm"), angle=25))+
  geom_abline(slope=1, intercept=0)+
  ylab("Total Plugging Liabilities for Marginal/Inactive Wells")+
  xlab("Required Bonds")+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 75000 to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/UPA_Suggestion_Bond_Changes.jpg",
       device="jpg",
       height=5,
       width=7)
