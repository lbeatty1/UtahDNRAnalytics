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

######################################
## GENERAL OUTLINE OF PROCEDURE ######
# 1. Clean, format data
# 2. Take production data and look a the year from 06/2022 through 05/2023
# 3. Create lots of flags for wells - inactive, inactive for >12 months, on fee/state land, depth flags
# 4.By operator, sum over wells to figure out how many wells of each type they own, calculate which tier operators fall in
# 5. Calculate bonds for each operator based on their imputed tier
# 6. Calculate plugging liabilities using a couple different assumptions
# 7. Make plots
# 8. Calculate some final stats on small operators
# 9. Write operator data to csv

setwd("C:/Users/lbeatty/Documents/")
data_dir = 'UtahData/'

source("UtahDNRAnalytics/bond_funs.R")

prod_data = read.csv(paste(data_dir, "Production2020To2024.csv", sep=''))      ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Production2020To2024.zip
wells = read.csv(paste(data_dir, "Wells.csv", sep=''))                         ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Wells.zip
wellhistory = read.csv(paste(data_dir, "WellHistory.csv", sep=''))             ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/WellHistory.zip

#maybe 300 form approvals
#look at lease type
#####################
# Get some numbers ##
#####################

print(paste("Number of unique operators in production data:", length(unique(prod_data$Oper_No))))
print(paste("Number of unique wells in production data: ", length(unique(prod_data$API))))

print(paste("Number of unique operators in well data:", length(unique(wells$OperatorNo))))
print(paste("Number of unique wells in wells data: ", length(unique(wells$API))))

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
         AbandonDate=as.Date(AbandonDate,"%Y-%m-%d"))

print(paste("Updated number of unique operators in well data:", length(unique(wells$OperatorNo))))
print(paste("Updated number of unique wells in wells data: ", length(unique(wells$API))))


##### Get depth by API10
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
  summarise(depth=max(TVD,na.rm=T),
            FirstProdDate=min(FirstProdDate, na.rm=T))

welldepth=welldepth%>%
  mutate(depth=replace(depth, depth==-Inf, NA),
         FirstProdDate=replace(FirstProdDate, FirstProdDate==Inf, NA))

wells=left_join(wells, welldepth, by=c("API10"="API"))
rm(wellhistory, welldepth)

# injection_wells = wells%>%
#   filter(welltype%in%c("WI", "GI", "WD", "WS", "GS"))
# 
# wells = wells%>%
#   filter(welltype%in%c("OW", "GW", "OGW", "OWI", "GWI", "OGI", "GGI", "OWD", "GWD"))

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

#look from 2023-01-01 to 2023-12-01 for data availability
past_12_prod = prod_data%>%
  filter(ReportPeriod>=as.Date("2023-01-01"),
         ReportPeriod<=as.Date("2023-12-01"))%>%
  group_by(API)%>%
  summarise(Oil=sum(Oil),
            Gas=sum(Gas),
            Water=sum(Water),
            n_reports=n())

status_122023 = prod_data%>%
  filter(ReportPeriod=="2023-12-01")%>%
  select(API, time_shutin, time_producing)  #save length of inactivity at 12-2023

status_max = prod_data%>%
  group_by(API)%>%
  filter(ReportPeriod<=as.Date("2023-12-01"))%>%
  arrange(ReportPeriod)%>%
  mutate(rownumber=row_number())%>%
  slice_max(n=1, order_by=rownumber)%>%
  select(API, time_shutin, time_producing)%>%
  rename(time_shutin_imputed = time_shutin,
         time_producing_imputed = time_producing)

past_12_prod=left_join(past_12_prod, status_122023, by="API")
past_12_prod = left_join(past_12_prod, status_max, by="API")

#replace missing with imputed values
past_12_prod = past_12_prod%>%
  mutate(time_shutin=coalesce(time_shutin, time_shutin_imputed),
         time_producing = coalesce(time_producing, time_producing_imputed))%>%
  select(-c(time_shutin_imputed, time_producing_imputed))

well_data=left_join(wells, past_12_prod, by=c("API10"="API"))

###################
## Filter out already plugged wells
#remove brand new wells, lets think of this as a snapshot of what the policy would look like on 06/23

well_data = well_data%>%
  filter(is.na(AbandonDate)|AbandonDate>'2023-12-31',
         FirstProdDate<=as.Date("2023-12-31")|is.na(FirstProdDate),
         #welltype%in%c("OW", "GW", "OGW", "OWI", "GWI", "OGI", "GGI", "OWD", "GWD"),
         !wellstatus%in%c("PA", "LA", "APD", "RET", "NEW", "DRL", "OPS"))
##########
# quick search for missing prod data
missingdata=well_data%>%
  filter(is.na(Oil))
#lots of injection wells, a few shutin oil,gas wells


####################################
## Produce flags, figure out bonds##
####################################

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
         fee_state_flag=LeaseType=="FEE"|LeaseType=="STATE",
         shutin12_flag = time_shutin>12,                    #saves whether a well needs to be individually bonded
         depth_1000_flag=depth<=1000,
         depth_1000_3000_flag=depth>1000&depth<=3000,
         depth_3000_6000_flag = depth>3000&depth<=6000,     #these are the new depth thresholds
         depth_6000_9000_flag = depth>6000&depth<=9000,
         depth_9000_12000_flag = depth>9000&depth<=12000,
         depth_12000_flag = depth>12000,
         shutin12_flag = replace(shutin12_flag, is.na(shutin12_flag)&is.na(n_reports), 1),
         shutin12_flag = replace(shutin12_flag, is.na(shutin12_flag), 0),  #bunch of missing shutin12 flags -- looks like these are mostly injection wells but I'll fill all the missings in with 0 to be conservative
         marginal_flag=(BOEperday<=2&welltype%in%c("OW", "GW", "OGW")),
         inactive_flag = (wellstatus%in%c("S", "TA", "I")&time_shutin>0&welltype%in%c("OW", "GW", "OGW"))|(wellstatus%in%c("S", "TA", "I")&!welltype%in%c("OW", "GW", "OGW")|(wellstatus%in%c("S", "TA", "I")&shutin12_flag==1&welltype%in%c("OW", "GW", "OGW"))),
         inactive_marginal_flag=pmax(marginal_flag, inactive_flag),
         UPA_inactive = pmax(shutin12_flag&welltype%in%c("OW", "GW", "OGW")&wellstatus==("P"), inactive_flag&welltype%in%c("WI", "GI", "WD", "GS"))) 

View(well_data%>%group_by(inactive_flag, marginal_flag, welltype, shutin12_flag, UPA_inactive)%>%summarise(n=n()))

############################
#### CALCULATE OPERATOR TIER
############################
#need total production, percentage inactive wells
#also save avg depth, total inactive fee/state wells for use in bond calculation later

#Save total wells, average depth, total production (for use in calculating tier)
operator_stats = well_data%>%
  group_by(Operator)%>%
  summarise(tot_operator_wells=n(),
            avg_depth=mean(depth,na.rm=T),
            tot_BOE=sum(BOEtot))

operator_marginal_depth = well_data%>%
  group_by(Operator)%>%
  filter(fee_state_flag==1,
         inactive_marginal_flag==1)%>%
  summarise(avg_marginal_depth = mean(depth, na.rm=T))

#save number of fee/state wells (used to calculate blanket bond amount for tiered firms)
operator_feestate = well_data%>%
  filter(fee_state_flag==1)%>%
  group_by(Operator)%>%
  summarise(tot_feestate_wells = n())

operator_stats = left_join(operator_stats, operator_feestate, by="Operator")
operator_stats = left_join(operator_stats, operator_marginal_depth, by="Operator")

#figure out percentage inactive for each operator (used to calculate tier)
inactive_operator_dat=well_data%>%
  group_by(Operator, inactive_marginal_flag)%>%
  summarise(tot_inactive=n())%>%
  group_by(Operator)%>%
  mutate(tot_wells = sum(tot_inactive),
         pct_inactive=tot_inactive/tot_wells)%>%
  filter(inactive_marginal_flag==1)%>%
  select(Operator, tot_inactive, pct_inactive)

operator_stats = left_join(operator_stats, inactive_operator_dat, by="Operator")


### Calculate tier
operator_stats = operator_stats%>%
  mutate(tot_inactive = replace(tot_inactive, is.na(tot_inactive), 0),
         pct_inactive = replace(pct_inactive, is.na(pct_inactive), 0),
         BOEperday=tot_BOE/365,
         tier1 = BOEperday>=1000&pct_inactive<=0.15,
         tier2 = BOEperday>=500&pct_inactive<=0.2,
         tier3 = (BOEperday>=200&pct_inactive<=0.25)|(BOEperday>=1000),
         tier=4-tier1-tier2-tier3,
         UPA_tier1 = BOEperday>=1000&pct_inactive<=.25,
         UPA_tier2 = BOEperday>=500&pct_inactive<=0.25,
         UPA_tier3 = (BOEperday>=200&pct_inactive<=0.25)|(BOEperday>=1000),
         UPAtier = 4-UPA_tier1-UPA_tier2-UPA_tier3)

######################
## CALCULATE BONDS ##
######################

# group by operator save wells of each depth by whether fee/state
operator_dat = well_data%>%
  group_by(Operator, fee_state_flag)%>%
  summarise(tot_wells=n(),
            depth_1000_wells = sum(depth_1000_flag,na.rm=T),
            depth_1000_3000_wells = sum(depth_1000_3000_flag,na.rm=T),
            depth_3000_6000_wells = sum(depth_3000_6000_flag,na.rm=T),
            depth_6000_9000_wells = sum(depth_6000_9000_flag, na.rm=T),
            depth_9000_12000_wells = sum(depth_9000_12000_flag,na.rm=T),
            depth_12000_wells = sum(depth_12000_flag,na.rm=T))


#save inactive wells on fee/state land
inactive_operator_dat=well_data%>%
  group_by(Operator, inactive_marginal_flag, fee_state_flag)%>%
  summarise(tot_inactive_feestate=n())%>%
  filter(inactive_marginal_flag==1,
         fee_state_flag==1)%>%
  ungroup()%>%
  select(Operator, tot_inactive_feestate)

UPAinactive_operator_dat=well_data%>%
  group_by(Operator, UPA_inactive, fee_state_flag)%>%
  summarise(tot_UPAinactive_feestate=n())%>%
  filter(UPA_inactive==1,
         fee_state_flag==1)%>%
  ungroup()%>%
  select(Operator, tot_UPAinactive_feestate)

operator_stats=left_join(operator_stats, inactive_operator_dat, by="Operator")
operator_stats=left_join(operator_stats, UPAinactive_operator_dat, by="Operator")

#some operators don't have feestate inactive wells
operator_stats = operator_stats%>%
  mutate(tot_inactive_feestate=replace(tot_inactive_feestate, is.na(tot_inactive_feestate),0),
         tot_feestate_wells = replace(tot_feestate_wells, is.na(tot_feestate_wells),0),
         tot_UPAinactive_feestate=replace(tot_UPAinactive_feestate, is.na(tot_UPAinactive_feestate),0))


## Calculate new blanket bonds
operator_stats = operator_stats%>%
  mutate(tier1blanket = sapply(tot_feestate_wells, tier1_blanket),
         tier1marginal = sapply(avg_marginal_depth,tier1_marginalbond)*tot_inactive_feestate,
         tier2blanket = sapply(tot_feestate_wells, tier2_blanket),
         tier2marginal = sapply(avg_marginal_depth, tier2_marginalbond)*tot_inactive_feestate,
         tier3blanket = sapply(tot_feestate_wells, tier3_blanket),
         tier3marginal = sapply(avg_marginal_depth,tier3_marginalbond)*tot_inactive_feestate)



## Calculate UPA bonds
operator_stats = operator_stats%>%
  mutate(tier1blanket_UPA = sapply(tot_feestate_wells, tier1_blanket_UPA),
         tier1marginal_UPA = sapply(avg_marginal_depth,tier1_marginalbond)*tot_UPAinactive_feestate,
         tier2blanket_UPA = sapply(tot_feestate_wells, tier2_blanket_UPA),
         tier2marginal_UPA = sapply(avg_marginal_depth, tier2_marginalbond)*tot_UPAinactive_feestate,
         tier3blanket_UPA = sapply(tot_feestate_wells, tier3_blanket_UPA),
         tier3marginal_UPA = sapply(avg_marginal_depth,tier3_marginalbond)*tot_UPAinactive_feestate)


## Calculate per-well bonds
#per-well bond numbers come from April DOGM draft
operator_individualbonds = operator_dat%>%
  filter(fee_state_flag==1)%>%
  mutate(depth_1000_bond = depth_1000_wells*10000,
         depth_1000_3000_bond = depth_1000_3000_wells*20000,
         depth_3000_6000_bond = depth_3000_6000_wells*40000,
         depth_6000_9000_bond = depth_6000_9000_wells*65000,
         depth_9000_12000_bond = depth_9000_12000_wells*85000,
         depth_12000_bond = depth_12000_wells*110000,
         individual_well_bond = depth_1000_bond+depth_1000_3000_bond+depth_3000_6000_bond+depth_6000_9000_bond+depth_9000_12000_bond+depth_12000_bond)

operator_stats = left_join(operator_stats, operator_individualbonds, by="Operator")

#operator total bond will be sum of blanket bond, marginal/inactive fee/state bond, and individual well bonds for shutin wells shutin for more than 12 months
#for operators not meeting tiers, its simply the sum of individual well bonds
operator_stats = operator_stats%>%
  mutate(tier1marginal = replace(tier1marginal, is.na(tier1marginal),0),
         tier2marginal = replace(tier2marginal, is.na(tier2marginal),0),
         tier3marginal = replace(tier3marginal, is.na(tier3marginal),0),
         tier1marginal_UPA = replace(tier1marginal_UPA, is.na(tier1marginal_UPA),0),
         tier2marginal_UPA = replace(tier2marginal_UPA, is.na(tier2marginal_UPA),0),
         tier3marginal_UPA = replace(tier3marginal_UPA, is.na(tier3marginal_UPA),0),
         individual_well_bond=replace(individual_well_bond, is.na(individual_well_bond), 0),
         bond = (tier1blanket+tier1marginal)*(tier==1)+(tier2blanket+tier2marginal)*(tier==2)+(tier3blanket+tier3marginal)*(tier==3)+individual_well_bond*(tier==4),
         UPAbond = (tier1blanket_UPA+tier1marginal_UPA)*(UPAtier==1)+(tier2blanket_UPA+tier2marginal_UPA)*(UPAtier==2)+(tier3blanket_UPA+tier3marginal_UPA)*(UPAtier==3)+individual_well_bond*(UPAtier==4),
         bond_difference=bond-UPAbond)


#get rid of operators without fee/state wells
operator_stats=operator_stats%>%
  filter(tot_feestate_wells>0)

##########################################
#####  Calculate Plugging Liabilities ####
##########################################

#liability1 is from the spreadsheet on average decomissioning costs by state
#liability2 is the median decomissioning cost in Raimi
#liability3 assumes $6 per foot of depth
#liability4 assumes $12 per foot of depth
well_data = well_data%>%
  mutate(liability1 = 37500,
         liability2 = 75000,
         liability3 = depth*6,
         liability4 = depth*12)

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
         UPAtier=as.character(UPAtier),
         UPAtier=replace(UPAtier, UPAtier=="4", "No tier"))

####################
## PLOTS ###########
#####################
## Just consider marginal and inactive wells?
##
operator_liabilities_marginalinactive = well_data%>%
  filter(inactive_marginal_flag==1)%>%
  group_by(Operator)%>%
  summarise(liability1_marginal=sum(liability1),
            liability2_marginal=sum(liability2),
            liability3_marginal=sum(liability3),
            liability4_marginal=sum(liability4))

operator_stats = left_join(operator_stats, operator_liabilities_marginalinactive, by="Operator")


########################
## Difference between UPA and DOGM


ggplot(data=operator_stats)+
  geom_histogram(aes(x=bond_difference), alpha=0.9)+
  scale_x_continuous(label=dollar)+
  xlab("DOGM Bond - UPA Bond")+
  ggtitle("Difference between DOGM and UPA Bonding Amounts")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/DOGM_UPA_Difference.jpg",
       device="jpg",
       height=5,
       width=7)

## Histogram of tiers
histdatformat = pivot_longer(operator_stats, cols=c("tier", "UPAtier"))%>%
  mutate(name=replace(name, name=="tier", "DOGM tier"),name=replace(name, name=="UPAtier", "UPA tier"))%>%
  rename(proposal=name)
ggplot(data=histdatformat)+
  geom_bar(aes(x=value, fill=proposal), position="dodge")+
  ylab("Number of Firms")+
  xlab("Tier")+
  ggtitle("Histogram of Firm Tiers")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/Histogram_Tiers_UPAsuggestion.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=bond, y=liability4_feestate_marginal, col="Proposed Rule"))+
  geom_point(aes(x=UPAbond, y=liability4_feestate_marginal, col="UPA Suggestion"))+
  geom_segment(data=operator_stats%>%filter(bond!=UPAbond, bond<10000000),aes(x=bond, y=liability4_feestate_marginal, xend=UPAbond, yend=liability4_feestate_marginal), arrow=arrow(type="closed",length=unit(0.13,"cm"), angle=25))+
  geom_abline(slope=1, intercept=0)+
  ylab("Total Plugging Liabilities for Marginal/Inactive Fee/State Wells")+
  xlab("Required Bonds")+
  scale_x_continuous(label=dollar)+
  scale_y_continuous(label=dollar)+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/UPA_Suggestion_Bond_Changes.jpg",
       device="jpg",
       height=5,
       width=7)

print(paste("DOGM proposal collects: ", sum(operator_stats$bond)))
print(paste("UPA proposal collects: ", sum(operator_stats$UPAbond)))


ggplot(data=operator_stats%>%filter(bond<10000000))+
  geom_point(aes(x=UPAbond, y=liability4_feestate_marginal, color=tier))+
  geom_abline(slope=1, intercept=0)+
  #ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Fee/State Marginal/Inactive Wells")+
  scale_x_continuous(label=dollar)+
  xlab("UPA Proposal - Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs $12 per foot to plug.")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/FeeStateMarginalLiability4_UPA.jpg",
       device="jpg",
       height=5,
       width=7)

############################################################################
## How many wells operated by small firms where liabilities exceed bonds? ##
############################################################################

small_risky_operators = operator_stats%>%
  filter(tot_BOE<1000000)

print(paste("Small operators hold ", sum(small_risky_operators$tot_operator_wells), "wells and", sum(small_risky_operators$tot_operator_wells)/sum(operator_stats$tot_operator_wells), "percent of the state's total wells"))
print(paste("Small operators hold", sum(small_risky_operators$tot_inactive,na.rm=T), "inactive wells and", sum(small_risky_operators$tot_inactive,na.rm=T)/sum(operator_stats$tot_inactive, na.rm=T), "percent of the state's inactive wells"))

print(paste("Inactive wells held by these firms could cost ", sum(small_risky_operators$liability1), "dollars to plug"))
print(paste("Inactive wells held by these firms could cost ", sum(small_risky_operators$liability2), "dollars to plug"))
print(paste("Inactive wells held by these firms could cost ", sum(small_risky_operators$liability3), "dollars to plug"))
print(paste("Inactive wells held by these firms could cost ", sum(small_risky_operators$liability4), "dollars to plug"))

small_risky1 = small_risky_operators%>%
  filter(bond<liability1_marginal)
print(paste("There are ", nrow(small_risky1), "firms which produce less than 1000000 BOE/yr whose plugging liabilities for marginal/inactive wells exceed bond amounts if plug costs are low (37500 per well)."))


small_risky2 = small_risky_operators%>%
  filter(bond<liability2_marginal)
print(paste("There are ", nrow(small_risky2), "firms which produce less than 1000000 BOE/yr whose plugging liabilities for marginal/inactive wells exceed bond amounts if plug costs are high (75000 per well)."))
print(paste("For these firms marginal/inactive plugging liabilities exceed collected bonds by", sum(small_risky2$liability2_marginal)-sum(small_risky2$bond), "if plugging costs are high (75000)"))
write.csv(operator_stats%>%select(Operator, tot_operator_wells, avg_depth, avg_marginal_depth, tot_feestate_wells, tot_inactive, BOEperday, tier,UPAtier, bond,UPAbond, tot_inactive_feestate, starts_with("liability")), "UtahDNRAnalytics/Operator_dat.csv")

