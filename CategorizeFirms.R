rm(list=ls())

library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

###################################
## Code to analyze Utah DNR data ##
###################################

setwd("C:/Users/lbeatty/Documents/")

source("UtahDNRAnalytics/bond_funs.R")

prod_data = read.csv("Production2020To2024.csv")      ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Production2020To2024.zip
wells = read.csv("Wells.csv")                         ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/Wells.zip
wellhistory = read.csv("WellHistory.csv")             ## Downloaded from https://oilgas.ogm.utah.gov/pub/Database/WellHistory.zip

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


#look from 2022-06-01 to 2023-05-01 for data availability
past_12_prod = prod_data%>%
  filter(ReportPeriod>=as.Date("2022-06-01"),
         ReportPeriod<as.Date("2023-06-01"))%>%
  group_by(API)%>%
  summarise(Oil=sum(Oil),
            Gas=sum(Gas),
            Water=sum(Water),
            n_reports=n())

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
         inactive_flag = wellstatus%in%c("S", "TA", "I"),
         inactive_marginal_flag=pmax(marginal_flag, inactive_flag),
         fee_state_flag=LeaseType=="FEE"|LeaseType=="STATE",
         depth1000_flag=depth<1000,
         depth3000_flag=depth>=1000&depth<3000,
         depth10000_flag=depth>=3000&depth<10000,
         depthmax_flag=depth>=10000)

well_data%>%group_by(inactive_marginal_flag)%>%summarise(n=n())

# group by operator, get well counts
operator_dat = well_data%>%
  group_by(Operator)%>%
  summarise(avg_depth=mean(depth, na.rm=T),
            tot_BOE=sum(BOEtot),
            tot_wells=n(),
            depth1000_wells = sum(depth1000_flag,na.rm=T),
            depth3000_wells = sum(depth3000_flag,na.rm=T),
            depth10000_wells = sum(depth10000_flag,na.rm=T),
            depthmax_wells = sum(depthmax_flag,na.rm=T))

inactive_operator_dat=well_data%>%
  group_by(Operator, inactive_marginal_flag)%>%
  summarise(tot_inactive=n())%>%
  filter(inactive_marginal_flag==1)%>%
  select(Operator, tot_inactive)


#calculate operator tier
operator_dat=left_join(operator_dat, inactive_operator_dat, by="Operator")
operator_dat = operator_dat%>%
  mutate(tot_inactive = replace(tot_inactive, is.na(tot_inactive), 0),
         pct_inactive = tot_inactive/tot_wells,
         BOEperday=tot_BOE/365,
         tier1 = BOEperday>=1000&pct_inactive<=0.15,
         tier2 = BOEperday>=500&pct_inactive<=0.2,
         tier3 = BOEperday>=200&pct_inactive<=0.25,
         tier=4-tier1-tier2-tier3)

#calculate number of fee/state inactive wells by operator
inactive_operator_dat=well_data%>%
  group_by(Operator, inactive_marginal_flag, fee_state_flag)%>%
  summarise(tot_inactive_feestate=n())%>%
  filter(inactive_marginal_flag==1,
         fee_state_flag==1)%>%
  ungroup()%>%
  select(Operator, tot_inactive_feestate)

operator_dat=left_join(operator_dat, inactive_operator_dat, by="Operator")
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


## Calculate old bonds
operator_dat = operator_dat%>%
  mutate(depth1000_bond = depth1000_wells*1500,
         depth3000_bond = depth3000_wells*15000,
         depth10000_bond = depth10000_wells*30000,
         depthmax_bond = depthmax_wells*60000,
         depth1000_flag = depth1000_wells>0,
         depthgreater1000_flag = tot_wells-depth1000_wells>0)

operator_dat = operator_dat%>%
  mutate(old_blanket1000 = 15000*depth1000_flag,
         old_blanketgreater1000 = 120000*depthgreater1000_flag,
         old_bond = pmin(old_blanket1000, depth1000_bond)+ pmin(old_blanketgreater1000, depth3000_bond+depth10000_bond+depthmax_bond))

operator_dat = operator_dat%>%
  mutate(bond = (tier1blanket+tier1marginal)*(tier==1)+(tier2blanket+tier2marginal)*(tier==2)+(tier3blanket+tier3marginal)*(tier==3)+(depth1000_bond+depth3000_bond+depth10000_bond+depthmax_bond)*(tier==4),
         bond_delta = bond-old_bond)



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

operator_dat = left_join(operator_dat, operator_liabilities, by="Operator")
operator_dat=operator_dat%>%
  mutate(bondliability1 = bond-liability1,
         bondliability2 = bond-liability2,
         bondliability3 = bond-liability3,
         bondliability4 = bond-liability4)


ggplot(data=operator_dat%>%filter(bond<1000000))+
  geom_point(aes(x=bond, y=liability1))+
  geom_abline(slope=1, intercept=0)+
  ggtitle("Liabilities exceed bond amounts for most firms")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 37500 to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/BondsLiabilities1.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_dat%>%filter(bond<1000000))+
  geom_point(aes(x=bond, y=liability2))+
  geom_abline(slope=1, intercept=0)+
  ggtitle("Liabilities exceed bond amounts for most firms")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 75000 to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/BondsLiabilities2.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_dat%>%filter(bond_delta<10000000))+
  geom_point(aes(x=tot_BOE, y=bond_delta))+
  ggtitle("Bonds increase the most for high-production firms")+
  scale_y_continuous(labels = dollar)+
  ylab("Difference Between New and Old Bond Amounts")+
  xlab("Total Yearly Production (BOE)")+
  theme_bw()
#bond deltas are weakly positive, bonds mostly increase for large firms

ggsave(filename="UtahDNRAnalytics/Figures/BondDeltaProduction.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_dat)+
  geom_point(aes(x=tot_BOE, y=bondliability1))+
  ggtitle("Liabilities exceed bond amounts large firms")+
  scale_y_continuous(labels = dollar)+
  ylab("Difference between bonds and plugging liabilities")+
  labs(caption="Calculated assuming each well costs 37500 to plug.")+
  xlab("Total BOE")+
  theme_bw()
#difference between bonds and liabilities is decreasing in total production

ggsave(filename="UtahDNRAnalytics/Figures/NetLiability1Production.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_dat%>%filter(tot_BOE<200000000))+
  geom_point(aes(x=tot_BOE, y=bondliability1))+
  ggtitle("Liabilities exceed bond amounts large firms (zoomed in)")+
  scale_y_continuous(labels = dollar)+
  ylab("Difference between bonds and plugging liabilities")+
  xlab("Total BOE")+
  labs(caption="Calculated assuming each well costs 37500 to plug.")+
  theme_bw()
#difference between bonds and liabilities is decreasing in total production

ggsave(filename="UtahDNRAnalytics/Figures/NetLiability1Production_zoomed.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_dat%>%filter(tot_BOE<10000000))+
  geom_point(aes(x=tot_BOE, y=bondliability1))+
  ggtitle("Liabilities exceed bond amounts large firms (more zoomed in)")+
  scale_y_continuous(labels = dollar)+
  ylab("Difference between bonds and plugging liabilities")+
  xlab("Total BOE")+
  labs(caption="Calculated assuming each well costs 37500 to plug.  For very small firms, many still have liabilities that exceed bonds.")+
  theme_bw()
#difference between bonds and liabilities is decreasing in total production
ggsave(filename="UtahDNRAnalytics/Figures/NetLiability1Production_smallfirms.jpg",
       device="jpg",
       height=5,
       width=7)


#####################
## What if we just consider marginal and inactive wells?
##
operator_liabilities_marginalinactive = well_data%>%
  filter(inactive_marginal_flag==1)%>%
  group_by(Operator)%>%
  summarise(liability1_marginal=sum(liability1),
            liability2_marginal=sum(liability2),
            liability3_marginal=sum(liability3),
            liability4_marginal=sum(liability4))

operator_dat = left_join(operator_dat, operator_liabilities_marginalinactive, by="Operator")
operator_dat=operator_dat%>%
  mutate(bondliability1_marginal = bond-liability1_marginal,
         bondliability2_marginal = bond-liability2_marginal,
         bondliability3_marginal = bond-liability3_marginal,
         bondliability4_marginal = bond-liability4_marginal)



ggplot(data=operator_dat%>%filter(bond<1000000))+
  geom_point(aes(x=bond, y=liability1_marginal))+
  geom_abline(slope=1, intercept=0)+
  ggtitle("New bonds cover marginal and inactive well plugging liability")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Marginal/Inactive Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 37500 to plug.")+
  theme_bw()

ggsave(filename="UtahDNRAnalytics/Figures/InactiveMarginalLiabilities1.jpg",
       device="jpg",
       height=5,
       width=7)

ggplot(data=operator_dat%>%filter(bond<1000000))+
  geom_point(aes(x=bond, y=liability2))+
  geom_abline(slope=1, intercept=0)+
  ggtitle("New bonds don't cover marginal and inactive well plugging liability \n if plugging costs are high")+
  scale_y_continuous(labels = dollar)+
  ylab("Total Plugging Liabilities for Marginal/Inactive Wells")+
  scale_x_continuous(label=dollar)+
  xlab("Required Bonds")+
  labs(caption="Plot of firm-level total estimated plugging liabilities against required bonds. \n A line is plotted at y=x. Plugging costs assume each well costs 75000 to plug.")+
  theme_bw()
ggsave(filename="UtahDNRAnalytics/Figures/InactiveMarginalLiabilities2.jpg",
       device="jpg",
       height=5,
       width=7)


############################################################################
## How many wells operated by small firms where liabilities exceed bonds? ##
############################################################################
small_risky_operators = operator_dat%>%
  filter(tot_BOE<5000000,
         bondliability1<0)

print(paste("Small operators where plugging liabilities exceed bonds hold ", sum(small_risky_operators$tot_wells), "wells and", sum(small_risky_operators$tot_wells)/sum(operator_dat$tot_wells), "percent of the state's total wells"))
print(paste("Small operators where plugging liabilities exceed bonds hold ", sum(small_risky_operators$tot_inactive), "inactive wells and", sum(small_risky_operators$tot_inactive)/sum(operator_dat$tot_inactive), "percent of the state's inactive wells"))

print(paste("Inactive wells held by these firms could cost ", sum(small_risky_operators$liability1), "dollars to plug"))
print(paste("Inactive wells held by these firms could cost ", sum(small_risky_operators$liability2), "dollars to plug"))
print(paste("Inactive wells held by these firms could cost ", sum(small_risky_operators$liability3), "dollars to plug"))
print(paste("Inactive wells held by these firms could cost ", sum(small_risky_operators$liability4), "dollars to plug"))

write.csv(operator_dat%>%select(Operator, avg_depth, tot_BOE, tot_wells, tot_inactive, tier, old_bond, bond, liability1, liability2, liability1_marginal, liability2_marginal), "UtahDNRAnalytics/Operator_dat.csv")
