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


######################################
## GENERAL OUTLINE OF PROCEDURE ######
## (updated Oct 2025 ) ###############
# 1. Read, clean, format data from UT DNR - wells and production data
# 2. Compile production data for the year from 01/2024 through 12/2024
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

setwd("C:/Users/laure/Documents/Utah")
data_dir = 'Data/'
code_dir = 'UtahDNRAnalytics/'

redline = fread(file.path(code_dir, 'September25Redline/operator_stats.csv'))
current = fread(file.path(code_dir, 'CurrentReg', 'current_bonds.csv'))

firms = merge(redline, current, by='Operator')

firms[,liability:=75000*tot_feestate]
firms[,atrisk_liability:=75000*tot_feestate_atrisk]
#################33##################
## plot liability versus bonds #####

firms[,current_pct_covered:=current_bond/liability]
firms[,current_atrisk_pct_covered:=current_bond/atrisk_liability]

ggplot(firms)+
  geom_histogram(aes(x=current_pct_covered), bins=10)+
  xlab("Current Bonds: \nPercentage of Liability Covered")+
  ylab("Number of firms")+
  theme_bw()

total = firms[,.(
  sum_liability = sum(liability,na.rm=T),
  sum_atrisk_liability = sum(atrisk_liability,na.rm=T),
  current_bond = sum(current_bond, na.rm=T),
  redline_bond = sum(bond,na.rm=T)
)]

total <- melt(total, measure.vars = c("sum_liability", "sum_atrisk_liability", "current_bond", "redline_bond"))

total[, variable := factor(
  variable,
  levels = c("sum_liability", "sum_atrisk_liability", "current_bond", "redline_bond"), # custom order
  labels = c("Fee/State Liabilities","Fee/State At-Risk Liabilities", "Current Bond", "Redline Bond")      # custom labels
)]

ggplot(total, aes(x = variable, y = value, fill = variable)) +
  geom_col(color = "black") +

  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +

  scale_fill_manual(values = c(
    "Fee/State Liabilities" = "#d95f02",
    "Fee/State At-Risk Liabilities" = "#d95f02",
    "Current Bond" = "#1b9e77",
    "Redline Bond" = "#1b9e77"
  )) +

  labs(x = NULL, y = "USD", title = "Liabilities and Bonds") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave(file.path(code_dir, "September25Redline", "Figs", "Total_Bar.png"),
       width=8,
       height=5)

firms[,pct20_threshold:=pct_atrisk<0.2]
ggplot(firms)+
  geom_point(aes(x=current_bond, y=bond, color=pct20_threshold))+
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  labs(color = "Less than 20% At-Risk",
       x = "Current Bond",
       y = "Proposed Bond")+
  theme_minimal()
ggsave(file.path(code_dir, "September25Redline", "Figs", "Current_Proposed_Risk_Scatter.png"),
       width=8,
       height=5)

#mean by pct20_threshold
mean_dif = firms[tot_feestate>10,]
mean_dif[,bond_dif:=bond-current_bond]
mean_dif = mean_dif[,.(
  bond_dif=mean(bond_dif,na.rm=T)
), by=pct20_threshold]

ggplot(mean_dif, aes(x = pct20_threshold, y = bond_dif, fill = pct20_threshold)) +
  geom_col(color = "black") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_discrete(labels = c(
    "TRUE" = "Less than 20% At-Risk",
    "FALSE" = "Greater than 20% At-Risk"
  )) +
  labs(x = NULL, y = "USD", title = "Mean Increase in Bonding Amounts Between\nCurrent and Proposed Rules") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 22)
  )
ggsave(file.path(code_dir, "September25Redline", "Figs", "Current_Proposed_Mean_Difference.png"),
       width=8,
       height=5)


ggplot(firms)+
  geom_point(aes(x=current_bond, y=bond, color=as.factor(tier)))+
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  labs(color = "Firm Tier",
       x = "Current Bond",
       y = "Proposed Bond")+
  theme_minimal()
ggsave(file.path(code_dir, "September25Redline", "Figs", "Current_Proposed_Tier_Scatter.png"),
       width=8,
       height=5)


############
## Liability
##########3#
ymax <- max(firms$liability, na.rm = TRUE) * 1.2

# Fewer y grid lines: only 1, 3, and 10 per decade (instead of 1–9)
y_exponents <- floor(log10(5e3)):ceiling(log10(ymax))
y_major <- 10^y_exponents
y_minor <- unlist(lapply(y_exponents, function(e) c(2, 4, 6,8) * 10^e))
y_minor <- y_minor[y_minor >= 5e3 & y_minor <= ymax]

ggplot(firms) +
  geom_point(aes(x = current_bond, y = liability)) +
  geom_abline(intercept = 0, slope = 1, color = "black") +

  # X-axis: add 50,000 label explicitly
  scale_x_log10(
    labels = label_dollar(scale_cut = cut_short_scale()),
    breaks = sort(unique(c(10^(2:8), 5e4))),   # ensure 50k is labeled
    minor_breaks = unlist(lapply(2:8, function(e) (2:9) * 10^e))
  ) +

  # Y-axis: fewer grid lines, starting at 5e3
  scale_y_log10(
    labels = label_dollar(scale_cut = cut_short_scale()),
    breaks = y_major,
    minor_breaks = y_minor
  ) +

  coord_cartesian(
    xlim = c(1e4, max(firms$current_bond, na.rm = TRUE) * 1.2),
    ylim = c(5e3, ymax)
  ) +

  labs(
    x = "Current Bond (log scale)",
    y = "Fee/State Liabilities (log scale)",
    caption = "Black line drawn at y = x"
  ) +

  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.3)
  )


ggsave(file.path(code_dir, "September25Redline", "Figs", "Current_Liability_Scatter.png"),
       width=8,
       height=5)

ymax <- max(firms$liability, na.rm = TRUE) * 1.2
y_exponents <- floor(log10(5e3)):ceiling(log10(ymax))
y_major <- 10^y_exponents
y_minor <- unlist(lapply(y_exponents, function(e) c(2, 4, 6,8) * 10^e))
y_minor <- y_minor[y_minor >= 5e3 & y_minor <= ymax]

ggplot(firms) +
  geom_point(aes(x = bond, y = liability, color=as.factor(tier))) +
  geom_abline(intercept = 0, slope = 1, color = "black") +

  # X-axis: add 50,000 label explicitly
  scale_x_log10(
    labels = label_dollar(scale_cut = cut_short_scale()),
    breaks = sort(unique(c(10^(2:8)))),
    minor_breaks = unlist(lapply(2:8, function(e) (seq(1,9,by=1)) * 10^e))
  ) +

  # Y-axis: fewer grid lines, starting at 5e3
  scale_y_log10(
    labels = label_dollar(scale_cut = cut_short_scale()),
    breaks = y_major,
    minor_breaks = y_minor
  ) +

  coord_cartesian(
    xlim = c(1e4, max(firms$bond, na.rm = TRUE) * 1.2),
    ylim = c(5e3, ymax)
  ) +

  labs(
    x = "Proposed Bond (log scale)",
    y = "Fee/State Liabilities (log scale)",
    caption = "Black line drawn at y = x",
    color="Tier"
  ) +

  theme_minimal(base_size = 18) +
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.3)
  )

ggsave(file.path(code_dir, "September25Redline", "Figs", "Proposed_Liability_Scatter.png"),
       width=8,
       height=5)
