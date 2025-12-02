library(data.table)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(scales)
library(kableExtra)
library(knitr)

if(Sys.getenv("COMPUTERNAME")=="LOLOPUTER"){
  setwd('C:/Users/laure/Documents/MethaneSAT_Review/')
}

source('Code/edf_theme.R')
source('C:/Users/laure/Documents/state_api_codes.R')

wellbores = fread("Data/enverus_wellbores.csv")

wellbores[, state_code := substr(API_UWI, 1, 2)]

# map using the named vector
wellbores[, state := state_codes[state_code]]

wellbores[,fifty_yrs:=year(FirstCompletionDate)<=1975]

age_summary = wellbores[,.(
  older_fifty_yrs=sum(fifty_yrs,na.rm=T),
  younger_fifty_yrs = sum(fifty_yrs==F, na.rm=T)), by=.(state)]
age_summary[,pct_older_fifty:=older_fifty_yrs/(older_fifty_yrs+younger_fifty_yrs)]

age_summary[state%in%c('Colorado', 'New Mexico', 'Texas', 'Utah')] |>
  kable(
    caption = "Well Ages by State",
    digits = 2,
    col.names = c(
      "State",
      "Older than 50 yrs",
      "Younger than 50 yrs",
      "Pct older (>50)"
    )
  ) |>
  kable_styling(full_width = FALSE)

#make a timeseries
well_counts <- wellbores[!is.na(FirstCompletionDate),
                     .(n_wells = .N),
                     by = .(state, year(FirstCompletionDate))
]
well_counts[order(year), cum_wells := cumsum(n_wells), by = state]

#year from when cross threshold
threshold=5000
well_counts[,geq_threshold:=cum_wells>threshold]
well_counts[geq_threshold==T,min_threshold_yr:=min(year), by=state]
well_counts[,yrs_since_threshold:=year-min_threshold_yr]

ggplot(well_counts[state %in% c("Utah", "Colorado", "New Mexico")], aes(x = yrs_since_threshold, y = cum_wells, color = state)) +
  geom_line(lwd = 1.5) +
  theme_bw() +
  labs(
    x = "Years Since Passing 5,000 Wells",
    y = "Cumulative Wells",
    color = "State"
  ) +
  scale_y_continuous(labels = comma) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


ggsave(
  filename = file.path("UtahDNRAnalytics/September25Redline/Figs/Threshold_Timeseries.png"),
  width = 6,
  height = 4,
  dpi = 300
)

## break out by basins
ut_wells = wellbores[state=="Utah"]
ut_wells[,Last12_BOE:=Last12MonthGasProduction_MCF/6+Last12MonthOilProduction_BBL]
ut_wells[ENVPlay=="", ENVPlay:="Other"]
ggplot(ut_wells, aes(x = ENVPlay, y = Last12_BOE)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw() +
  labs(
    x = "Basin",
    y = "Last 12 Months BOE",
    title = "Distribution of Last12_BOE by Basin"
  )

quant_table <- ut_wells[, .(
  gas_p10  = quantile(Last12MonthGasProduction_MCF, 0.10, na.rm = TRUE),
  gas_p25  = quantile(Last12MonthGasProduction_MCF, 0.25, na.rm = TRUE),
  gas_p50  = quantile(Last12MonthGasProduction_MCF, 0.50, na.rm = TRUE),
  gas_p75  = quantile(Last12MonthGasProduction_MCF, 0.75, na.rm = TRUE),
  gas_p90  = quantile(Last12MonthGasProduction_MCF, 0.90, na.rm = TRUE),

  oil_p10  = quantile(Last12MonthOilProduction_BBL, 0.10, na.rm = TRUE),
  oil_p25  = quantile(Last12MonthOilProduction_BBL, 0.25, na.rm = TRUE),
  oil_p50  = quantile(Last12MonthOilProduction_BBL, 0.50, na.rm = TRUE),
  oil_p75  = quantile(Last12MonthOilProduction_BBL, 0.75, na.rm = TRUE),
  oil_p90  = quantile(Last12MonthOilProduction_BBL, 0.90, na.rm = TRUE)
), by = ENVPlay]

kable(
  quant_table,
  caption = "Quantiles of Gas and Oil Production by Basin (ENVPlay)",
  digits = 0,
  col.names = c(
    "Basin",
    "Gas P10", "Gas P25", "Gas P50", "Gas P75", "Gas P90",
    "Oil P10", "Oil P25", "Oil P50", "Oil P75", "Oil P90"
  )
)|>
  kable_styling(full_width = FALSE)
