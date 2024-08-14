# UtahDNRAnalytics
Lauren Beatty
lbeatty@edf.org

This code was created to analyze whether the proposed Utah DOGM regulation sufficiently covers the potential plugging of oil and gas wells.
To do this, it calculates the bonding amounts by firm and the potential plugging costs of each firm under a number of different cost scenarios.

The main file is CategorizeFirms.R, which contains all of the script to categorize firms, calculate bonds, and produce the charts from the presentation.
It calls a helper file called bond_funs.R which simply holds functions which spit out the blanket and marginal bond amounts based on the number of wells and depth.

To reproduce the charts, simply clone this repository onto your machine and download and unzip Production2020To2024.csv, WellHistory.csv, and Wells.csv from [https://oilgas.ogm.utah.gov/pub/Database/Production2020To2024.zip]
(https://oilgas.ogm.utah.gov/oilgasweb/data-center/dc-main.xhtml#download)
into the same folder as the repo.

## Bond Calculation
Bonds are calculated as follows:
1. First I figure out what tier each firm is.  This involves summing their production for a year and determining their proportion of statewide inactive wells.
2. I separate out fee/state wells that have been inactive fore more than 12 months as these need to be bonded individually regardless of tier status.  I calculate the bonds for these wells and sum within firms.
3. I calculate the blanket and marginal/depth bond for each firm that meets tier requirements based on the number of fee/state wells and fee/state inactive wells they hold and their average well depth.
4. For firms that don't meet tier requirements, I simply calculate the sum of individual bonds for fee/state wells.
5. Finally, for each firm, I sum across their required bonds.  So for a tier 3 firm their bond is tier 3 blanket bond + tier 3 marginal/inactive bond + individual well bonds for fee/state wells inactive >12 months.

###############################################################################

Update - Aug 2024 for latest draft UT bond proposal schedule
Christine Gerbode 
cgerbode@edf.org

Original code from Lauren pulled from GitHub end of July 2024
Key updates: 
1. New bonding schedule is based on classifications of "Active Wells" and "At-Risk Wells". 
"Marginal/Inactive" classifications and related stats have been updated to "at risk" throughout, where appropriate. 
Depth bins, well count bins, and dollar amounts associated with each have been updated. 

Active and at-risk are defined uses the following criteria:
##---------------------------------------------------------------------------##
## CHECKLIST for needed flags under 2024 schedule proposal: 
##--New definitions under Aug 2024 proposal bonding tier defintions/ schedule:
##---"ACTIVE WELLS" include...:
##-----Producing wells greater than 1 BOE per day on average, annual basis
##------- find with BOE/day calculation (ID'd below as marginal_wells)
##-----Drilling wells
##-------- well status = DRL
##-----Active enhanced oil recover (EOR) wells
##-------- well type = OWI, GWI, OGI, GGI, OWD, GWD; status = P, PAI, PII
##-----Approved permits
##-------- well type = APD
##-----Active water wells
##-------- interpreted as water source wells, well type = WS, status = A
##-----Active WIW
##-------- interpreted as Water Injection Wells, well type = WI, status = A
##-------- OR could be well type = OWI, GWI and status = SAI
##-----Active WDW
##-------- interpreted as Water Disposal Wells, well type = WD, status= A
##-------- OR could be well type = OWD, GWD and status = SAI
##---------------------------------------------------------------------------##
##---"AT RISK WELLS" include...:
##-----Producing wells with less than 1 BOE per day (annual basis)
##-------- find with BOE/day calculation (flagged as marginal_wells)
##-----Shut-in (S) wells
##-------- find with shut-in time>0 and/or status = S, SII, PII
##-----Temporarily Abandoned (TA) wells
##-------- well status = TA
##-----Operation Suspended (OPS) wells
##-------- well status = OPS
##-----Inactive EOR wells - greater than 12 months
##-------- interpreted as well type = OWI, GWI, OGI, GGI; shut-in time > 12
##-----Inactive WIW - greater than 12 months
##-------- interpreted as type = WI, status= I or SII 
##-------- alternative to consider: calc. water production inactive for last >12 months
##-----Inactive WDW -- greater than 12 months
##-------- interpreted as type = WD, status = I or SII
######OTHER FLAGS created:
### inactive,
### inactive for >12 months, 
### on fee/state land, 
### max depth of well borehole (across all records for the same API10)
##---------------------------------------------------------------------------##

2. Added stats on the % of projected liabilities covered by the proposed bond, for each row in Operator_stats. 
Summary stats (table of averages for each cost assumption and operator tier are exported to a separate CVS (/bond_sufficiency_stats.csv). 

3. I've maintained the original date restrictions of 1-12/2023, to make these results comparable to the previous schedule. Well data is now available through 7/2023 however.

