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
2. I calculate for each operator, how many 'at-risk' Fee/State wells each Operator has, along with the average depth of these wells.
3. I calculate the blanket and marginal/depth bond for each firm that meets tier requirements based on the number of fee/state wells and fee/state 'at-risk' wells they hold and their average well depth.
4. For firms that don't meet tier requirements, I simply calculate the sum of individual bonds for fee/state wells.
5. Finally, for each firm, I sum across their required bonds.  So for a tier 3 firm their bond is tier 3 blanket bond + at-risk bond.