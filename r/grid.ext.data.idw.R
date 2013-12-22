# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       May 30, 2012
# Last modified: Jun 01, 2012
# Purpose:       Take smoothed spatial data and calculate gridded
# extinction rate.
# ====================================================================


# plan: 
# take the richness data
# merge it with the extinction rate data by MatchTaxon
# take a weighted average
# return it

# [u]nique [e]xtinction [rates]
uerates <- data.finite[!duplicated(data.finite[,c("MatchTaxon")]), c("MatchTaxon", "class", "ext")]

srdat.with.rates <- merge(srdat, uerates, all = TRUE)

srdat.ext <- ddply(srdat.with.rates, c("PID", "SID"), summarize, Z = sum(richness * ext)/sum(richness))

