##################################################
# A Shiny app for creating custom IPEDS peer     #
# reports.                                       #
##################################################

setwd("./data")

ay1213_df <- odbcConnectAccess2007("IPEDS201213.accdb")
ay1314_df <- odbcConnectAccess2007("IPEDS201314.accdb")
ay1415_df <- odbcConnectAccess2007("IPEDS201415.accdb")
ay1516_df <- odbcConnectAccess2007("IPEDS201516.accdb")
ay1617_df <- odbcConnectAccess2007("IPEDS201617.accdb")