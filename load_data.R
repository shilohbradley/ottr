##################################################
# A Shiny app for creating custom IPEDS peer     #
# reports.                                       #
##################################################

setwd("./data")

odbcConnectAccess2007("IPEDS201213.accdb")
odbcConnectAccess2007("IPEDS201314.accdb")
odbcConnectAccess2007("IPEDS201415.accdb")
odbcConnectAccess2007("IPEDS201516.accdb")
odbcConnectAccess2007("IPEDS201617.accdb")