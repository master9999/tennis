rm(list=ls())

require(RODBC)



mdbConnect<-odbcConnectAccess("OnCourt")





######### loading data on tournaments ################
court_data <- sqlQuery(mdbConnect, "SELECT ID_T, ID_C_T 
FROM tours_atp 
ORDER BY ID_T DESC ", as.is=TRUE)


write.csv(court_data,"ATP\\court_data.csv")



courts <- read.csv("court_data.csv")




