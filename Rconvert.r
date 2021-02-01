library("RODBC") #load package
library(foreign)
library(readxl)

for (j in 63:98) {

  if (j==89|j==90) {
    db <- file.path(paste0("F:/Data/HBS/",j,"/Data-",j,"/Data",j,".accdb") )
  } else {
    db <- file.path(paste0("F:/Data/HBS/",j,"/Data-",j,"/Data",j) )
  }

channel<-odbcConnectAccess2007(db) #internal RODBC function

names <-subset(sqlTables(channel),TABLE_TYPE == "TABLE", TABLE_NAME)[[1]]

for (i in names) {
  print(i)
  nam <- paste(i)
  assign(nam, sqlFetch(channel,i))
}

rm("channel","i", "nam", "names","db")

save.image(file=paste0("F:/Data/HBS/r/raw",j,".RData"))

print(j)

rm(list=ls())
}


## merging summary files

for (j in 63:75) {
  
  assign(paste0("Sum_R",j), read.dbf(paste0("F:/Data/HBS/Summeries/63-75/R",j,"_SUM.dbf")))
  assign(paste0("Sum_U",j), read.dbf(paste0("F:/Data/HBS/Summeries/63-75/U",j,"_SUM.dbf")))
  
  load(paste0("F:/DATA/HBS/r/raw",j,".rdata")) 
  
  save.image(file=paste0("F:/Data/HBS/r/",j,".RData"))
  
  print(j)
  
  rm(list=ls())
}

for (j in rbind(76:87,93,95)) {
  
  assign(paste0("Sum_R",j), read_xls(paste0("F:/Data/HBS/Summeries/sum_",j,"/W_SUM_R",j,".xls")))
  assign(paste0("Sum_U",j), read_xls(paste0("F:/Data/HBS/Summeries/sum_",j,"/W_SUM_U",j,".xls")))
  
  load(paste0("F:/DATA/HBS/r/raw",j,".rdata")) 
  
  save.image(file=paste0("F:/Data/HBS/r/",j,".RData"))
  
  print(j)
  
  rm(list=ls())
}

for (j in rbind(88:92,94)) {
  
  assign(paste0("Sum_R",j), read_xlsx(paste0("F:/Data/HBS/Summeries/sum_",j,"/W_SUM_R",j,".xlsx")))
  assign(paste0("Sum_U",j), read_xlsx(paste0("F:/Data/HBS/Summeries/sum_",j,"/W_SUM_U",j,".xlsx")))
  
  load(paste0("F:/DATA/HBS/r/raw",j,".rdata")) 
  
  save.image(file=paste0("F:/Data/HBS/r/",j,".RData"))
  
  print(j)
  
  rm(list=ls())
}

