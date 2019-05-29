# Loading library "readxl" for reading in .xlsx file via R.
# Loading library "openxlsx" for writing in .xlsx file via R.
library(readxl)
library(openxlsx)

# Data from file is stored in the data frame named as 'Intermediate'.
# "read_excel" function of "readxl" library is used.
Intermediate<-read_excel("(IMD)_Rainfall_Data_-_100_years.xlsx")

# Removing NA values from 'Intermediate'.
Intermediate<-na.omit(Intermediate)

# Finding names of each district seperately using "unique" function.
Unique<-unique(Intermediate$DIST)

# "which" function is used to find the row number for which certain value is present in the data frame.
# This function can be used to create subset of data for each district.
# In order to view an example for showing the working of "which" function, remove hash from the following snippet.
# Intermediate[which(Intermediate$DIST==Unique[1])[1]:which(Intermediate$DIST==Unique[1])[length(which(Intermediate$DIST==Unique[1]))],]

# Creating a folder for storing all .xlsx files having data for each district seperately.
dir.create("Intermediate")

# Creating loop for writing an excel file with a particular district data.
for(i in 1:length(Unique))
{
  write.xlsx(Intermediate[which(Intermediate$DIST==Unique[i])[1]:which(Intermediate$DIST==Unique[i])[length(which(Intermediate$DIST==Unique[i]))],-1:-3],
  file=paste0("Intermediate/Intermediate_",i,".xlsx"),
  sheetName = Unique[i], col.names = TRUE, row.names = TRUE, append = TRUE)
}

# Reading each file and changing its rownames with years.
# Removing all "WC" data.
# Storing each district's data in a separate data frame having the name of that particular district.
for(j in 1:length(Unique))
{
  Data<-read_excel(paste0("Intermediate/Intermediate_",j,".xlsx"))
  Data<-as.data.frame(Data)
  row.names(Data)<-Intermediate$Year[1:length(row.names(Data))]
  Data<-Data[,-1]
  #Data<-Data[,-13:-18]
  assign(Unique[j],Data)
}

# Converting each data frame containing district rainfall data into a time series and plotting its graph.
for(k in 1:length(Unique))
{
  Data<-eval(parse(text=paste0("`",Unique[k],"`")))
  
  # For plotting monthwise.
  Month<-Data
  Month<-(as.vector(t(as.matrix(Month[,-13:-18]))))
  Month<-ts(Month,frequency=12,start=Intermediate$Year[1],end=Intermediate$Year[length(Intermediate$Year)])
  
  # For plotting periodwise.
  Period<-Data
  Period<-(as.vector(t(as.matrix(Period[,-1:-12]))))
  Period<-ts(Period,frequency=12,start=Intermediate$Year[1],end=Intermediate$Year[length(Intermediate$Year)])
  
  # For plotting Annual Rainfall.
  Annual<-Data
  Annual<-(as.vector(t(as.matrix(Annual[,13]))))
  Annual<-ts(Annual,frequency=12,start=Intermediate$Year[1],end=Intermediate$Year[length(Intermediate$Year)])

  # For plotting Kharif Rainfall.
  Kharif<-Data
  Kharif<-(as.vector(t(as.matrix(Kharif[,14]))))
  Kharif<-ts(Kharif,frequency=12,start=Intermediate$Year[1],end=Intermediate$Year[length(Intermediate$Year)])
  
  # Plotting all plots together for each district.
  plot(Month,main=paste0(Unique[k]," - Monthwise"),ylab="Rainfall in mm")
  plot(Period,main=paste0(Unique[k]," - Periodwise"),ylab="Rainfall in mm")
  plot(Annual,main=paste0(Unique[k]," - Annual"),ylab="Rainfall in mm")
  plot(Kharif,main=paste0(Unique[k]," - Kharif"),ylab="Rainfall in mm")
}
