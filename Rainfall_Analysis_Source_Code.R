#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Reading in Data as data frame, converting into Time series.
#---------------------------------------------------------------------------------------------------------------------------------------------------------

# Loading library "readxl" for reading in .xlsx file via R.
# Loading library "openxlsx" for writing in .xlsx file via R.
library(readxl)
library(openxlsx)

# Data from file is stored in the data frame named as 'Intermediate'.
# "read_excel" function of "readxl" library is used.
Intermediate<-read_excel("Rainfall_Data_(111_years).xlsx")

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
  assign(Unique[j],Data)
}

# Deleting "Intermediate" directory.
unlink("Intermediate",recursive=T)

#---------------------------------------------------------------------------------------------------------------------------------------------------------
# Plotting Monthwise and Annual data for each district.
#---------------------------------------------------------------------------------------------------------------------------------------------------------

# Creating a folder for storing all analysis charts for each district seperately.
dir.create("Analysis")
for(k in 1:length(Unique))
{
  # Creating a folder for each district.
  dir.create(paste0("Analysis/",Unique[k]))
}

# Converting each data frame containing district rainfall data into time series and plotting its graph.
# Saving each plot in its respective folder.
for(l in 1:length(Unique))
{
  # Creating dataframes each having a seperate district data with the same name as the respective district.
  Data<-eval(parse(text=paste0("`",Unique[1],"`")))
  # For plotting monthwise.
  Month<-Data
  Month<-(as.vector(t(as.matrix(Month[,-13:-18]))))
  Month<-ts(Month,frequency=12,start=Intermediate$Year[1],
            end=Intermediate$Year[length(Intermediate$Year)])
  
  # Saving the plot on monthwise rainfall in working directory.
  png(file=paste0("Analysis/",Unique[l],"/Monthwise.png"),width=800,height=600)
  plot(Month,main=paste0("Monthwise Data Visualization - ",Unique[l]),
       ylab="Rainfall in mm",xlab="Years")
  dev.off(which=dev.cur())
}
for(m in 1:length(Unique))
{
  # Creating dataframes each having a seperate district data with the same name as the respective district.
  Data<-eval(parse(text=paste0("`",Unique[m],"`")))
  # For plotting Annual Rainfall.
  Annual<-Data
  Annual<-(as.vector(t(as.matrix(Annual[,13]))))
  Annual<-ts(Annual,frequency=1,start=Intermediate$Year[1],
             end=Intermediate$Year[length(Intermediate$Year)])
  
  # Saving the plot on annual rainfall in working directory.
  png(file=paste0("Analysis/",Unique[m],"/Annual.png"),width=800,height=600)
  plot(Annual,main=paste0("Annual Data Visualization - ",Unique[m]),
       ylab="Rainfall in mm",xlab="Years")
  dev.off(which=dev.cur())
}

#---------------------------------------------------------------------------------------------------------------------------------------------------------
# 30 year moving average estimation and deviation of actual rainfall from 30 year moving average for "Adilabad" district using Annual rainfall data.
#---------------------------------------------------------------------------------------------------------------------------------------------------------

library(pracma)
for(n in 1:length(Unique))
{
  # Creating dataframes each having a seperate district data with the same name as the respective district.
  Data<-eval(parse(text=paste0("`",Unique[n],"`")))