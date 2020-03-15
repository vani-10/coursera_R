# coursera_R
assignments on R
setwd("/Users/karthik/desktop/pollutant_mean/specdata")
data <-read.csv("001.csv")
#Write a function that reads a directory full of files and reports the number of completely observed cases in each data file.
The function should return a data frame where the first column is the name of the file and the second column is the number of 
complete cases. A prototype of this function follows

complete.cases(data)#it takes only the complete reading of a row 
sum(complete.cases(data))
list.files(path = "/Users/karthik/desktop/pollutant_mean/specdata",pattern = "")#pattern here takes any kinf of file format unless otherwise speified exclusively 
filelist <-list.files(path = "/Users/karthik/desktop/pollutant_mean/specdata",pattern = "")
length(filelist)
filelist <-list.files(path = "/Users/karthik/desktop/pollutant_mean/specdata",pattern = ".csv", full.names = TRUE)
filelist

complete <-function(directory,id =1:332)
{
  filelist <-list.files(path ="/Users/karthik/desktop/pollutant_mean/specdata",pattern=".csv",full.names = TRUE)
  nobs <-numeric()
  for (i in id )
  {
    data<- read.csv(filelist[i])
    nobs<-c(nobs,sum(complete.cases(data)))
  }
  data.frame(id,nobs)
}
complete("/Users/karthik/desktop/pollutant_mean/specdata",1)
complete("/Users/karthik/desktop/pollutant_mean/specdata",c(2,4,8,10,12))

#Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list
of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor 
ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' 
argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. 
A prototype of the function is as follows

pollutantmean <-function(directory,pollutant, id =1:332)
{
  filelist <-list.files(path ="/Users/karthik/desktop/pollutant_mean/specdata",pattern=".csv",full.names = TRUE)
  values<-numeric()
  for(i in id)
  {
    data<-read.csv(filelist[i])
    values<-c(values,data[[pollutant]])
  }
  mean(values,na.rm = TRUE)
    
}

