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

Programmimg Assignment 3 on Hospital data
setwd("/Users/karthik/desktop/pollutant_mean/rprog_data_ProgAssignment3-data")
data<-read.csv("hospital-data.csv")
head(data)#gives information on the headings inside the file with certain columns
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")#retreiving one of the file
ncol(outcome)#gives number of columns here 46
nrow(outcome)#gives number of rows here 4706
names(outcome)


The first function finds best hospital in state


best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    fd   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                       stringsAsFactors = FALSE)
    colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
                                        
    ## Check that state and outcome are valid
    if(!state %in% fd[, "state"]){
        stop('invalid state')
    } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else {
        si <- which(fd[, "state"] == state)
        ts <- fd[si, ]    # extracting data for the called state
        oi <- as.numeric(ts[, eval(outcome)])
        min_val <- min(oi, na.rm = TRUE)
        result  <- ts[, "hospital"][which(oi == min_val)]
        output  <- result[order(result)]
    }
return(output)
}

# example output:
best("SC", "heart attack")

The second funtion ranks hospitals by outcome in a state

rankhospital <- function(state, outcome, rank = "best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    fd   <- as.data.frame(cbind(data[, 2],  # hospital
                                data[, 7],  # state
                               data[, 11],  # heart attack
                               data[, 17],  # heart failure
                               data[, 23]), # pneumonia
                               stringsAsFactors = FALSE)
    colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    ## Check that state and outcome are valid
    if (!state %in% fd[, "state"]) {
        stop('invalid state')
    } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else if (is.numeric(rank)) {
        si <- which(fd[, "state"] == state)
        ts <- fd[si, ]                     # extracting dataframe for the called state
        ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
        ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"]), ]
        output <- ts[, "hospital"][rank]
    } else if (!is.numeric(rank)){
        if (rank == "best") {
             output <- best(state, outcome)
        } else if (rank == "worst") {
                si <- which(fd[, "state"] == state)
                ts <- fd[si, ]    
                ts[, eval(outcome)] <- as.numeric(ts[, eval(outcome)])
                ts <- ts[order(ts[, eval(outcome)], ts[, "hospital"], decreasing = TRUE), ]
                output <- ts[, "hospital"][1]
        } else {
            stop('invalid rank')
        }
    }
return(output)
}
The third function ranks hospitals in all states.

# example output:
rankhospital("NC", "heart attack", "worst")
rankall <- function(outcome, num = "best"){
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    fd   <- as.data.frame(cbind(data[, 2],  # hospital
                                data[, 7],  # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
    colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    fd[, eval(outcome)] <- as.numeric(fd[, eval(outcome)])
    
    ## Check that state and outcome are valid
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')
    } else if (is.numeric(num)) {
        by_state <- with(fd, split(fd, state))
        ordered  <- list()
        for (i in seq_along(by_state)){
            by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                 by_state[[i]][, "hospital"]), ]
            ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
        }
        result <- do.call(rbind, ordered)
        output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
        names(output) <- c("hospital", "state")
    } else if (!is.numeric(num)) {
        if (num == "best") {
            by_state <- with(fd, split(fd, state))
            ordered  <- list()
            for (i in seq_along(by_state)){
                by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                     by_state[[i]][, "hospital"]), ]
                ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
            }
            result <- do.call(rbind, ordered)
            output <- as.data.frame(result, stringsAsFactors = FALSE)
            rownames(output) <- output[, 2]
        } else if (num == "worst") {
            by_state <- with(fd, split(fd, state))
            ordered  <- list()
            for (i in seq_along(by_state)){
                by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                                     by_state[[i]][, "hospital"], 
                                                     decreasing = TRUE), ]
                ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
            }
            result <- do.call(rbind, ordered)
            output <- as.data.frame(result, stringsAsFactors = FALSE)
            rownames(output) <- output[, 2]
        } else {
            stop('invalid num')
        }
    }
return(output)
}

# example output:
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)
