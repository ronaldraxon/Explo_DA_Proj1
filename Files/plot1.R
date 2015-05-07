######################################################################
## plot1.R 
## ----------------------- 
## Description:
## This is the script for extracting the 
## "Individual household electric power consumption Data Set" 
## and plots a histogram with the Global Active Power and showing its 
## frequency.
##
######################################################################
plot1 <- function (rds_path = "") {
  message("Executing!")
#####################################################
##1.Checks if the rds_path contains file path 
#####################################################
  if(rds_path == "")
  {
    message("File path was not specified or is not valid. This program will automatically download the file")
    temp <- tempfile()
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
    message("Loading data into R. Please Wait...")
    dataset <- read.table(unz(temp, "household_power_consumption.txt"), sep = ";", header = TRUE)
    unlink(temp)
  }
  else
  { 
    if(!file.exists(rds_path))
    {
      message("The file path was not specified or is not valid.")
      message("Process Finished.")
      return(FALSE)
    }
    message("Loading data into R. Please Wait...")
    dataset <- read.table(unz(rds_path, "household_power_consumption.txt"), sep = ";",  header = TRUE)
  } 
#####################################################################
##2.Converts variables 2 and 1 to time and date  formats respectively
#####################################################################
dataset$Time <- strptime(paste(dataset$Date, dataset$Time), "%d/%m/%Y %H:%M:%S")
dataset$Date <- as.Date(dataset$Date, "%d/%m/%Y")
comdata<- dataset
#####################################################################
##3.Filters observations with dates between
#####################################################################
comdata1 <- comdata[comdata$Time >= "2007-02-01 00:00:00" & comdata$Time < "2007-02-03 00:00:00" ,]
#####################################################################
##4.Converts variables 3-9 to numeric variables
#####################################################################     
comdata1$Global_active_power <- as.double(as.character(comdata1$Global_active_power))
comdata1$Global_reactive_power <- as.double(as.character(comdata1$Global_reactive_power))
comdata1$Voltage <- as.double(as.character(comdata1$Voltage))
comdata1$Global_intensity <- as.double(as.character(comdata1$Global_intensity))
comdata1$Sub_metering_1 <- as.double(as.character(comdata1$Sub_metering_1))
comdata1$Sub_metering_2 <- as.double(as.character(comdata1$Sub_metering_2))
comdata1$Sub_metering_3 <- as.double(as.character(comdata1$Sub_metering_3))

#####################################################################
##5.Creates de png with the histogram
#####################################################################
png("plot1.png", width = 480, height = 480)
hist(comdata1$Global_active_power, col = "RED", main= "Global Active Power" , freq = TRUE, xlab = "Global Active Power (kilowatts)",cex.axis=0.8, cex.lab=0.8)
dev.off()
message("Hist saved as plot1.png in the current working directory")
}
