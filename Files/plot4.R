######################################################################
## plot4.R 
## ----------------------- 
## Description:
## This is the script for extracting the 
## "Individual household electric power consumption Data Set" 
## and plots a graphic with the Energy sub metering and time. 
## 
##
######################################################################
plot4 <- function (rds_path = "") {
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
  comdata1$Sub_metering_1 <- as.integer(as.character(comdata1$Sub_metering_1))
  comdata1$Sub_metering_2 <- as.integer(as.character(comdata1$Sub_metering_2))
  comdata1$Sub_metering_3 <- as.integer(as.character(comdata1$Sub_metering_3))
  comdata2 <<- comdata1
  #####################################################################
  ##5.Creates de png with the histogram
  #####################################################################
  
  png("plot4.png", width = 480, height = 480)
  par(mfrow = c(2,2), mar= c(4,4,2,1), oma= c(0,0,2,0))
  with (comdata1,{
    plot(Time,Global_active_power, type='l',ylab = "Global Active Power",xlab= "",cex.axis=0.8, cex.lab=0.8)
    plot(Time,Voltage, type='l',ylab = "Voltage",xlab= "datetime",cex.axis=0.8, cex.lab=0.8)
    plot(Time,Sub_metering_1,type ="l",ylim = c(0, 40),yaxp = c(0, 30, 3), ylab = "Energy sub metering", xlab ="",cex.axis=0.8, cex.lab=0.8)
    lines(Time,Sub_metering_2,col ="RED")
    lines(Time,Sub_metering_3,col ="BLUE")
    legend("topright",col = c("black","red","blue"), legend = c("Sub_metering_1 ","Sub_metering_2 ","Sub_metering_3 "),lty=c(1,1,1),cex=0.7,bty = "n")
    plot(Time,Global_reactive_power, type='l',xlab= "datetime",cex.axis=0.8, cex.lab=0.8)
  })
  dev.off()
  message("Graphic saved as plot4.png in the current working directory")
}