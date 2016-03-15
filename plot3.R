
date_time <- function(date, time) {
  return (strptime(paste(date, time), "%d/%m/%Y %H:%M:%S"))
}


load_data_file <- function() {
  filename <- "household_power_consumption.txt"
  
  datafile <- read.table(filename, header=TRUE, sep=";", colClasses=c("character", "character", rep("numeric",7)),
                         na="?")
  # convert date and time variables to Date/Time class
  datafile$Date <- as.Date(datafile$Date, "%d/%m/%Y")
  datafile$Time <- strptime(paste(datafile$Date, datafile$Time), "%d/%m/%Y %H:%M:%S")
  
  
  # only use data from the dates 2007-02-01 and 2007-02-02
  dates <- as.Date(c("2007-02-01", "2007-02-02"), "%Y-%m-%d")
  datafile <- subset(datafile, Date %in% dates)
  
  return(datafile)
  
}

plot3 <- function(data=NULL) {
  if(is.null(data))
    data <- load_data()
  
  png("plot3.png", width=400, height=400)
  
  plot(data$Time, data$Sub_metering_1, type="l", col="black",
       xlab="", ylab="Energy sub metering")
  
  lines(data$Time, data$Sub_metering_2, col="red")
  
  lines(data$Time, data$Sub_metering_3, col="blue")
  
  legend("topright", col=c("black", "red", "blue"),
         c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         lty=1)
  
  dev.off()
}