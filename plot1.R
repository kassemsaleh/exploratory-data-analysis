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


plot1 <- function(data=NULL) {
  if(is.null(data))
    data <- load_data_file()
  
  png("plot1.png", width=400, height=400)
  
  hist(data$Global_active_power,  main="Global Active Power", xlab="Global Active Power (in Kilo Watts)",
       ylab="Frequency", col="red")
  
  dev.off()
}