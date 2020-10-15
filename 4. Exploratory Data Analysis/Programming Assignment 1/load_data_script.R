## Download and load the data from remote source
download.file('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip',
              "power_consumption.zip")
unzip("power_consumption.zip")
data.table::fread("household_power_consumption.txt") -> data
data <- data[data$Date == '1/2/2007' | data$Date == '2/2/2007']
data <- data[complete.cases(data)]