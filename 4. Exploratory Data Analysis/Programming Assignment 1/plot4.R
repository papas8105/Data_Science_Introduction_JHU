par(mfrow = c(2,2))
plot(strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S"),as.double(data$Global_active_power),type = 'l',
     xlab = '',ylab = "Global Active Power")
plot(strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S"),as.double(data$Voltage),type = 'l',
     xlab = '',ylab = "Voltage")
plot(strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S"),as.double(data$Sub_metering_1),type = 'l',
     ylab = "Energy sub metering",xlab = '')
lines(strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S"),as.double(data$Sub_metering_2),type = 'l',
      ylab = "Energy sub metering",col = 'red')
lines(strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S"),as.double(data$Sub_metering_3),type = 'l',
      ylab = "Energy sub metering",col = 'blue')
legend('topright',lty = 1,col = c('black','red','blue'),legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
plot(strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S"),as.double(data$Global_reactive_power),type = 'l',
     ylab = "Global_reactive_power",xlab = '')
par(mfrow = c(1,1))
dev.copy(png,"plot4.png")
dev.off()
rm(data)
unlink("power_consumption.zip")
unlink("household_power_consumption.txt")