plot(strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S"),as.double(data$Sub_metering_1),type = 'l',
     ylab = "Energy sub metering",xlab = '')
lines(strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S"),as.double(data$Sub_metering_2),type = 'l',
      ylab = "Energy sub metering",col = 'red')
lines(strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S"),as.double(data$Sub_metering_3),type = 'l',
      ylab = "Energy sub metering",col = 'blue')
legend('topright',lty = 1,col = c('black','red','blue'),legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
dev.copy(png,"plot3.png")
dev.off()