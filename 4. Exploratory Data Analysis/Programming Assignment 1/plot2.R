plot(strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S"),as.double(data$Global_active_power),type = 'l',
     xlab = '',ylab = "Global Active Power")
dev.copy(png,"plot2.png")
dev.off()