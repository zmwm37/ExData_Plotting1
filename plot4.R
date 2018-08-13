## MODULE 4 - WEEK 1 ASSIGNMENT
# Plot 4
raw <- read.table(".\\Coursera\\Exploratory Analysis\\household_power_consumption.txt", header = T, sep = ";")
head(raw)
str(raw)

#clean up data

#change question marks to blanks
no_qm<-function(x){
        gsub("\\?","",x)
}

df<-data.frame(sapply(raw, no_qm))

#format date as date

df$Date_clean <- as.Date(df$Date,format = "%d/%m/%Y")
df$dt <- paste(df$Date,df$Time)
df$Date_Time <- strptime(df$dt, format = "%d/%m/%Y %H:%M:%S")

#change class
df$Global_active_power<- as.numeric(df$Global_active_power)
df$Sub_metering_1<- as.numeric((df$Sub_metering_1))
df$Sub_metering_2<- as.numeric((df$Sub_metering_2))
df$Sub_metering_3<- as.numeric((df$Sub_metering_3))
df$Global_reactive_power<-as.numeric(df$Global_reactive_power)
df$Voltage<-as.numeric(df$Voltage)

#subset date
df_sub <- subset(df, Date_clean >= "2007-02-01" & Date_clean <= "2007-02-02")

#plot 4

png(file = "./Coursera/ExData_Plotting1/plot4.png")
par(mfrow = c(2,2))


plot(
        df_sub$Global_active_power/500 ~ as.POSIXct(df_sub$Date_Time),
        type = "l",
        ylab = "Global Active Power (kilowatts)",
        xlab = ""
)

plot(
        df_sub$Voltage ~ as.POSIXct(df_sub$Date_Time),
        type = "l",
        ylab = "Voltage",
        xlab = "datetime"
)

plot(df_sub$Sub_metering_1-2~as.POSIXct(df_sub$Date_Time), type = "l",
     ylab = "Energy sub metering",
     xlab = "",
     ylim = c(0,40))
lines((df_sub$Sub_metering_2-2)/5~as.POSIXct(df_sub$Date_Time), type = "l",col = "red")
lines(df_sub$Sub_metering_3-1~as.POSIXct(df_sub$Date_Time), type = "l",col = "blue")
legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col = c("black","red","blue"), lty = c(1,1,1))

plot(
        df_sub$Global_reactive_power/400 ~ as.POSIXct(df_sub$Date_Time),
        type = "l",
        ylab = "Global_reactive_power",
        xlab = "datetime"
)

dev.off()
