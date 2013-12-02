data <- read.csv("speedskateolympics5.csv")

#converting time to seconds, so that we can compare values
#get minutes
data$minutes <- sapply(strsplit(as.character(data$TimeFormat), split=":"), function(x) { x[1] })
#get seconds
data$seconds <- sapply(strsplit(as.character(data$TimeFormat), split=":"), function(x) { x[2] })
#convert strings to numeric
data$minutes <- as.numeric(data$minutes)
data$seconds <- as.numeric(data$seconds)
#muliply minutes by 60
data$minutes <- data$minutes*60
#add seconds and minutes to get total seconds
data$totalseconds <- data$minutes+data$seconds


#making some subsets and playing w/ data (not the 500m data has to be adjusted because a few years ago, they started running two races and taking the sum. I've take then average, so we can compare these two data sets.)
men500m <- subset(data, Event=="500m Men")
men500m$Adjusted <- ifelse(men500m$totalseconds>50, men500m$totalseconds/2, men500m$totalseconds)

plotcolors <- ifelse(men500m$Medal == "GOLD", "orange", "lightgray")
plot(men500m$Year, men500m$Adjusted, col=plotcolors)

women500m <- subset(data, Event=="500m Women")
women500m$Adjusted <- ifelse(women500m$totalseconds>50, women500m$totalseconds/2, women500m$totalseconds)

plotcolorswomen <- ifelse(women500m$Medal == "GOLD", "orange", "darkgray")
plot(women500m$Year, women500m$Adjusted, col=plotcolorswomen)

men1000m <- subset(data, Event=="1000m Men")
plotcolors1000m <- ifelse(men1000m$Medal=="GOLD", "orange", "lightgray")
plot(men1000m$Year, men1000m$totalseconds, col=plotcolors1000m)

women1000m <- subset(data, Event=="1000m Women")
plotcolors1000mWomen <- ifelse(women1000m$Medal=="GOLD", "orange", "darkgray")
plot(women1000m$Year, women1000m$totalseconds, col=plotcolors1000mWomen)



#now to compare men and women 500m (note, I already made the subsets for these groups up above and adjusted for changes in the rules (two races instead of one))
plotcomparemen <- ifelse(men500m$Medal=="GOLD", "orange", "black")
plotcomparewomen <- ifelse(women500m$Medal=="GOLD", "orange", "lightgray")
plot(men500m$Year, men500m$Adjusted, col=plotcomparemen, ylim=c(30,50), xlim=c(1924,2010))
par(new=TRUE)
plot(women500m$Year, women500m$Adjusted, col=plotcomparewomen, ylim=c(30,50), xlim=c(1924,2010))



#now to compare men and women 1000m
men1000m <- subset(data, Event=="1000m Men")
women1000m <- subset(data, Event=="1000m Women")

plotcomparemen <- ifelse(men1500m$Medal=="GOLD", "orange", "black")
plotcomparewomen <- ifelse(women1500m$Medal=="GOLD", "orange", "lightgray")
plot(men1500m$Year, men1500m$totalseconds, col=plotcomparemen, ylim=c(100,200), xlim=c(1924,2010))
par(new=TRUE)
plot(women1500m$Year, women1500m$totalseconds, col=plotcomparewomen, ylim=c(100,200), xlim=c(1924,2010))



#now to compare men and women 1500m
men1500m <- subset(data, Event=="1500m Men")
women1500m <- subset(data, Event=="1500m Women")

plotcomparemen <- ifelse(men1500m$Medal=="GOLD", "orange", "black")
plotcomparewomen <- ifelse(women1500m$Medal=="GOLD", "orange", "lightgray")
plot(men1500m$Year, men1500m$totalseconds, col=plotcomparemen, ylim=c(100,200), xlim=c(1924,2010))
par(new=TRUE)
plot(women1500m$Year, women1500m$totalseconds, col=plotcomparewomen, ylim=c(100,200), xlim=c(1924,2010))


#now to compare men and women 5000m
men5000m <- subset(data, Event=="5000m Men")
women5000m <- subset(data, Event=="5000m Women")

plotcomparemen <- ifelse(men5000m$Medal=="GOLD", "orange", "black")
plotcomparewomen <- ifelse(women5000m$Medal=="GOLD", "orange", "lightgray")
plot(men5000m$Year, men5000m$totalseconds, col=plotcomparemen, ylim=c(300,600), xlim=c(1924,2010))
par(new=TRUE)
plot(women5000m$Year, women5000m$totalseconds, col=plotcomparewomen, ylim=c(300,600), xlim=c(1924,2010))


#now to plot 10000m men
men10000m <- subset(data, Event=="10000m Men")
plotcolor <- ifelse(men10000m$Medal=="GOLD", "orange", "black")
plot(men10000m$Year, men10000m$totalseconds, col=plotcolor)


#now to plot 3000m women
women3000m <- subset(data, Event=="3000m Women")
plotcolor <- ifelse(women3000m$Medal=="GOLD", "orange", "lightgray")
plot(women3000m$Year, women3000m$totalseconds, col=plotcolor)


#now to compare short track 500m
ShortWomen500 <- subset(data, Event=="500m Short Track Women")
ShortMen500 <- subset(data, Event=="500m Short Track Men")

plotcomparemen <- ifelse(ShortMen500$Medal=="GOLD", "orange", "black")
plotcomparewomen <- ifelse(ShortWomen500$Medal=="GOLD", "orange", "lightgray")

plot(ShortMen500$Year, ShortMen500$totalseconds, col=plotcomparemen, ylim=c(40,50), xlim=c(1992,2010))
par(new=TRUE)
plot(ShortWomen500$Year, ShortWomen500$totalseconds, col=plotcomparewomen, ylim=c(40,50), xlim=c(1992,2010))


#and short track 1000m
ShortWomen1000 <- subset(data, Event=="1000m Short Track Women")
ShortMen1000 <- subset(data, Event=="1000m Short Track Men")

plotcomparemen <- ifelse(ShortMen1000$Medal=="GOLD", "orange", "black")
plotcomparewomen <- ifelse(ShortWomen1000$Medal=="GOLD", "orange", "lightgray")

plot(ShortMen1000$Year, ShortMen1000$totalseconds, col=plotcomparemen, ylim=c(80,105), xlim=c(1992,2010))
par(new=TRUE)
plot(ShortWomen1000$Year, ShortWomen1000$totalseconds, col=plotcomparewomen, ylim=c(80,105), xlim=c(1992,2010))


#and finally short track relay 5000m (men only)
ShortRelay5000 <- subset(data, Event=="5000m Short Track Relay Men")
plotcolor <- ifelse(ShortRelay5000$Medal=="GOLD", "orange", "black")
plot(ShortRelay5000$Year, ShortRelay5000$totalseconds, col=plotcolor)


#and short track relay 3000m (women only)
ShortRelay3000 <- subset(data, Event=="3000m Short Track Relay Women")
plotcolor <- ifelse(ShortRelay3000$Medal=="GOLD", "orange", "black")
plot(ShortRelay3000$Year, ShortRelay3000$totalseconds, col=plotcolor)


#now to plot the altitude
plot(altitude$Year, altitude$Altitude, col="green", type="l", ylim=c(-3000, 2000))
