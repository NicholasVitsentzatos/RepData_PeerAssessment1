df<-read.csv("activity.csv")

Ts<-tapply(df$steps,df$date,sum,na.rm=TRUE)

qplot(Ts,binwidth=1000,xlab = "total number of steps taken each day")

summary(Ts)

As <- aggregate(x = list(steps = df$steps), by = list(interval = df$interval),FUN = mean, na.rm = TRUE)

ggplot(data = As, aes(x = interval, y = steps)) + geom_line() + xlab("5-minute interval") + ylab("average number of steps taken")

As$interval[max(As$steps)==As$steps]

sum(is.na(df$steps))

fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps)) {
    filled <- c(steps)}
  else{
    filled <- (As[As$interval == interval, "steps"])}
  return(filled)}

filled.df <- df
filled.df$steps <- mapply(fill.value, filled.df$steps, filled.df$interval)

Ts.steps <- tapply(filled.df$steps, filled.df$date, FUN = sum)
qplot(Ts.steps, binwidth = 1000, xlab = "total number of steps taken each day")

weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Δευτέρα", "Τρίτη", "Τετάρτη", "Πέμπτη", "Παρασκευή")){
    return("weekday")}
  else if (day %in% c("Σάββατο", "Κυριακή")){ 
    return("weekend")}
}
filled.df$date <- as.Date(filled.df$date)
filled.df$day <- sapply(filled.df$date, FUN=weekday.or.weekend)
averages <- aggregate(steps ~ interval + day, data=filled.df, mean)

ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")
