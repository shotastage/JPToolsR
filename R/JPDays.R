is_Weekday <- function(timestamp) {
  wd <- tempdir()
  wf <- tempfile()

  if (!file.exists(paste0(wd, "/jp_governmental_holiday_db.rda"))) {
    jp_governmental_holiday_db <- read.csv("http://www8.cao.go.jp/chosei/shukujitsu/syukujitsu.csv",
                                           header = TRUE,
                                           fileEncoding = "CP932",
                                           stringsAsFactors = FALSE)
    save(jp_governmental_holiday_db, file = paste0(wd, "/jp_governmental_holiday_db.rda"))
  }

  load(paste0(wd, "/jp_governmental_holiday_db.rda"))

  # Convert the holiday dates to Date objects
  jp_governmental_holiday_db[, 1] <- as.Date(jp_governmental_holiday_db[, 1], format = "%Y/%m/%d")

  # Convert time stamp to Date object
  date <- as.Date(timestamp, origin = "1970-01-01")  # Set UNIX epoc time srart point as 1970/1/1

  # Get the weekday of the date
  weekday <- weekdays(date)

  # Define weekdays and weekends
  weekdays_list <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

  print(paste0("INFO: Executing Japanese Holidy Checker Job No: ", sample(1:9999, 1)))

  # Check if the date is a holiday or weekday
  is_holiday_or_weekday <- ifelse(date %in% jp_governmental_holiday_db[, 1], FALSE,
                                  ifelse(weekday %in% weekdays_list, TRUE, FALSE))

  return(is_holiday_or_weekday)
}
