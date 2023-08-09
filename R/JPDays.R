convert_to_posixt <- function(datetime_text) {
  # Pattern 1: XXXX年X月X日X時X分
  if (grepl("\\d{4}年\\d{1,2}月\\d{1,2}日\\d{1,2}時\\d{1,2}分", datetime_text)) {
    datetime <- as.POSIXct(datetime_text, format = "%Y年%m月%d日%H時%M分")
  }
  # Pattern 2: XXXX/XX/XX/XX:XX
  else if (grepl("\\d{4}/\\d{1,2}/\\d{1,2}/\\d{1,2}:\\d{1,2}", datetime_text)) {
    datetime <- as.POSIXct(datetime_text, format = "%Y/%m/%d/%H:%M")
  }
  # Pattern 3: Only date (ex: XXXX年X月X日 or XXXX/XX/XX)
  else if (grepl("\\d{4}年\\d{1,2}月\\d{1,2}日", datetime_text)) {
    # Add noon
    datetime <- as.POSIXct(paste0(datetime_text, "12時00分"), format = "%Y年%m月%d日%H時%M分")
  }
  else if (grepl("\\d{4}/\\d{1,2}/\\d{1,2}", datetime_text)) {
    # Add noon
    datetime <- as.POSIXct(paste0(datetime_text, "/12:00"), format = "%Y/%m/%d/%H:%M")
  }
  else {
    stop("Unknown datetime pattern.")
  }

  return(datetime)
}


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

  # Check if the date is a holiday or weekday
  is_holiday_or_weekday <- ifelse(date %in% jp_governmental_holiday_db[, 1], FALSE,
                                  ifelse(weekday %in% weekdays_list, TRUE, FALSE))

  return(is_holiday_or_weekday)
}
