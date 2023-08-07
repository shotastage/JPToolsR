is_Weekday <- function() {
  print("Hello, world!")
  if (!is_workspace_exists("jp_governmental_holiday_db.rda")) {
    jp_governmental_holiday_db <- read.csv("http://www8.cao.go.jp/chosei/shukujitsu/syukujitsu.csv",
                                           header = TRUE,
                                           fileEncoding = "CP932",
                                           stringsAsFactors = FALSE)
    save(jp_governmental_holiday_db, file = in_script_workspace("jp_governmental_holiday_db.rda"))
  }

}
