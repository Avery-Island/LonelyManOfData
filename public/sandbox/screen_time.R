library(tidyverse)
library(RSQLite)
library(rlang)
library(dbplyr)
library(hrbrthemes)
kdb <- src_sqlite("~/Library/Application Support/Knowledge/knowledgeC.db")

tbl(
  kdb, 
  sql('
      SELECT
      ZOBJECT.ZVALUESTRING AS "app", 
      (ZOBJECT.ZENDDATE - ZOBJECT.ZSTARTDATE) AS "usage",  
      CASE ZOBJECT.ZSTARTDAYOFWEEK 
      WHEN "1" THEN "Sunday"
      WHEN "2" THEN "Monday"
      WHEN "3" THEN "Tuesday"
      WHEN "4" THEN "Wednesday"
      WHEN "5" THEN "Thursday"
      WHEN "6" THEN "Friday"
      WHEN "7" THEN "Saturday"
      END "dow",
      ZOBJECT.ZSECONDSFROMGMT/3600 AS "tz",
      DATETIME(ZOBJECT.ZSTARTDATE + 978307200, \'UNIXEPOCH\') as "start_time", 
      DATETIME(ZOBJECT.ZENDDATE + 978307200, \'UNIXEPOCH\') as "end_time",
      DATETIME(ZOBJECT.ZCREATIONDATE + 978307200, \'UNIXEPOCH\') as "created_at", 
      CASE ZMODEL
      WHEN ZMODEL THEN ZMODEL
      ELSE "Other"
      END "source"
      FROM
      ZOBJECT 
      LEFT JOIN
      ZSTRUCTUREDMETADATA 
      ON ZOBJECT.ZSTRUCTUREDMETADATA = ZSTRUCTUREDMETADATA.Z_PK 
      LEFT JOIN
      ZSOURCE 
      ON ZOBJECT.ZSOURCE = ZSOURCE.Z_PK 
      LEFT JOIN
      ZSYNCPEER
      ON ZSOURCE.ZDEVICEID = ZSYNCPEER.ZDEVICEID
      WHERE
      ZSTREAMNAME = "/app/usage"'
      )) -> usage

# The same but dbplyr style

tbl(kdb, "ZOBJECT") %>% 
  mutate(
    created_at = datetime(ZCREATIONDATE + 978307200, "UNIXEPOCH", "LOCALTIME"),
    start_dow = case_when(
      ZSTARTDAYOFWEEK == 1 ~ "Sunday",
      ZSTARTDAYOFWEEK == 2 ~ "Monday",
      ZSTARTDAYOFWEEK == 3 ~ "Tuesday",
      ZSTARTDAYOFWEEK == 4 ~ "Wednesday",
      ZSTARTDAYOFWEEK == 5 ~ "Thursday",
      ZSTARTDAYOFWEEK == 6 ~ "Friday",
      ZSTARTDAYOFWEEK == 7 ~ "Saturday"
    ),
    start_time = datetime(ZSTARTDATE + 978307200, "UNIXEPOCH", "LOCALTIME"),
    end_time = datetime(ZENDDATE + 978307200, "UNIXEPOCH", "LOCALTIME"),
    usage = (ZENDDATE - ZSTARTDATE),
    tz = ZSECONDSFROMGMT/3600 
  ) %>% 
  left_join(tbl(kdb, "ZSTRUCTUREDMETADATA"), c("ZSTRUCTUREDMETADATA" = "Z_PK")) %>% 
  left_join(tbl(kdb, "ZSOURCE"), c("ZSOURCE" = "Z_PK")) %>% 
  left_join(tbl(kdb, "ZSYNCPEER"), "ZDEVICEID") %>% 
  filter(ZSTREAMNAME == "/app/usage")  %>% 
  select(
    app = ZVALUESTRING, created_at, start_dow, start_time, end_time, usage, tz, source = ZMODEL
  ) %>% 
  mutate(source = ifelse(is.na(source), "Other", source)) %>% 
  collect() %>% 
  mutate_at(vars(created_at, start_time, end_time), as.POSIXct) -> usage

# Extracting app time-----
list.files(
  c("/Applications", "/System/Library/CoreServices", "/Applications/Utilities", "/System/Applications"), # main places apps are stored (there are potentially more but this is sufficient for our needs)
  pattern = "\\.app$", 
  full.names = TRUE
) -> apps

x <- sys::exec_internal("mdls", c("-name", "kMDItemCFBundleIdentifier", "-r", apps))

# mdls null (\0) terminates each entry so we have to do some raw surgery to get it into a format we can use
x$stdout[x$stdout == as.raw(0)] <- as.raw(0x0a)

tibble(
  name = gsub("\\.app$", "", basename(apps)),
  app = read_lines(x$stdout) 
) -> app_trans

app_trans

usage %>% 
  group_by(app) %>% 
  summarise(first = min(start_time), last = max(end_time), total = sum(usage, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(total = total / 60 / 60) %>% # hours
  arrange(desc(total)) %>% 
  left_join(app_trans) -> overall_usage

overall_usage %>% 
  slice(1:10) %>% 
  left_join(app_trans) %>%
  mutate(name = fct_inorder(name) %>% fct_rev()) %>%
  ggplot(aes(x=total, y=name)) + 
  geom_segment(aes(xend=0, yend=name), size = 6, alpha = 0.6, color = "#107C10") +
  scale_x_comma(position = "top") +
  labs(
    x = "Total Usage (hrs)", y = NULL,
    title = glue::glue('App usage in the past {round(as.numeric(max(usage$end_time) - min(usage$start_time), "days"))} days')
  ) +
  theme_minimal()
