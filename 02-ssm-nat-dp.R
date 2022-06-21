
library(lubridate)
library(flux)
library(readxl)
library(reshape)
library(tidyverse)
library(gridExtra)
library(ggmap)
library(plotly)
library(GGally)

now <- Sys.time()
now


# dir('data/location-master', full.names=TRUE)
locMaster <- read_csv("data/location-master/location-master-20220619.csv")
locMaster


### Loop to ingest all scraped data
filelist <- dir('data/aptmon', full.names=TRUE)

scrp <- data.frame()
for (file in filelist) {
  filetimestr <- sub(".csv", "",sub(".*-", "", file))
  filetime <- strptime(filetimestr,"%Y%m%d%H%M%S")
  temp <- read.csv(file, na = "---")
  temp$DateTime <- as.POSIXlt(filetime)
  scrp <- rbind(scrp,as.data.frame(temp))
}

### Date and time transformation
attr(scrp$DateTime, "tzone") <- "Asia/Taipei"
scrp$DateTimeRound <- floor_date(scrp$DateTime, "30mins")
scrp$WaitingQueue <- as.numeric(as.character(sub("*人", "", scrp$輪候人數)))
scrp$WaitingMinutes <- as.numeric(as.character(sub("分鐘", "",sub(".*>", "", scrp$等候時間))))
scrp$口採樣點 = as.numeric(scrp$口採樣點)
scrp$鼻採樣點 = as.numeric(scrp$鼻採樣點)
scrp$DeskCount <- rowSums(scrp[ ,c("口採樣點", "鼻採樣點")], na.rm=TRUE)
scrp$HourNumber <- 
  sapply(strsplit(substr(scrp$DateTimeRound,12,16),":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )
str(scrp)



station <- scrp %>% group_by(序號,Location,類別,DateTimeRound,HourNumber) %>%
  summarise(
    DeskCount.mean = mean(DeskCount, na.rm = TRUE),
    DeskCount.median = median(DeskCount, na.rm = TRUE),
    口採樣點.mean = mean(口採樣點, na.rm = TRUE),
    鼻採樣點.mean = mean(鼻採樣點, na.rm = TRUE),
    口採樣點.median = median(口採樣點, na.rm = TRUE),
    鼻採樣點.median = median(鼻採樣點, na.rm = TRUE),
    WaitingQueue.mean = mean(WaitingQueue, na.rm = TRUE),
    WaitingMinutes.mean = mean(WaitingMinutes, na.rm = TRUE),
    WaitingQueue.median = median(WaitingQueue, na.rm = TRUE),
    WaitingMinutes.median = median(WaitingMinutes, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  as.data.frame()


### Complete for the missing DateTimeRound

unique(station$DateTimeRound)

station <- station %>%
  complete(nesting(Location,序號,類別),
           DateTimeRound = seq.POSIXt(as.POSIXct("2022-06-19 12:00"),
                                      # as.POSIXct("2022-06-19 12:30"),
                                      # as.POSIXct("2022-06-19 13:00"),
                                      as.POSIXct("2022-06-19 13:30"),
                                      by="30 min")
  ) %>%
  mutate(HourNumber = hour(DateTimeRound) + minute(DateTimeRound) / 60) %>%
  arrange(Location,desc(DateTimeRound)) %>%
  group_by(Location) %>%
  fill(`DeskCount.mean`,`DeskCount.median`,
       `口採樣點.mean`,`鼻採樣點.mean`,`口採樣點.median`,`鼻採樣點.median`,
       `WaitingQueue.mean`,`WaitingMinutes.mean`,`WaitingQueue.median`,`WaitingMinutes.median`) %>%
  mutate(DeskCount.ntile = ntile(DeskCount.mean, 5),
         AvgDeskCount = median(DeskCount.mean)) %>%
  ungroup() %>%
  complete(nesting(Location,序號,類別),
           DateTimeRound = seq.POSIXt(as.POSIXct("2022-06-19 17:00"),
                                      # as.POSIXct("2022-06-19 17:30"),
                                      # as.POSIXct("2022-06-19 18:00"),
                                      # as.POSIXct("2022-06-19 18:30"),
                                      # as.POSIXct("2022-06-19 21:00"),
                                      # as.POSIXct("2022-06-19 21:30"),
                                      # as.POSIXct("2022-06-19 22:00"),
                                      # as.POSIXct("2022-06-19 22:30"),
                                      # as.POSIXct("2022-06-19 23:00"),
                                      # as.POSIXct("2022-06-19 23:30"),
                                      # as.POSIXct("2022-06-20 01:00"),
                                      # as.POSIXct("2022-06-20 08:30"),
                                      # as.POSIXct("2022-06-20 09:00"),
                                      # as.POSIXct("2022-06-20 09:30"),
                                      # as.POSIXct("2022-06-20 10:00"),
                                      # as.POSIXct("2022-06-20 10:30"),
                                      # as.POSIXct("2022-06-20 11:00"),
                                      # as.POSIXct("2022-06-20 11:30"),
                                      # as.POSIXct("2022-06-20 12:00"),
                                      # as.POSIXct("2022-06-20 13:00"),
                                      # as.POSIXct("2022-06-20 14:30"),
                                      # as.POSIXct("2022-06-20 15:00"),
                                      as.POSIXct("2022-06-20 21:00"),
                                      by="30 min")
  ) %>%
  mutate(HourNumber = hour(DateTimeRound) + minute(DateTimeRound) / 60) %>%
  arrange(Location,DateTimeRound) %>%
  group_by(Location) %>%
  fill(`DeskCount.mean`,`DeskCount.median`,
       `口採樣點.mean`,`鼻採樣點.mean`,`口採樣點.median`,`鼻採樣點.median`,
       `WaitingQueue.mean`,`WaitingMinutes.mean`,`WaitingQueue.median`,`WaitingMinutes.median`) %>%
  mutate(DeskCount.ntile = ntile(DeskCount.mean, 5),
         AvgDeskCount = median(DeskCount.mean)) %>%
  ungroup()

#filter(station, DateTimeRound == "2022-06-19 12:00")


# Supply for Location Master data
set1 <- merge(station, locMaster)
set1




filelist2 <- dir('data/RNA010', full.names=TRUE)
file2 <- tail(filelist2, 1)

sheets <- c("20220619A","20220620A","20220621A")
df <- data.frame()
for (sheetname in sheets) {
  #sheetname <- "20220619A"
  
  ### Extract first row for location list
  cnames <- read_excel(file2, sheet = sheetname, n_max = 0, na = "---") %>% names()
  lls1 <- sub(".*?-", "",cnames[seq(6, length(cnames), 3)])
  ### Extract data from 2nd row
  rdf1 <- read_excel(file2, sheet=sheetname, na = "---", skip = ifelse(sheetname == "20220619A", 2, 1)) # skip 2 because there exists a hidden row 1 in this spreadsheet
  ### Set date
  rdf1$SwabDate <- as.Date(strptime(str_remove(sheetname, "A"),"%Y%m%d"))
  rdf1$SwabTime <- substr(rdf1$預約時段,1,5)
  ### select columns and rows
  sdf1 <- rdf1 %>% select(c(6:ncol(rdf1))) %>% slice(2:nrow(rdf1)) %>% select(-contains("總人次"))
  ### Repeat Location info for number of rows
  Location <- rep(lls1, each = nrow(sdf1) * 2)
  ### Melt to pivot
  sdf1 <- as.data.frame(sdf1)
  mdf1 <- reshape::melt(sdf1, id = c("SwabDate", "SwabTime"))
  ### Combine Location with dataset
  df1 <- cbind(Location,mdf1)
  ### Clean away column names with ...
  df1$variable <- sub("\\....*", "", df1$variable)
  df <- rbind(df,as.data.frame(df1))
}


pdf <- df %>% pivot_wider(names_from = variable, values_from = value)
pdf <- as.data.frame(pdf)
pdf$SwabCount <- rowSums(pdf[ ,c("口咽拭", "鼻咽拭")], na.rm=TRUE)
pdf$DateTimeRound <- as.POSIXlt(paste(pdf$SwabDate, pdf$SwabTime))
attr(pdf$DateTimeRound, "tzone") <- "Asia/Taipei"
pdf$HourNumber <- 
  sapply(strsplit(pdf$SwabTime,":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )

booking <- pdf %>%
  group_by(Location) %>%
  mutate(AvgSwabCount = mean(SwabCount)) %>%
  ungroup() %>%
  mutate(
    DurationHour = as.numeric((DateTimeRound - ymd_hms("2022-06-19 12:00:00", tz = "Asia/Taipei")),"hours"),
    DurationDay = as.numeric((DateTimeRound - ymd_hms("2022-06-19 12:00:00", tz = "Asia/Taipei")),"days"),
    DurationDayNumber = as.integer(as.numeric((DateTimeRound - ymd_hms("2022-06-19 12:00:00", tz = "Asia/Taipei")),"days") + 1),
    Status = ifelse(DateTimeRound <= Sys.time(), "Done", "Booked")
  )
str(booking)


## filter away C type stations
# pdf <- pdf %>% filter((Location %in% c("鏡湖醫院","南粵青茂口岸","科大體育館", "鏡湖醫院禮堂")))

## Note below location shows up in excel source but not in real time source hence filter away after merged
# B29 鏡湖醫院禮堂
# B30 科大體育館
# B31 南粵青茂口岸
# C04 鏡湖醫院* This one appears stayed as found also in LonLat

set2 <- merge(booking, locMaster)
str(set2)



mdf <- merge(booking, station, by = c("Location","DateTimeRound","HourNumber"))
mdf <- mdf %>%
  mutate(
    DateTimeRound = as.POSIXct(DateTimeRound),
    SwabPerDesk = mdf$SwabCount / ifelse(is.na(mdf$DeskCount.mean) | mdf$DeskCount.mean == 0, 1,
                                         mdf$DeskCount.mean),
    SwabPerDesk.ntile = ntile(SwabPerDesk, 4),
    MouthPerDesk = mdf$口咽拭 / ifelse(is.na(mdf$口採樣點.mean) | mdf$口採樣點.mean == 0 , 1,
                                    mdf$口採樣點.mean),
    NosePerDesk = mdf$鼻咽拭 / ifelse(is.na(mdf$鼻採樣點.mean) | mdf$鼻採樣點.mean == 0, 1,
                                   mdf$鼻採樣點.mean)) %>%
  group_by(Location) %>%
  mutate(AvgSwabPerDesk = mean(SwabPerDesk)) %>%
  ungroup()

throughput <- merge(mdf, locMaster)


sum(booking$SwabCount,na.rm = TRUE)




g01 <- ggplot(data = booking, aes(x = as.POSIXct(DateTimeRound), y = SwabCount, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  xlab("48 Hours") + ylab("Number counts")

g02 <- ggplot(data = booking, aes(x = factor(DurationDayNumber), y = SwabCount / 1000, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  xlab("Day 1 / Day 2") + ylab("Number counts in Thousands")

grid.arrange(g01, g02, ncol = 1, top = paste("Total Swabs Done vs Booked as of", now, sep = " "))





unique(booking$Variable)
options("scipen" = 100, "digits" = 4)

p1 <- df %>% group_by(variable) %>% summarise(value.sum = sum(value, na.rm = TRUE))
p2 <- throughput %>% group_by(area) %>% tally(SwabCount)
rng <- range(0, p1$value.sum, p2$n)

g1 <- ggplot(data=p1, aes(x = variable, y = value.sum, fill = variable)) +
  geom_bar(stat="identity", alpha = .7) + coord_flip(ylim = rng) +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  # guides(alpha = FALSE) + 
  theme_minimal() +
  xlab("SWab Method") + ylab("Swab Counts")

g2 <- ggplot(data=p2, aes(x = reorder(area,n), y = n, fill = area)) +
  geom_bar(stat="identity", alpha = .7) + coord_flip(ylim = rng) +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  # guides(alpha = FALSE) + 
  theme_minimal() +
  xlab("Macao / Cotai") + ylab("Swab Counts")


grid.arrange(g1, g2, ncol = 1, top = "Total Swabs by Method / Location Area")


tilevalue <- c(max(filter(throughput, SwabPerDesk.ntile == 1)$SwabPerDesk),
               max(filter(throughput, SwabPerDesk.ntile == 2)$SwabPerDesk),
               max(filter(throughput, SwabPerDesk.ntile == 3)$SwabPerDesk),
               max(filter(throughput, SwabPerDesk.ntile == 4)$SwabPerDesk))
tilevalue


fl <- as_labeller(
  c(`1` = "below 22.25 swap/desk", `2` = "below 32.86 swap/desk",`3` = "below 43.38 swap/desk", `4` = "below 159.67 swap/desk"))
#c(`1` = "below 27 swap/desk", `2` = "below 33 swap/desk",`3` = "below 42 swap/desk", `4` = "below 122 swap/desk"))

ggplot(throughput, aes(x = HourNumber, fill = factor(DurationDayNumber))) +
  geom_histogram(binwidth = 1, alpha = .7) +
  geom_hline(linetype = "dotted", yintercept = 32, color = "goldenrod") +
  scale_fill_viridis_d(name = "Day", option = 'magma') +
  facet_wrap(~SwabPerDesk.ntile, ncol = 2, labeller = fl) +
  theme_minimal() +
  xlab("24 Hours") + ylab("Counts") +
  ggtitle("Swap per desk in 4-tiles, 24 hours")


p3 <- set2 %>% group_by(Location, Status) %>% tally(SwabCount)

ggplot(p3, aes(x = reorder(Location, n), y = n, fill = Status)) +
  geom_bar(stat = "identity", alpha = .7) + coord_flip() +
  scale_fill_viridis_d(name = "Day", option = 'magma') +
  # guides(alpha = FALSE) + 
  theme_minimal() +
  ggtitle(paste("Total swabs by location as of", now, sep = " ")) + 
  xlab("Location") + ylab("Swab Counts")

p3 <- set2 %>% group_by(LocationEnglish, Status) %>% tally(SwabCount)

ggplot(p3, aes(x = reorder(LocationEnglish, n), y = n, fill = Status)) +
  geom_bar(stat = "identity", alpha = .7) + coord_flip() +
  scale_fill_viridis_d(name = "Day", option = 'magma') +
  # guides(alpha = FALSE) + 
  theme_minimal() +
  ggtitle(paste("Total swabs by location as of", now, sep = " ")) + 
  xlab("Location") + ylab("Swab Counts")



ggplot(set1, aes(x = reorder(Location, AvgDeskCount), y = DeskCount.mean, fill = AvgDeskCount)) +
  geom_boxplot(alpha = .7) +
  coord_flip() +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  # guides(fill = FALSE, alpha = FALSE) + 
  theme_minimal() +
  ggtitle("Number of swab desk by location") + xlab("Location") + ylab("Number of Swab Desks")

# filter(set1, DeskCount.mean >= 20)


unique(throughput$Location)
t <- filter(throughput, Location == "澳門理工大學懷遠樓展覽廳")
unique(throughput$DateTimeRound)

prdf <- throughput %>% 
  group_by(Location, DeskCount.ntile) %>% 
  summarise(SwabCountByDesk = sum(SwabCount, na.rm = TRUE)) %>% 
  ungroup() %>%
  group_by(Location) %>%
  mutate(prop = SwabCountByDesk / sum(SwabCountByDesk)) %>%
  ungroup() %>%
  
  select(-SwabCountByDesk) %>%
  pivot_wider(
    id_cols = Location,
    names_from = DeskCount.ntile,
    values_from = prop
  ) %>% 
  mutate(Location = fct_reorder(Location, `5`)) %>%
  pivot_longer(
    cols = -Location,
    names_to = "DeskCount.ntile",
    values_to = "prop"
  )



ggplot(prdf, aes(DeskCount.ntile, Location)) +
  geom_tile(aes(fill = prop), alpha = .7) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), size = 3) +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  guides(fill = FALSE, alpha = FALSE) + theme_minimal() +
  ggtitle("Number Of swabs in proportions by locations") + xlab("Number of Swab Desks in 5-tiles")  + ylab("Location")

#unique(throughput[c("Location", "AvgSwabPerDesk")]) %>% arrange(-AvgSwabPerDesk)


pldf1 <- throughput[c("Location", "DateTimeRound", "DurationHour","口咽拭", "鼻咽拭")] %>% 
  pivot_longer(
    cols = c("口咽拭", "鼻咽拭"),
    names_to = "variable",
    values_to = "Count"
  )
pldf2 <- throughput[c("Location", "DateTimeRound", "DurationHour","口採樣點.mean","鼻採樣點.mean")] %>% 
  pivot_longer(
    cols = c("口採樣點.mean","鼻採樣點.mean"),
    names_to = "variable",
    values_to = "Count"
  )
pldf <- rbind(pldf1, pldf2) %>%
  mutate(variable = case_when(variable == "口咽拭" ~ "M.Swab Sum",
                              variable == "口採樣點.mean" ~ "M.Swab Desk",
                              variable == "鼻咽拭" ~ "N.Swab Sum",
                              variable == "鼻採樣點.mean" ~ "N.Swab Desk",
                              TRUE ~ "N/A"
  )) %>%
  arrange(Location, DateTimeRound, DurationHour, variable) %>%
  group_by(Location, variable) %>%
  mutate(prop = Count / sum(Count, na.rm = TRUE)) %>%
  ungroup()

unique(pldf$Location)



s1 <- c("澳門文化中心","鏡平學校（中學部）","望廈體育中心一樓","工人體育場一樓")

sh1 <- filter(pldf, Location %in% s1) %>%
  ggplot(aes(DateTimeRound, variable)) +
  geom_tile(aes(fill = prop), alpha = .8) +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  facet_wrap(~Location, ncol = 1) +
  # guides(fill = FALSE, alpha = FALSE) + 
  theme_minimal() +
  xlab("") + ylab("Number proportion")

sb1 <- filter(throughput, Location %in% s1) %>% 
  ggplot(aes(x = DateTimeRound, y = SwabPerDesk, fill = SwabPerDesk)) +
  geom_bar(stat = "identity", alpha = .8) +
  geom_hline(linetype = "dotted", aes(yintercept = AvgSwabPerDesk), color = "goldenrod") +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  facet_wrap(~Location, ncol = 1) +
  # guides(fill = FALSE, alpha = FALSE) + 
  theme_minimal() +
  xlab("") + ylab("Swabs per desk")

grid.arrange(sh1, sb1, nrow = 1, top = "Set of 4 locations from Macao area in 3 days")


s2 <- c("北安客運碼頭","威尼斯人展覽館A、B、C館","澳門大學","奧林匹克體育中心室內體育館")

sh2 <- filter(pldf, Location %in% s2) %>%
  ggplot(aes(DateTimeRound, variable)) +
  geom_tile(aes(fill = prop), alpha = .8) +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  facet_wrap(~Location, ncol = 1) +
  # guides(fill = FALSE, alpha = FALSE) + 
  theme_minimal() +
  xlab("") + ylab("Number proportion")

sb2 <- filter(throughput, Location %in% s2) %>% 
  ggplot(aes(x = DateTimeRound, y = SwabPerDesk, fill = SwabPerDesk)) +
  geom_bar(stat = "identity", alpha = .8) +
  geom_hline(linetype = "dotted", aes(yintercept = AvgSwabPerDesk), color = "goldenrod") +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  facet_wrap(~Location, ncol = 1) +
  #　guides(fill = FALSE, alpha = FALSE) + 
  theme_minimal() +
  xlab("") + ylab("Swabs per desk")

grid.arrange(sh2, sb2, nrow = 1, top = "Set of 4 locations from Cotai area in 3 days")



p4 <- throughput %>% filter(SwabPerDesk.ntile == 4)

plot <- ggmap(get_map(location = "taipa, macao", zoom = 12), darken = .5, 
              base_layer = ggplot(data = p4, aes(x = lon, y = lat, frame = DurationHour, ids = Location))) +
  geom_point(data = p4, aes(color = SwabPerDesk, size = SwabPerDesk, alpha = .5)) +
  scale_size(range = c(0, 12)) +
  scale_color_viridis_c(option = "magma")

ggplotly(plot)





system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
system("taskkill /im geckodriver.exe /f", intern=FALSE, ignore.stdout=FALSE)
