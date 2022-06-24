
library(lubridate)
library(readxl)
library(tidyverse)
library(grid)
library(gridExtra)
library(plotly)
library(ggmap)
library(GGally)

now <- Sys.time()

verMaster <- read_csv("data/version-master/version-master.csv", col_types = cols(StartTime = col_character(), EndTime = col_character()), quote="\"")
versiondf <- filter(verMaster, CurrentFlag == 1)
version <- versiondf %>% select(Version) %>% toString

targetTime <- as.POSIXct(now, "Asia/Taipei")
# targetTime <- as.POSIXct("2022-06-23 11:45", "Asia/Taipei")
targetTimeRound <- floor_date(targetTime, "30mins")

StartTimeStr <- versiondf %>% select(StartTime) %>% toString
EndTimeStr <- versiondf %>% select(EndTime) %>% toString
versionSt <- as.POSIXct(StartTimeStr, "Asia/Taipei")
versionEt <- as.POSIXct(EndTimeStr, "Asia/Taipei")
if (targetTime %within% interval(versionSt, versionEt)) {
  versionEt <- as.POSIXct(targetTime, "Asia/Taipei")
}
versionTi <- interval(versionSt, versionEt)
versionTr <- seq(versionSt, versionEt, "30 mins")

versionEtStr <- str_remove_all(str_remove_all(toString(versionEt), "-"),":")


# dir('data/location-master', full.names=TRUE)
locMaster <- read_csv(paste("data/location-master/location-master-",version,".csv", sep = ""))
# tail(locMaster,18)

### Loop to ingest all scraped data
filelist <- dir('data/aptmon', full.names=TRUE)

scrp <- data.frame()
for (file in filelist) {
  filetimestr <- sub(".csv", "",sub(".*-", "", file))
  filetime <- strptime(filetimestr,"%Y%m%d%H%M%S")
  if(filetime %within% versionTi) {
    temp <- read.csv(file, na = "---")
    temp$DateTime <- as.POSIXlt(filetime)
    scrp <- rbind(scrp,as.data.frame(temp))
  }
}

# tail(scrp[order(scrp[,"DateTime"]),], 30)

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
# unique(station$DateTimeRound)

station <- station %>%
  complete(nesting(Location,序號,類別),
           DateTimeRound = seq.POSIXt(versionSt, versionEt, by="30 min")
  ) %>%
  mutate(HourNumber = hour(DateTimeRound) + minute(DateTimeRound) / 60) %>%
  arrange(Location,DateTimeRound) %>%
  group_by(Location) %>%
  fill(`DeskCount.mean`,`DeskCount.median`,
       `口採樣點.mean`,`鼻採樣點.mean`,`口採樣點.median`,`鼻採樣點.median`,
       `WaitingQueue.mean`,`WaitingMinutes.mean`,`WaitingQueue.median`,`WaitingMinutes.median`) %>%
  mutate(DeskCount.ntile = ntile(DeskCount.mean, 5),
         AvgDeskCount = median(DeskCount.mean)) %>%
  ungroup() %>%
  complete(nesting(Location,序號,類別),
           DateTimeRound = seq.POSIXt(versionSt, versionEt, by="30 min")
  ) %>%
  mutate(HourNumber = hour(DateTimeRound) + minute(DateTimeRound) / 60) %>%
  arrange(Location,desc(DateTimeRound)) %>%
  group_by(Location) %>%
  fill(`DeskCount.mean`,`DeskCount.median`,
       `口採樣點.mean`,`鼻採樣點.mean`,`口採樣點.median`,`鼻採樣點.median`,
       `WaitingQueue.mean`,`WaitingMinutes.mean`,`WaitingQueue.median`,`WaitingMinutes.median`) %>%
  mutate(DeskCount.ntile = ntile(DeskCount.mean, 5),
         AvgDeskCount = median(DeskCount.mean)) %>%
  ungroup()


### Inspect filled data via the complete process
# filter(station, DateTimeRound == "2022-06-19 12:00")

### Supply for Location Master data
set1 <- merge(station, locMaster)


fileTwr <- data.frame()
filelist2 <- dir('data/RNA010', full.names=TRUE)
for (file2 in filelist2) {
  filetimestr2 <- sub(".xlsx", "",sub(".*-", "", file2))
  filetime2 <- strptime(filetimestr2,"%Y%m%d%H%M%S")
  if(filetime2 %within% versionTi) {
    fileTwr <- rbind(fileTwr,as.data.frame(file2))
  }
}
file2 <- tail(fileTwr, 1) %>% toString

day1 <- as.Date(StartTimeStr)
day2 <- as.Date(StartTimeStr)+1
day3 <- as.Date(StartTimeStr)+2
sheet1 <- paste(str_remove_all(day1,"-"),"A",sep="")
sheet2 <- paste(str_remove_all(day2,"-"),"A",sep="")
# sheet3 <- paste(str_remove_all(day3,"-"),"A",sep="")

# sheets <- c(sheet1,sheet2,sheet3)
sheets <- c(sheet1,sheet2)

df <- data.frame()
for (sheetname in sheets) {
  #sheetname <- "20220623A"
  
  ### Extract first row for location list
  cnames <- read_excel(file2, sheet = sheetname, n_max = 0, na = "---") %>% names()
  lls1 <- sub(".*?-", "",cnames[seq(6, length(cnames), 3)])
  ### Extract data from 2nd row
  rdf1 <- read_excel(file2, sheet=sheetname, na = "---", skip = ifelse(sheetname == sheet1, 2, 1)) # skip 2 because there exists a hidden row 1 in this spreadsheet
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
    DurationHour = as.numeric((DateTimeRound - ymd_hms(versionSt, tz = "Asia/Taipei")),"hours"),
    DurationDay = as.numeric((DateTimeRound - ymd_hms(versionSt, tz = "Asia/Taipei")),"days"),
    DurationDayNumber = as.integer(as.numeric((DateTimeRound - ymd_hms(versionSt, tz = "Asia/Taipei")),"days") + 1),
    Status = ifelse(DateTimeRound <= versionEt, "2.已完成", "1.預約中")
  )
str(booking)

### Supply for Location Master data
set2 <- merge(booking, locMaster)
str(set2)



mdf <- merge(booking, station, by = c("Location", "DateTimeRound","HourNumber")) #, all.x=TRUE)
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


## Basic checking

# str(scrp)
# str(station)
# str(set1)

# str(pdf)
# str(booking)
# str(set2)

# str(mdf)
# str(throughput)

# head(throughput[order(throughput[,"DateTimeRound"]),], 30)



totalSwabBooking <- sum(booking$SwabCount,na.rm = TRUE)
totalSwabDone <- sum(throughput$SwabCount,na.rm = TRUE)

## Start plotting
options("scipen" = 100, "digits" = 4)

p1 <- df %>% mutate(Status = ifelse(as.POSIXlt(paste(df$SwabDate, df$SwabTime)) <= versionEt, "2.已完成", "1.預約中")) %>%
  group_by(variable, Status) %>%
  summarise(value.sum = sum(value, na.rm = TRUE))
p2 <- set2 %>% group_by(area, Status) %>% tally(SwabCount)

g1 <- ggplot(data=p1, aes(x = variable, y = value.sum, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  coord_flip() +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  xlab("採集方法") + ylab("採樣數") +
  ggtitle("口鼻採集法總計數")
  # ggtitle("Total Swabs Done vs Booked by Swab Method")

g2 <- ggplot(data=p2, aes(x = reorder(area,n), y = n, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  coord_flip() +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  xlab("地區") + ylab("採樣數") +
  ggtitle("地區總計數")

g3 <- ggplot(data = booking, aes(x = as.POSIXct(DateTimeRound), y = SwabCount, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  xlab("每小時") + ylab("採樣數") +
  ggtitle("每小時總計數")

g4 <- ggplot(data = booking, aes(x = reorder(factor(DurationDayNumber),SwabCount), y = SwabCount, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  coord_flip() +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  theme_minimal() +
  xlab("第N天") + ylab("採樣數") +
  ggtitle("當天總計數")

# grid.arrange(g1, g3, g2, g4, ncol = 2,
# top = paste("Total Swab Done ", totalSwabDone, " out of ", totalSwabBooking, " bookings as of ", targetTimeRound, sep = " "))


# tilevalue <- c(max(filter(throughput, SwabPerDesk.ntile == 1)$SwabPerDesk),
#                max(filter(throughput, SwabPerDesk.ntile == 2)$SwabPerDesk),
#                max(filter(throughput, SwabPerDesk.ntile == 3)$SwabPerDesk),
#                max(filter(throughput, SwabPerDesk.ntile == 4)$SwabPerDesk))
# tilevalue
# 
# 
# fl <- as_labeller(
#   c(`1` = "below 23 swab/counter", `2` = "23 - 35 swab/counter",`3` = "35 - 50 swab/counter", `4` = "above 50 swab/counter"))
# 
# ggplot(throughput, aes(x = HourNumber, fill = factor(DurationDayNumber))) +
#   geom_histogram(binwidth = 1, alpha = .7) +
#   geom_hline(linetype = "dotted", yintercept = 32, color = "goldenrod") +
#   scale_fill_viridis_d(name = "Day", option = 'magma') +
#   facet_wrap(~SwabPerDesk.ntile, ncol = 2, labeller = fl) +
#   theme_minimal() +
#   xlab("24 Hours") + ylab("Counts") +
#   ggtitle("Swap per counter in 4-tiles, 24 hours")


areaList <- unique(locMaster$area)

for (setArea in areaList) {
# setArea <- #"半島南"

# if (setArea == "Macau") {
#   areaSet <- c("澳門文化中心","鏡平學校（中學部）","望廈體育中心一樓","工人體育場一樓")
# } else if (setArea == "Taipa") {
#   areaSet <- c("北安客運碼頭","威尼斯人展覽館A、B、C館","澳門大學","奧林匹克體育中心室內體育館")
# } else if (setArea == "Coloane") {
#   areaSet <- c("石排灣公立學校","澳門保安部隊高等學校","街總石排灣家庭及社區綜合服務中心","澳門大學")
# }

# unique(throughput[c("Sno","Location", "AvgSwabPerDesk")])
areaSet0 <- throughput %>% filter(area == setArea & substr(Sno,0,1) == "B") %>%
  group_by(Location) %>%
  tally(AvgSwabPerDesk) %>%
  slice_min(n, n = 3) %>%
  select(Location)

areaSet <- paste(areaSet0$Location, sep=" ", collapse=NULL)

p5 <- set2 %>% filter(Location %in% areaSet) %>%
  group_by(Location, Status) %>%
  tally(SwabCount)

g5 <- ggplot(p5, aes(x = reorder(Location, n), y = n, fill = Status)) +
  geom_bar(stat = "identity", alpha = .7) + coord_flip() +
  scale_fill_viridis_d(name = "Day", option = 'magma') +
  theme_minimal() +
  xlab("採樣站") + ylab("總計數") +
  ggtitle("採樣站已完成及預約中總計數(預約中佔比較少為佳)")
  
p6 <- set1 %>% filter(Location %in% areaSet)

g6 <- ggplot(p6, aes(x = reorder(Location, AvgDeskCount), y = DeskCount.mean, fill = AvgDeskCount)) +
  geom_boxplot(alpha = .7) +
  coord_flip() +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  theme_minimal() +
  xlab("採樣站") + ylab("可採樣點數目") +
  ggtitle("採樣站可採樣點中位數(較高為佳)")

# grid.arrange(g5, g6, ncol = 2, top = paste("Number of swab counters by location as of", targetTimeRound, sep = " "))

# filter(set1, DeskCount.mean >= 20)



# prdf <- throughput %>% 
#   group_by(Location, DeskCount.ntile) %>% 
#   summarise(SwabCountByDesk = sum(SwabCount, na.rm = TRUE)) %>% 
#   ungroup() %>%
#   group_by(Location) %>%
#   mutate(prop = SwabCountByDesk / sum(SwabCountByDesk)) %>%
#   ungroup() %>%
#   
#   select(-SwabCountByDesk) %>%
#   pivot_wider(
#     id_cols = Location,
#     names_from = DeskCount.ntile,
#     values_from = prop
#   ) %>% 
#   mutate(Location = fct_reorder(Location, `5`)) %>%
#   pivot_longer(
#     cols = -Location,
#     names_to = "DeskCount.ntile",
#     values_to = "prop"
#   )
# 
# ggplot(prdf, aes(DeskCount.ntile, Location)) +
#   geom_tile(aes(fill = prop), alpha = .7) +
#   geom_text(aes(label = scales::percent(prop, accuracy = 1)), size = 3) +
#   scale_fill_viridis_c(option = 'magma', direction = -1) +
#   theme_minimal() +
#   xlab("Number of Swab Counters in 5-tiles")  + ylab("Location") + 
#   ggtitle("Number of swabs in proportions by locations")

#unique(throughput[c("Location", "AvgSwabPerDesk")]) %>% arrange(-AvgSwabPerDesk)


# tp1 <- throughput[c("Location", "DateTimeRound", "DurationHour","口咽拭", "鼻咽拭")] %>% 
#   pivot_longer(
#     cols = c("口咽拭", "鼻咽拭"),
#     names_to = "variable",
#     values_to = "Count"
#   )
# tp2 <- throughput[c("Location", "DateTimeRound", "DurationHour","口採樣點.mean","鼻採樣點.mean")] %>% 
#   pivot_longer(
#     cols = c("口採樣點.mean","鼻採樣點.mean"),
#     names_to = "variable",
#     values_to = "Count"
#   )
# tp <- rbind(tp1, tp2) %>%
#   mutate(variable = case_when(variable == "口咽拭" ~ "M.Swab Sum",
#                               variable == "口採樣點.mean" ~ "M.Swab Counter",
#                               variable == "鼻咽拭" ~ "N.Swab Sum",
#                               variable == "鼻採樣點.mean" ~ "N.Swab Counter",
#                               TRUE ~ "N/A"
#   )) %>%
#   arrange(Location, DateTimeRound, DurationHour, variable) %>%
#   group_by(Location, variable) %>%
#   mutate(prop = Count / sum(Count, na.rm = TRUE)) %>%
#   ungroup()

# unique(tp$Location)


sb0 <- filter(set2, Location %in% areaSet) %>% 
  ggplot(aes(x = as.POSIXct(DateTimeRound), y = SwabCount, fill = Status)) +
  geom_bar(stat="identity", alpha = .7) +
  scale_color_viridis_d(option = 'magma') + scale_fill_viridis_d(option = 'magma') +
  facet_wrap(~Location, ncol = 1) +
  theme_minimal() +
  xlab("每小時") + ylab("總計數") +
  ggtitle("採樣站已完成及預約數每小時變化")

# sh0 <- filter(tp, Location %in% areaSet) %>%
#   ggplot(aes(DateTimeRound, variable)) +
#   geom_tile(aes(fill = prop), alpha = .8) +
#   scale_fill_viridis_c(option = 'magma', direction = -1) +
#   facet_wrap(~LocationEnglish, ncol = 1) +
#   theme_minimal() +
#   xlab("Hours in interval") + ylab("Number proportion")

st0 <- filter(throughput, Location %in% areaSet) %>% 
  ggplot(aes(x = DateTimeRound, y = SwabPerDesk, fill = SwabPerDesk)) +
  geom_bar(stat = "identity", alpha = .8) +
  geom_hline(linetype = "dotted", aes(yintercept = AvgSwabPerDesk), color = "goldenrod") +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  facet_wrap(~Location, ncol = 1) +
  theme_minimal() +
  xlab("每小時") + ylab("吞吐量") +
  ggtitle("採樣站吞吐量(即每採樣點每半小時採樣數)")

# grid.arrange(sb0, st0, nrow = 1, top = "Set of 4 locations from Macao area")

png(paste("Macau All People NAT ", versionEtStr, " ", setArea, ".png",sep = ""), width = 1200, height = 2000, res = 100)
# grid.arrange(g1, g3, g2, g4, g6, g8, sb1, st1, sb2, st2, ncol = 2,
grid.arrange(layout_matrix = rbind(c(1,2), c(3,4), c(5,5), c(6,6)), g3, g2, g5, g6, sb0, st0, ncol = 2,
top = textGrob(paste("全民核酸總完成數 ", format(totalSwabDone,big.mark=",",scientific=FALSE)
          ," / ", format(totalSwabBooking,big.mark=",",scientific=FALSE)
          , " 數據基於現在時間 ", targetTimeRound, "\n以下統整為",setArea,"區，僅取該區單採樣點最低吞吐量三採樣站", sep = ""), gp=gpar(fontsize=24,font=8)),
bottom = textGrob(paste("* 數據來源官方但由民間統計整理，可能有延遲。 ** 各地區僅選取最低吞吐量三個採樣站，供排隊引流參考。"), gp=gpar(fontsize=16,font=8)))
dev.off()
}


# tp4tile <- throughput %>% filter(SwabPerDesk.ntile == 4)
# 
# plot <- ggmap(get_map(location = "taipa, macao", zoom = 12), darken = .5, 
#               base_layer = ggplot(data = tp4tile, aes(x = lon, y = lat, frame = DurationHour, ids = Location))) +
#   geom_point(data = tp4tile, aes(color = SwabPerDesk, size = SwabPerDesk, alpha = .5)) +
#   scale_size(range = c(0, 12)) +
#   scale_color_viridis_c(option = 'magma')
# 
# ggplotly(plot)
