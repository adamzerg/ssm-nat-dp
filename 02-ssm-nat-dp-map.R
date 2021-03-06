
setwd(paste(Sys.getenv("OneDrive"), "\\GitHub\\ssm-nat-dp", sep=''))

library(lubridate)
library(data.table)
library(readxl)
library(tidyverse)
library(grid)
library(gridExtra)
library(plotly)
library(ggmap)
library(GGally)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(sp)
library(rgeos)


### Set time, version, hence pick location master data
now <- Sys.time()

verMaster <- read_csv("data/version-master/version-master.csv", col_types = cols(StartTime = col_character(), EndTime = col_character()), quote="\"")
versiondf <- filter(verMaster, CurrentFlag == 1)
version <- versiondf %>% select(Version) %>% toString

# SWitch the target time HERE to now for real-time capture!!!
targetTime <- as.POSIXct(now, "Asia/Taipei")
# targetTime <- as.POSIXct("2022-07-12 21:48", "Asia/Taipei")

StartTimeStr <- versiondf %>% select(StartTime) %>% toString
EndTimeStr <- versiondf %>% select(EndTime) %>% toString
versionSt <- as.POSIXct(StartTimeStr, "Asia/Taipei")
versionEt0 <- as.POSIXct(EndTimeStr, "Asia/Taipei")
versionEt <- as.POSIXct(EndTimeStr, "Asia/Taipei") - 1 # Avoid version ended given no result capture
if (targetTime %within% interval(versionSt, versionEt)) {
  versionEt <- as.POSIXct(targetTime, "Asia/Taipei")
}
versionEtRound <- floor_date(versionEt, "30mins")
versionTi <- interval(versionSt, versionEt)
versionTr <- seq(versionSt, versionEt, "30 mins")
versionTr0 <- seq(versionSt, versionEt0, "30 mins")

versionEtStr <- str_remove_all(str_remove_all(toString(versionEt), "-"),":")

# dir('data/location-master', full.names=TRUE)
locMaster <- read_csv(paste("data/location-master/location-master-",version,".csv", sep = ""))
# print(locMaster, n=70)


### Loop to ingest all scraped data fallen within the version interval
# if (versionSt < '2022-07-07') {
# filelist <- dir('data/station', full.names=TRUE)
# tailfile <- tail(filelist, 1)
# scrp <- data.frame()
#   for (file in filelist) {
#     filetimestr <- sub(".csv", "", sub(".*-", "", file))
#     filetime <- strptime(filetimestr,"%Y%m%d%H%M%S")
#     tailfileflag <- ifelse(file == tailfile, 1, 0)
#     if (filetime %within% versionTi) {
#       temp <- read.csv(file, na = "---")
#       temp$DateTime <- as.POSIXlt(filetime)
#       temp$CurrentFlag <- tailfileflag
#       scrp <- rbind(scrp,as.data.frame(temp))
#     }
#   }
#   setnames(scrp,c("等候時間","等候時間.1"),c("SymbolToRemove","等候時間"))
# } else {
filelist <- dir('data/aptmon', full.names=TRUE)
tailfile <- tail(filelist, 1)
scrp <- data.frame()
  for (file in filelist) {
    filetimestr <- sub(".xlsx", "", sub(".*-", "", file))
    filetime <- strptime(filetimestr,"%Y%m%d%H%M%S")
    tailfileflag <- ifelse(file == tailfile, 1, 0)
    if (filetime %within% versionTi) {
      temp <- read_excel(file, na = "---")
      temp$DateTime <- as.POSIXlt(filetime)
      temp$CurrentFlag <- tailfileflag
      scrp <- rbind(scrp,as.data.frame(temp))
    }
  }

# tail(scrp[order(scrp[,"DateTime"]),], 70)

### Date and time transformation
attr(scrp$DateTime, "tzone") <- "Asia/Taipei"
scrp$DateTimeRound <- floor_date(scrp$DateTime, "30mins")
scrp$WaitingQueue <- as.numeric(as.character(sub("*人", "", scrp$輪候人數)))
scrp$WaitingMinutes <- as.numeric(as.character(sub("分鐘", "",sub(".*>", "", scrp$等候時間))))
scrp$口採樣點 = as.numeric(scrp$口採樣點)
scrp$鼻採樣點 = as.numeric(scrp$鼻採樣點)
scrp$DeskCount <- rowSums(scrp[ ,c("口採樣點", "鼻採樣點")], na.rm=TRUE)
scrp$HourNumber <- sapply(strsplit(substr(scrp$DateTimeRound,12,16),":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )
# str(scrp)


station <- scrp %>% group_by(序號,Location,DateTimeRound,HourNumber) %>%
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
    WaitingQueue.current = max(ifelse(CurrentFlag == 1, WaitingQueue, 0)),
    WaitingMinutes.current = max(ifelse(CurrentFlag == 1, WaitingMinutes, 0)),
    MaxCurrentFlag = max(CurrentFlag)
  ) %>%
  ungroup() %>%
  as.data.frame()
# str(station)

# filter(scrp, Location == "MGM Cotai")
### Complete for the missing DateTimeRound, note there's no filling result of MaxCurrentFlag
# unique(scrp$Location)
# unique(station$DateTimeRound)

station <- station %>%
  complete(nesting(Location,序號),
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
  complete(nesting(Location,序號),
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

# str(set1)
# t1 <- filter(station, MaxCurrentFlag == 1)
# t2 <- filter(station, MaxCurrentFlag == 0)
# t3 <- filter(station, is.na(MaxCurrentFlag))
# str(t1)
# str(t2)
# str(t3)

# t1 <- filter(station, WaitingQueue.current > 0)
# t2 <- filter(station, WaitingQueue.current <= 0)
# t3 <- filter(station, is.na(WaitingQueue.current))
# str(t1)
# str(t2)
# str(t3)



### Locate for booking data
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

### Loop to ingest for all sheets
df <- data.frame()
for (sheetname in sheets) {
  # sheetname <- "20220710A"
  
  ### Extract first row for location list
  cnames <- read_excel(file2, sheet = sheetname, n_max = 0, na = "---") %>% names()
  cnames <- gsub("\n", "\r\n", cnames)
  cn1 <- cnames[seq(6, length(cnames))]
  lls1 <- sub(".*?-", "",cn1[grepl("^[^...*]", cn1)])
  ### Extract data from 2nd row
  rdf1 <- read_excel(file2, sheet=sheetname, na = "---", skip = ifelse(sheetname == sheet1, 2, 1)) # skip 2 because there exists a hidden row 1 in this spreadsheet
  ### Set date
  rdf1$SwabDate <- as.Date(strptime(str_remove(sheetname, "A"),"%Y%m%d"))
  rdf1$SwabTime <- substr(rdf1$預約時段,1,5)
  ### select columns and rows
  sdf1 <- rdf1 %>% select(c(6:ncol(rdf1))) %>% slice(2:nrow(rdf1)) %>% select(-contains("總人次"))
  ### Repeat Location info for number of rows
  dtls <- as.data.table(lls1)
  setnames(dtls, "lls1", "Location")
  Location <- dtls[, repeats:=ifelse(grepl("^流動核酸採樣車.*", Location), nrow(sdf1), nrow(sdf1) * 2)][rep(1:.N,repeats)]
  ### Melt to pivot
  sdf1 <- as.data.frame(sdf1)
  mdf1 <- reshape::melt(sdf1, id = c("SwabDate", "SwabTime"))
  ### Combine Location with dataset
  df1 <- cbind(Location,mdf1)
  ### Clean away column names with ...
  df1$variable <- sub("\\....*", "", df1$variable)
  df <- rbind(df,as.data.frame(df1))
}

# df %>% filter(Location %like% '流動核酸採樣車')

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

# pdf %>% filter(Location %like% '流動核酸採樣車')


booking <- pdf %>%
  group_by(Location) %>%
  mutate(AvgSwabCount = mean(SwabCount)) %>%
  ungroup() %>%
  mutate(
    DurationHour = as.numeric((DateTimeRound - ymd_hms(versionSt, tz = "Asia/Taipei")),"hours"),
    DurationDay = as.numeric((DateTimeRound - ymd_hms(versionSt, tz = "Asia/Taipei")),"days"),
    DurationDayNumber = as.integer(as.numeric((DateTimeRound - ymd_hms(versionSt, tz = "Asia/Taipei")),"days") + 1),
    Status = ifelse(DateTimeRound <= versionEt, "1.已完成", "2.預約中")
  )
# str(booking)

### Supply for Location Master data
set2 <- merge(booking, locMaster)

# str(set2)

### Combine for bookings and station data into throughput dataframe
mdf <- merge(booking, station, by = c("Location", "DateTimeRound","HourNumber")) #, all.x=TRUE)
mdf <- mdf %>%
  mutate(
    DateTimeRound = as.POSIXct(DateTimeRound),
    SwabPerDesk = mdf$SwabCount / ifelse(is.na(mdf$DeskCount.mean) | mdf$DeskCount.mean == 0, 1,
                                         mdf$DeskCount.mean),
    SwabPerDesk.ntile = ntile(SwabPerDesk, 4),
    SwabPerDesk.color = case_when(SwabPerDesk.ntile == 4 ~ "coral",
                                SwabPerDesk.ntile == 3 ~ "goldenrod",
                                SwabPerDesk.ntile == 2 ~ "steelblue",
                                SwabPerDesk.ntile == 1 ~ "seagreen",
                                TRUE ~ "chocolate"
                                ),
    MouthPerDesk = mdf$口咽拭 / ifelse(is.na(mdf$口採樣點.mean) | mdf$口採樣點.mean == 0 , 1,
                                    mdf$口採樣點.mean),
    NosePerDesk = mdf$鼻咽拭 / ifelse(is.na(mdf$鼻採樣點.mean) | mdf$鼻採樣點.mean == 0, 1,
                                   mdf$鼻採樣點.mean)) %>%
  group_by(Location) %>%
  mutate(AvgSwabPerDesk = mean(SwabPerDesk)) %>%
  ungroup()

throughput <- merge(mdf, locMaster)


print("Log: step 1 data preparation succeeded")

### Basic validations

# mdf %>% group_by(SwabPerDesk.ntile) %>%
#       summarise(minSwabPerDesk = min(SwabPerDesk),
#                 maxSwabPerDesk = max(SwabPerDesk)) %>%
#       print(n=70)
# 
# mdf %>% group_by(SwabPerDesk.ntile) %>% summarise(count = n_distinct(Location)) %>% print(n=70)

# locMaster %>% print(n=70)
# station %>% group_by(Location) %>% summarise(count = n_distinct(DateTimeRound)) %>% print(n=70)
# booking %>% group_by(Location) %>% summarise(count = n_distinct(DateTimeRound)) %>% print(n=70)

# str(scrp)
# str(station)
# str(set1)

# str(pdf)
# str(booking)
# str(set2)

# print(locMaster, n=70)
# str(mdf)
# str(throughput)


# head(throughput[order(throughput[,"DateTimeRound"]),], 60)

# set1 %>% group_by(Location) %>% summarise(count = n_distinct(DateTimeRound)) %>% print(n=70)
# booking %>% group_by(Location) %>% summarise(count = n_distinct(DateTimeRound)) %>% print(n=70)
# t <- filter(booking, Location %like% "流動核酸採樣車")
# unique(t$Location)
# t2 <- filter(locMaster, Location %like% "流動核酸採樣車")
# unique(t2$Location)
# t3 <- filter(throughput, Location %like% "流動核酸採樣車")
# unique(t3$Location)


### Start generate for map html with leaflet
totalSwabBooking <- sum(booking$SwabCount,na.rm = TRUE)
totalSwabDone <- sum(throughput$SwabCount,na.rm = TRUE)

mapTitle <- paste("本輪全民核酸總完成<br>", format(totalSwabDone,big.mark=",",scientific = FALSE)
          ," / ", format(totalSwabBooking,big.mark = ",",scientific = FALSE)
          , " <br>於 ", versionEt, sep = "")
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 5px;
    padding-right: 5px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
"))

mapFooter <- paste("* 數據來源澳門衛生局<br>** 地圖由澳門數據科學應用協會<br>  Macau Data Lab by MDSAA 統整繪製")
tag.map.footer <- tags$style(HTML("
  .leaflet-control.map-bottom {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 16px;
  }
"))


title <- tags$div(
  tag.map.title, HTML(mapTitle)
)

footer <- tags$div(
  tag.map.footer, HTML(mapFooter)
)



leaf <- throughput %>% filter(DateTimeRound == versionEtRound & SwabCount > 0)

colorlabel <- unique(leaf[c("SwabPerDesk.ntile", "SwabPerDesk.color")]) %>%
            mutate(WaitingLevel = case_when(SwabPerDesk.ntile == 4 ~ "逼迫 / Max",
                                SwabPerDesk.ntile == 3 ~ "多人 / Crowded",
                                SwabPerDesk.ntile == 2 ~ "稍等 / Some Crowd",
                                SwabPerDesk.ntile == 1 ~ "輕鬆 / Not Crowded",
                                TRUE ~ ""),)
label <- colorlabel[order(colorlabel[,1]),]


# create the string for responsiveness that will be injected in the <head> section of the leaflet output html file. Note that the quotes were escaped using the backslash character : `\`.  
responsiveness = "\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=0.7, maximum-scale=1.0\">\'"

locMap <- leaflet() %>% addProviderTiles(providers$CartoDB.Voyager) %>%
addCircleMarkers(data = leaf, lng = ~lon, lat = ~lat,
    weight = 5,
    radius = sqrt(leaf$SwabCount) * 4,
    color = ~SwabPerDesk.color,
    fillOpacity = .2,
    label = ~paste(Sno,Location,"\n等待",WaitingQueue.current,"人","\n需時",WaitingMinutes.current,"分鐘"),
    popup = ~paste("採樣站: ", Location,
                    "<br>Location: ", LocationEnglish,
                    "<br>現時等待人數 / Number in queuing: ", WaitingQueue.current,
                    "<br>現時等待需分鐘 / Queuing in minutes: ", WaitingMinutes.current,
                    "<br>預採樣數 / Number to swab: ", SwabCount,
                    "<br>口採樣點 / Counters of mouth swab: ", 口採樣點.median,
                    "<br>鼻採樣點 / Counters of nasal swab: ", 鼻採樣點.median,
                    "<br>每採樣枱預約数 / Swabs per counter: ", round(SwabPerDesk),
                    "<br>每採樣枱預待時級別 / Per counter waiting level: ", SwabPerDesk.ntile,
                    "<br> <br>",
                    "<img src = ", Sno, ".png>",
                    sep = ""),
    labelOptions = labelOptions(textsize = "18px"),
    clusterOptions = markerClusterOptions()
    ) %>%
addLegend(title = "等待級別 / Waiting level", position = "bottomright", colors = label$SwabPerDesk.color, labels = label$WaitingLevel) %>%
addControl(title, position = "topleft", className = "map-title") %>% 
addControl(footer, position = "bottomleft", className = "map-footer") %>%
htmlwidgets::onRender(paste0("
    function(el, x) {
      $('head').append(",responsiveness,");
    }"))

saveWidget(locMap, title = "MODL全民核酸輪候地圖", file = "macau-all-people-nat.html",selfcontained = TRUE)

print("Log: step 2 map generated succeeded")




### Generate of thumbnails
lonlat <- unique(throughput[, c("Sno","Location","LocationEnglish","lon","lat","area")])
sp.lonlat <- lonlat
coordinates(sp.lonlat) <- ~lon+lat
d <- gDistance(sp.lonlat, byid=T)
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2])
locNeibor <- cbind(lonlat, lonlat[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]))
colnames(locNeibor) <- c(colnames(lonlat), "n.Sno","n.Location","n.LocationEnglish","n.lon","n.lat","n.area","distance")
locNeibor <- locNeibor[order(locNeibor$Sno),]
row.names(locNeibor) <- NULL
# view(locNeibor)


for (i in 1:nrow(locNeibor)) {
# neibor <- filter(item, Location == "澳門大學") %>% select("Location","n.Location")
itemSno <- locNeibor[i,]$Sno
itemLocation <- locNeibor[i,]$Location
itemNeibor <- locNeibor[i,]$n.Location

neib1 <- filter(set2, Location %in% c(itemLocation, itemNeibor)) %>%
  mutate(n.color = ifelse(Location == itemNeibor, paste("2.",itemNeibor,sep=""), paste("1.",itemLocation,sep="")))
p1 <-
  ggplot(neib1, aes(x = as.POSIXct(DateTimeRound), y = SwabCount, color = n.color, linetype = Status)) +
  geom_line() +
  coord_cartesian(xlim = c(versionSt, versionEt0)) +
  scale_color_brewer(palette = "Paired", direction = -1) +
  theme_minimal() +
  theme(legend.title=element_blank(),legend.position="top") +
  xlab("每半小時") +
  ylab("採樣量")

neib2 <- filter(throughput, Location %in% c(itemLocation,itemNeibor)) %>%
  mutate(n.color = ifelse(Location == itemNeibor, paste("2.",itemNeibor,sep=""), paste("1.",itemLocation,sep="")))
# neib2$Location <- factor(Location, levels = c(itemLocation,itemNeibor)) # Reordering group factor levels
p2 <-
  ggplot(neib2, aes(x = DateTimeRound, y = SwabPerDesk, group = Location, fill = n.color)) +
  geom_bar(stat = "identity", alpha = .8) +
  geom_hline(linetype = "dotted", aes(yintercept = AvgSwabPerDesk), color = "grey") +
  coord_cartesian(xlim = c(versionSt, versionEt0)) +
  scale_fill_brewer(palette = "Paired", direction = -1) +
  facet_wrap(~n.color, ncol = 1) +
  theme_minimal() +
  theme(legend.position="none") +
  xlab("每半小時") + 
  ylab("吞吐量")

png(paste(itemSno, ".png",sep = ""), width = 280, height = 350, res = 70)
# png(paste(itemSno, ".png",sep = ""), width = 600, height = 700, res = 100)
grid.arrange(p1, p2, ncol = 1,
  top = textGrob(paste("與最鄰近",itemNeibor,"比較"))
)
dev.off()
}

print("Log: step 3 plotting comparison succeeded")