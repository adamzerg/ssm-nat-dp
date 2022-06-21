
library(tidyverse)
library(ggmap)

## sets Google map for this session
register_google(key = "AIzaSyD9z90fvzxmOhRzoNbxbwOmuIXI6CVKcTE")
ggmap_hide_api_key()

# locationList <- dir('data/location-master', full.names=TRUE)
# locationList

locScrpZh <- read.csv("data/location-master/aptmon-location-chinese.csv", quote="\"")
locScrpEn <- read_csv("data/location-master/aptmon-location-english.csv", quote="\"")
locXlsx <- read_csv("data/location-master/RNA010-location.csv")

# view(locScrpZh)
# view(locScrpEn)
# view(locXlsx)

# 53
# str(locScrpZh)
# 53
# str(locScrpEn)
# 32
# str(locXlsx)

# For data quality, ensure number of records stays same as 53
locMaster.1 <- merge(locScrpZh,locScrpEn)
# str(locMaster.1)

# merge all, cross join 2 sets 53 vs 32
locMaster.2 <- merge(x=locMaster.1,y=locXlsx,by="Sno",all=TRUE)
# str(locMaster.2)
# view(locMaster.2)

# check location name in chinese for differences
# filter(locMaster.2, is.na(LocationChinese))
# filter(locMaster.2, is.na(LocationEnglish))
# filter(locMaster.2, is.na(LocationXlsx))

# Assign name from Xlsx
locMaster.2$Location <- ifelse(is.na(locMaster.2$LocationChinese), locMaster.2$LocationXlsx, locMaster.2$LocationChinese)

# Assign English name from Xlsx
# B29 鏡湖醫院禮堂
# B30 科大體育館
# B31 南粵青茂口岸
locMaster.2$LocationEnglish <- ifelse(locMaster.2$Location == "鏡湖醫院禮堂","Kiang Wu Hospital 3rd floor",locMaster.2$LocationEnglish)
locMaster.2$LocationEnglish <- ifelse(locMaster.2$Location == "科大體育館","Gymnasium at M.U.S.T",locMaster.2$LocationEnglish)
locMaster.2$LocationEnglish <- ifelse(locMaster.2$Location == "南粵青茂口岸","Namyue QingMao Port",locMaster.2$LocationEnglish)
# Correction for English names
locMaster.2$LocationEnglish <- ifelse(locMaster.2$Location == "塔石體育館B館","Tap Seac Multisport Pavilion - Pavilion B",locMaster.2$LocationEnglish)
locMaster.2$LocationEnglish <- ifelse(locMaster.2$Location == "威尼斯人展覽館A、B、C館","The Venetian Macau Exhibition A B C",locMaster.2$LocationEnglish)
locMaster.2$LocationEnglish <- ifelse(locMaster.2$Location == "工人體育場一樓","Macao Federation of Trade Unions Workers Stadium 1st floor",locMaster.2$LocationEnglish)

# str(locMaster.2)
# view(locMaster.2)

## Prepare for Location adding lon and lat
locMaster.2$MapLoc <- ifelse(locMaster.2$Location == "科大醫院","Macao, University Hospital",locMaster.2$Location)
locMaster.2$MapLoc <- ifelse(locMaster.2$Location == "科大體育館","Macao, 澳門科技大學室內體育館 Gymnasium",locMaster.2$MapLoc)
locMaster.2$MapLoc <- ifelse(locMaster.2$Location == "街總石排灣家庭及社區綜合服務中心","Macao, 石排灣業興大廈",locMaster.2$MapLoc)
locMaster.2$MapLoc <- ifelse(locMaster.2$Location == "沙梨頭活動中心","沙梨頭街市",locMaster.2$MapLoc)
locMaster.2$MapLoc <- ifelse(locMaster.2$Location == "培正中學","澳門培正中學",locMaster.2$MapLoc)
locMaster.2[grep("奧林匹克體育中心.*", locMaster.2$Location, perl=T), ]$MapLoc <- "奧林匹克體育中心"
locMaster.2[grep(".*湖畔", locMaster.2$Location, perl=T), ]$MapLoc <- "Edifício do Lago, Macao"
locMaster.2[grep("工人體育.*", locMaster.2$Location, perl=T), ]$MapLoc <- "Campo dos Operários da Associação Geral dos Operários de Macau"
locMaster.2[grep("望廈體育中心.*", locMaster.2$Location, perl=T), ]$MapLoc <- "望廈體育中心 Centro Desportivo Mong-Há"
locMaster.2[grep("南粵青茂口岸.*", locMaster.2$Location, perl=T), ]$MapLoc <- "青茂口岸澳門邊檢大樓"
locMaster.2[grep("威尼斯人.*", locMaster.2$Location, perl=T), ]$MapLoc <- "澳門威尼斯人"
locMaster.2 <- mutate_geocode(locMaster.2, MapLoc)
locMaster.2$area <- ifelse(locMaster.2$lat>=22.17,'Macao','Taipa')

# str(locMaster.2)
# view(locMaster.2)

locMaster <- data.frame(locMaster.2[ , c("Sno", "Location", "LocationEnglish", "lon", "lat", "area")])
# view(locMaster)

## Export to csv
write_csv(locMaster, "data/location-master/location-master-20220619.csv")
