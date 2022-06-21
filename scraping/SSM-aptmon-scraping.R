library(netstat)
library(rvest)
library(data.table)
library(httr)
library(XML)
library(RSelenium)
library(netstat)
library(httpuv)
library(dplyr)
library(stringr)

# Sys.getlocale()
# Sys.setlocale("LC_ALL","Chinese")

# port = netstat::free_port()
port <- randomPort(min = 1024L, max = 49151L, host = "127.0.0.1", n = 20)
mybrowser <- rsDriver(browser = 'firefox', verbose = TRUE, port = port)
Sys.sleep(10)

link <- "https://eservice.ssm.gov.mo/aptmon/ch"
mybrowser$client$navigate(link)
Sys.sleep(60)

#mybrowser$client$findElement(using = 'id', "tblist")$getElementText()
html.table.0 <-  mybrowser$client$findElement(using = 'id', "tblist")
webElem5txt.0 <- html.table.0$getElementAttribute("outerHTML")[[1]]

#read_html(webElem5txt.0) %>% html_nodes('div.hidden-xs') %>% html_text(trim = TRUE)
df.table.0 <- read_html(webElem5txt.0) %>%
  html_table() %>% data.frame(.)

df <- df.table.0[,1:8, drop=FALSE] %>%
  filter(str_detect(序號, "^[A-Z][0-9].*"))
  #filter(!序號 %in% c('A. 關愛專站（無需預約）','B. 一般市民採樣站（需預約，不上碼及不提供紙本證明）','C. 自費採樣站（需預約，可上碼及提供紙本證明）'))
Sys.sleep(5)

df$Location <- read_html(webElem5txt.0) %>% html_nodes('div.hidden-xs') %>% html_text(trim = TRUE)
station <- df

#head(station)
#summary(station)

Sys.sleep(10)

write.csv(station,paste("D:/Documents/GitHub/ssm-nat-dp/data/aptmon/station-",format(Sys.time(), "%Y%m%d%H%M%S"),".csv", sep = ""), row.names = TRUE)

Sys.sleep(10)

# to close the browser
mybrowser$client$close()
# to close the server
mybrowser$server$stop()

system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
system("taskkill /im geckodriver.exe /f", intern=FALSE, ignore.stdout=FALSE)

#rm(mybrowser)
#kill = 'for /f "tokens=5" %a in (\'netstat -aon ^| findstr ":4548" ^| findstr "LISTENING"\') do taskkill /f /pid %a'
#shell(kill, ignore.stderr = TRUE, ignore.stdout = TRUE)
