library(httr)
library(rvest)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(xml2)
library(gsubfn)
library(writexl)

#the link from the discover life website after the species search on the map with coordinates
link = "https://www.discoverlife.org/mp/20m?kind=Trigonisca+vitrifrons"

#this supposed to be labeled as species but I'm afraid to change it
genera = "Trigonisca vitrifrons"

discover <- read_html(link)

#got the url
list_sighting <- discover %>%
  html_elements(css = "a") %>% 
  html_attr("href")
#print(list_sighting)

latlong <- discover %>%
  html_elements("img") %>% 
  html_attr("alt")
#view(latlong)

#finally works
maybe_data <- do.call(rbind,
              lapply(1:length(latlong),
               function(i)
                 data.frame(A=unlist(list_sighting[i]),
                            B=unlist(latlong[i]))))

View(maybe_data)

bees <- maybe_data %>%
  filter(str_detect(A, 'BEES'))
#View(bees)

#create column with full URL
bees$URL <- paste('https://www.discoverlife.org', bees$A, sep='')
#View(bees)

#clean up coordinates
bees$coor <- sub(genera, "", bees$B)
bees$coor2 <- sub("--.*", "", bees$coor)
bees$coor2 <- gsub("^\\s+|\\s+$","", bees$coor2)
#View(bees)

bees2 <- subset(bees, select = c(URL, coor2))
#View(bees2)

#separate Lat and Long
bees2 <- bees2 %>% separate(coor2, c('Lat', 'Long'), " ")
#View(bees2)

bees2 <- bees2 %>% separate(Lat, into = c('NumLat', 'DirLat'), sep = -1, convert = TRUE)
bees2 <- bees2 %>% separate(Long, into = c('NumLong', 'DirLong'), sep = -1, convert = TRUE)
#View(bees2)

#get rid of the decimal
bees2$NumLat <- gsub("°", "", bees2$NumLat)
#print(bees2$NumLat)
bees2$NumLong <- gsub("°", "", bees2$NumLong)
#print(bees2$NumLong)
#double check numeric
bees2$NumLat <- as.numeric(bees2$NumLat)
bees2$NumLong <- as.numeric(bees2$NumLong)
#print(bees2$NumLat)


#typeof(NumLat)
URL <- bees2$URL
NumLat <- bees2$NumLat
DirLat <- bees2$DirLat
NumLong <- bees2$NumLong
DirLong <- bees2$DirLong
#print(NumLat)
#print(DirLat)

#convert S and W to negative
NumLat <- mapply(function(num, dir) {
  num <- as.numeric(num)
  ifelse(dir == "N", num, -1*num)
}, NumLat, DirLat)

NumLong <- mapply(function(num, dir) {
  num <- as.numeric(num)
  ifelse(dir == "E", num, -1*num)
}, NumLong, DirLong)

#print(NumLat)
#print(NumLong)


bees3 <- data.frame(URL, NumLat, NumLong)
#View(bees3)

write_xlsx(bees3, "C://Users//Biljana//Documents//Bees//bee.xlsx")


#count if not BEES, ??


