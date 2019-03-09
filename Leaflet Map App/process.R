

whales<- read_csv("sp_obis_westcoast.csv")

whalesdf <- as.data.frame(whales)

whalesdf$vernacularName <- as.factor(whalesdf$vernacularName)
class(whalesdf$vernacularName)

new<- whalesdf %>% 
  filter(vernacularName == "Blue Whale"| vernacularName =="Gray Whale"| vernacularName =="Humpback Whale") %>% 
  rename(lat = DecimalLatitude) %>% 
  rename(lon = DecimalLongitude)

ymd_hms(new$EventDate) 
new$EventDate<- year(new$EventDate)


saveRDS(new, "./new.rds")
