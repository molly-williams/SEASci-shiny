

whales<- read_csv("sp_obis_westcoast.csv")

whalesdf <- as.data.frame(whales)

whalesdf$vernacularName <- as.factor(whalesdf$vernacularName)
class(whalesdf$vernacularName)

new <- whalesdf %>% 
  filter(vernacularName == "Blue Whale"| vernacularName =="Gray Whale"| vernacularName =="Humpback Whale") %>% 
  filter(DecimalLatitude > 32 | DecimalLatitude < 39) %>% 
  filter(DecimalLongitude < -116 | DecimalLongitude > -124) %>% 
  rename(lat = DecimalLatitude) %>% 
  rename(lon = DecimalLongitude)
  

ymd_hms(new$EventDate) 
new$EventDate<- year(new$EventDate)

na.omit(new)


