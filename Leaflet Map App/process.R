

whales<- read_csv("sp_obis_westcoast.csv")

whalesdf <- as.data.frame(whales)

whalesdf$vernacularName <- as.factor(whalesdf$vernacularName)
class(whalesdf$vernacularName)

whalesdf2 <- whalesdf %>% 
  filter(vernacularName == "Blue Whale"| vernacularName =="Gray Whale"| vernacularName =="Humpback Whale") %>% 
  filter(DecimalLatitude > 32 | DecimalLatitude < 39) %>% 
  filter(DecimalLongitude < -116 | DecimalLongitude > -124) %>%
  rename(lat = DecimalLatitude) %>% 
  rename(lon = DecimalLongitude)
  
# Isolate month and year from observation time stamp:

whalesdf2$date_simple <- as.Date(new$EventDate, format="%Y/%M/%D")
whalesdf2$year <- format(as.Date(new$EventDate, format="%Y/%M/%d"),"%Y")
whalesdf2$month <- format(as.Date(new$date_simple, format="%Y/%m/%d"),"%m")

new <- whalesdf2 %>% 
  filter(year > 1970)

ymd_hms(new$EventDate) 
new$EventDate<- year(new$EventDate)

na.omit(new)


