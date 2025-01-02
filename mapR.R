library(geojsonio)
library(ggplot2)
library(sf)
library(stringr)
library(dplyr)
library(tidyr)
library(glue)
library(htmltools)
library(plotly)
library(htmlwidgets)
spdf <- geojson_read('moscow_region.geojson', what = "sp")
fortified <- fortify(spdf, region = "shapeISO")
fortified <- st_as_sf(spdf)

ggplot()+
  geom_sf(data = fortified)+
  geom_point(data = coordinates, aes(x = Longitude, y = Latitude), size = 0.6, color = "#74b560")

mosobl = results[results$region == 'Московская область', ]
coordinates$X = coordinates$X + 1

mosobl = mosobl %>% 
  mutate(num = stringr::str_remove(uik, 'УИК №'))

newcoord = merge(coordinates, mosobl, by.x = 'X', by.y = 'num')

ggplot()+
  geom_sf(data = fortified)+
  geom_point(data = newcoord, aes(x = Longitude, y = Latitude, color = votecol))+
  coord_sf(
    xlim = c(37, 38), ylim = c(55.6, 56),
    expand = FALSE)

newcoord$votecol = ifelse(newcoord$putinvote >= 90, 1,
                          ifelse(newcoord$putinvote >= 85, 2, 
                                 ifelse(newcoord$putinvote >= 80, 3), 4), 5)
newcoord$votecol = ifelse(newcoord$putinvote >= 90, 1, ifelse(newcoord$putinvote >= 85, 2, ifelse(newcoord$putinvote >= 80, 3, 4)))
newcoord$votecol = factor(newcoord$votecol, levels = c(1, 2, 3, 4), labels = c('>= 90%', '85-90%', '80-85%', '<80%'))

class(unique(newcoord$putinvote)[1])
names(newcoord)[names(newcoord) == "Путин.Владимир.Владимирович...."] <- "putinvote"

hist(newcoord$Даванков.Владислав.Андреевич....)

head(mosobl$num)
plot(mosobl$num, mosobl$Путин.Владимир.Владимирович....)

spdf_donetsk <- geojson_read('https://raw.githubusercontent.com/EugeneBorshch/ukraine_geojson/master/UA_14_Donetska.geojson', what = "sp")
fortified_donetsk <- st_as_sf(spdf_donetsk)
spdf_luhansk <- geojson_read('https://raw.githubusercontent.com/EugeneBorshch/ukraine_geojson/master/UA_09_Luhanska.geojson' , what = "sp")
fortified_luhansk <- st_as_sf(spdf_luhansk)

spdf_zaporizka <- geojson_read('https://raw.githubusercontent.com/EugeneBorshch/ukraine_geojson/master/UA_23_Zaporizka.geojson', what = "sp")
fortified_zaporizka <- st_as_sf(spdf_zaporizka)
spdf_kherson <- geojson_read('https://raw.githubusercontent.com/EugeneBorshch/ukraine_geojson/master/UA_65_Khersonska.geojson', what = "sp")
fortified_kherson <- st_as_sf(spdf_kherson)

newfortif = bind_rows(fortified_luhansk, fortified_donetsk, fortified_zaporizka, fortified_kherson)
  
ggplot()+
  geom_sf(data = newfortif)

dfresnewreg = results %>% 
  filter(region %in% c('Донецкая Народная Республика', 'Луганская Народная Республика', 'Херсонская область', 'Запорожская область')) %>% 
  mutate(num = stringr::str_remove(uik, 'УИК №'))

results = results %>% 
  mutate(num = stringr::str_remove(uik, 'УИК №')) %>% 
  drop_na('Путин.Владимир.Владимирович....')

for (i in unique(results$region)) {
  png(file=glue("plot_{i}.png"),
      width=1280, height=720)
  plot(results[results$region == i,]$num, results[results$region == i,]$Путин.Владимир.Владимирович....)  # getting there
  dev.off()
}

resmsk = data.frame(sort(results[results$region == 'город Москва',]$Путин.Владимир.Владимирович....))

spdf <- geojson_read('russia.geojson', what = "sp")
spdf <- geojson_read('Russia_regions.geojson', what = 'sp')
fortified <- st_as_sf(spdf)
coords = coords %>%
  select(x,y) %>% 
  drop_na(x, y) %>% 
  mutate(x = as.numeric(x), y = as.numeric(y))



ggplot()+
  geom_sf(data = fortified)+
  geom_point(data = uiks, aes(x=lon, y=lat), size = 0.05)

results2 = results %>% 
  mutate(num = stringr::str_remove(uik, 'УИК №'))

uik2 = merge(uiks, codesregions, by.x = 'subjCode', by.y = 'Код')
results3  = merge(uik2, results2, by.x = c('Наименование', 'uiknum'), by.y = c('region', 'num'))
results4 = results3[!duplicated(results3$vrn), ]
write.csv(results4, 'resultsuik.csv',fileEncoding = "cp1251")

ggplot()+
  geom_sf(data = fortified)+
  geom_point(data = results4, aes(x=lon, y=lat), size = 0.05)+
  coord_sf(
    xlim = c(30, 60),
    expand = FALSE
  )

setdiff(codesregions$Код, uiks$subjCode)

#No data in 1  2  3  4  5  6  7  8  9, 41, 59, 75, 89
spdf <- geojson_read('Russia_regions.geojson', what = 'sp')
fortified <- st_as_sf(spdf)
dopregion <- read.csv("C:/Users/manin/Downloads/dopregion.csv")
results <- read.delim("C:/Users/manin/Downloads/Telegram Desktop/results-uik-20240320T0352UTC.tsv")
uiks <- read.csv("C:/Users/manin/Downloads/uiks (1).csv", sep=";")
codesregions <- read.csv("C:/Users/manin/Desktop/codesregions.csv", sep=";")

dopregion2 = dopregion %>% 
  mutate(uiknum = stringr::str_remove(name, 'УИК №'))
dopregion2

uiks2 = rbind(uiks, dopregion2)
uiks3 = merge(uiks2, codesregions, by.x = 'subjCode', by.y = 'Код')
results2 = results %>% 
  mutate(num = stringr::str_remove(uik, 'УИК №'))
results3  = merge(uiks3, results2, by.x = c('Наименование', 'uiknum'), by.y = c('region', 'num'))
results4 = results3[!duplicated(results3$descr), ]
results4.new = results3[seq(1, nrow(results3), 2), ]
results4.new$votecol = ifelse(results4.new$Путин.Владимир.Владимирович.... >= 90, 1, ifelse(results4.new$Путин.Владимир.Владимирович.... >= 85, 2, ifelse(results4.new$Путин.Владимир.Владимирович.... >= 80, 3, 4)))
results4.new$votecol = factor(results4.new$votecol, levels = c(1, 2, 3, 4), labels = c('>= 90%', '85-90%', '80-85%', '<80%'))
results4.new$voteputin = results4.new$Путин.Владимир.Владимирович....
write.csv(results4.new, 'resultsuik_new.csv',fileEncoding = "cp1251")

ggsf = ggplot()+
  geom_sf(data = fortified)+
  geom_point(data = results4.new, aes(x = lon, y = lat, col = votecol, text = paste0("Результат\nВ. Путина: ", voteputin, "\nНомер УИК: ", uiknum, "\nРегион: ", Наименование)), size = 0.05, alpha = 0.6)+
  labs(title = 'Результаты президентских\nвыборов в России, 2024 г.',
       x = "",
       y = "",
       col ='Результат\nВ. Путина')+
  coord_sf(xlim = c(20, 185), ylim = c(40, 83)) 

ggsf2 = ggplot()+
  geom_sf(data = fortified)+
  geom_jitter(data = results4.new, aes(x = lon, y = lat, col = votecol, text = paste0("Результат\nВ. Путина: ", voteputin, "\nНомер УИК: ", uiknum, "\nРегион: ", Наименование)), size = 0.05, alpha = 0.6)+
  labs(title = 'Результаты президентских\nвыборов в России, 2024 г.',
       x = "",
       y = "",
       col ='Результат\nВ. Путина')+
  coord_sf(xlim = c(20, 185), ylim = c(40, 83)) 

ggsf2

crop_factor <- st_bbox(c(xmin = 0, 
                         xmax = 170, 
                         ymax = 40, 
                         ymin = 80),
                       crs = st_crs(fortified))

oz_cropped <- st_crop(fortified, crop_factor)

ggsf2 = ggplot()+
  geom_sf(data = oz_cropped)+
  geom_point(data = results4.new, aes(x = lon, y = lat, col = votecol, text = paste0("Результат\nВ. Путина: ", voteputin, "\nНомер УИК: ", uiknum, "\nРегион: ", Наименование)), size = 0.05, alpha = 0.6)+
  labs(title = 'Результаты президентских\nвыборов в России, 2024 г.',
       x = "",
       y = "",
       col ='Результат\nВ. Путина')
ggsf2

#  geom_point(data = results4.new, aes(x = lon, y = lat, text = paste0(glue("Результат В. Путина: {voteputin} \n Номер УИК: {uiknum} \n Регион: {Наименование}")), size = 0.05, alpha = 0.6))
#  geom_point(data = results4.new, aes(x = lon, y = lat, col = votecol, text = paste("УИК", uiknum)), size = 0.05, alpha = 0.6)
plot <- ggplotly(ggsf, tooltip = 'text')
plot
saveWidget(partial_bundle(ggplotly(ggsf)), file = "myplot2.html")
#position = "jitter", tooltip = 'text'

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}





recent_turnout <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/european_turnout.csv",stringsAsFactors = FALSE)
recent_turnout$region <- factor(recent_turnout$region, levels=c("British","Northern","Western","Mediterranean","Central/Eastern"))

p <- recent_turnout %>%
  ggplot(aes(x=nat_turnout,y=euro_turnout)) + 
  geom_point(aes(size=population, colour=region, text=paste("country:", country)), alpha=0.4) +
  geom_text(aes(size=population/3.5, label=abbreviation), colour="gray20", alpha=1) +
  labs(title = "Recent turnout in European Union countries",
       x = "Latest legislative or presidential election (whichever had higher turnout)",
       y = "May 2019 European Parliament election")
fig <- ggplotly(p)

fig
