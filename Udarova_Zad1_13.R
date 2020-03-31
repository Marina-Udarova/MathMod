##### ЗАДАНИЕ 1 #####

# Ударова Марина
# Для региона 4 рассчитайте урожайность пшеницы в 2017 году, 
#взяв для рассчета средние суммы активных температур за предыдущие 
#7 лет, с 21 ближайших метеостанций

# Республика Алтай
# Столица - Горно-Алтайск - 51.958182, 85.960373

rm(list=ls())

library(tidyverse)
library(rnoaa)
library(lubridate)

#station_data = ghcnd_stations()
#write.csv(station_data, file = "station_data.csv")

# Загрузка списока всех метеостанций
station_data = read.csv("station_data.csv")

# Получим список метеостанций ближайших к столице,
#создав таблицу с именем региона и координатами его столицы
gorno_altaysk = data.frame(id = "gorno_altaysk", latitude = 51.958182, longitude = 85.960373)

# Найдем метеостанции, соответствующие критериям
gorno_altaysk_around = meteo_nearby_stations(lat_lon_df = gorno_altaysk, station_data = station_data,
                                             limit = 21, var = c("PRCP", "TAVG"),
                                             year_min = 2010, year_max = 2017)
gorno_altaysk_around

# Первым элементом таблицы будет идентификатор метеостанции Горно-Алтайска, 
#его то мы и попытаемся получить
gorno_altaysk_id = gorno_altaysk_around[["gorno_altaysk"]][["id"]][1]
summary(gorno_altaysk_id)
gorno_altaysk_id

# чтобы получить таблицу всех метеостанций вокруг Горно-Алтайска
#нужно выбрать целиком первый объект из списка
gorno_altaysk_table = gorno_altaysk_around[[1]]
summary(gorno_altaysk_table)
# в таблице gorno_altaysk_table оказалось 21 объект, ранжированных по расстоянию от Горно-Алтайска

# Для получения всех данных с 1 метеостанции, зная ее идентификатор, 
#используйем след. команду
all_gorno_altaysk_data = meteo_tidy_ghcnd(stationid = gorno_altaysk_id)
all_gorno_altaysk_data

#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()
#Создадим объект, куда скачаем все данные всех метеостанций
all_altaysk_meteodata = data.frame()

#Цикл для всех метеостанций
for(i in 1:21) 
{ 
  print(i)
  #выберем нужные свойства 
  all_i = meteo_tidy_ghcnd(stationid = gorno_altaysk_table$id[i])
  all_i = all_i[,c("id", "date", "tmax", "tmin")]
  
  #с помощью команды rbind соединяем данные, полученные на предыдущих и данном этапах цикла
  all_altaysk_meteodata = rbind(all_altaysk_meteodata, all_i)
}

# добавим колонку со средней температурой
  all_altaysk_meteodata = mutate(all_altaysk_meteodata, 
                      tavg = (all_altaysk_meteodata$tmax + all_altaysk_meteodata$tmin)/2)

  ### Записываем полученные результаты ###
  write.csv(all_altaysk_meteodata,"all_altaysk_meteodata.csv")

  
## Cчитываем данные из файла all_altaysk_meteodata.csv
all_altaysk_meteodata = read.csv("all_altaysk_meteodata.csv")

#посмотрим на данные
str(all_altaysk_meteodata)

# Добавим год, месяц, день
all_altaysk_meteodata = mutate(all_altaysk_meteodata, year = year(date), 
                               month = month(date), day = day(date))
#проверим результат
str(all_altaysk_meteodata)

#отфильтруем данные за 2010-2017 года
years_altaysk_meteodata =filter(all_altaysk_meteodata, year %in% c(2010:2017))
#проверим результат
str(years_altaysk_meteodata)
summary(years_altaysk_meteodata)

### Средняя (по годам и метеостанциям) сумма активных температур за месяц ###

# Изучаем формулу и видим, что нужно расчитать сумму температур больше 5 град. по месячно, 
#остальное в формуле - константы

# Разделим температуру на 10, чтобы привести в нормальный вид
years_altaysk_meteodata[,"tavg"] = years_altaysk_meteodata$tavg/10
summary (years_altaysk_meteodata)

# Превратим в нули все NA и где tavg<5 
years_altaysk_meteodata[is.na(years_altaysk_meteodata$tavg),"tavg"] = 0
years_altaysk_meteodata[years_altaysk_meteodata$tavg<5, "tavg"] = 0

#проверяем, что температура получилась или 0, или больше 5 градусов
summary(years_altaysk_meteodata)

# Расчитаем суммарную температуру за месяц за 7 лет для всех станций 
# группируем по метеостанциям, годам и месяцам
#??group_by
alldays = group_by(years_altaysk_meteodata,id,year,month)
#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum
sumT_alldays_altaysk = summarize(alldays, tsum = sum(tavg))
# максимальная суммарная температура за месяц 686,25, то есть 686,25/30=22,9, что достаточно разумно
summary(sumT_alldays_altaysk) 

# Сгруппируем данные по месяцам  
groups_altaysk_months = group_by(sumT_alldays_altaysk,month)
groups_altaysk_months
# найдем для всех метеостанций и ВСЕХ лет среднее по месяцам
sumT_months = summarize(groups_altaysk_months, St = mean(tsum))
sumT_months

## Подготовка к расчету по формуле Урожая ##
### Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000) # константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000) # константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000) # отношение числа дней i-го месяца, 
                                                                                #входящих в период вегетации культуры, к общему 
                                                                                #числу дней в месяце,константа по табл. 1.
y = 1.0 # Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf = 300 # Коэффициент использования ФАР посевом 
Qj = 1600 # калорийность урожая культуры 
Lj = 2.2 # сумма частей основной и побочной продукции 
Ej = 25 # стандартная влажность культуры 
# Рассчитаем Fi по месяца
sumT_months = mutate(sumT_months, Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))

##  Расчитываем урожай как сумму по месяцам ##
  Yield = sum(sumT_months$Yi);  Yield


