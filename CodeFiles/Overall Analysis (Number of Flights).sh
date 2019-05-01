#Creating table with the daily number of flights(Done in hive):
hive -e "SELECT Year, Month, DayofMonth, count (DayofMonth) FROM flights GROUP BY Year, Month, DayofMonth" > count_day.csv

#Creating table with the daily number of flights for Carriers(done in hive):
hive -e "SELECT Year, Month, DayofMonth, UniqueCarrier,  count (UniqueCarrier) FROM flights GROUP BY Year, Month, DayofMonth, UniqueCarrier" > count_carrier_day.csv

#Creating table with the Year, Origin, Dest, Carrier
hive -e "SELECT Year,Origin, Dest, Carrier FROM flights" > OriDesCar.csv
