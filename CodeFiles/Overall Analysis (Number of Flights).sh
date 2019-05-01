# Downloading the necessary data (done in terminal)
wget https://raw.githubusercontent.com/coatless/stat490uiuc/master/airlines/airlines_data.sh
chmod u+x airlines_data.sh
./airlines_data.sh 2002 2002
mv airlines.csv groupproject.csv
./airlines_data.sh 1998 1998
tail -n+2 airlines.csv >> groupproject.csv

# Deleting columns with all data missing
cut -d ',' -f 23,25,26,27,28,29 --complement groupproject.csv > groupprojectcl.csv



#Creating table with the daily number of flights(Done in hive):
hive -e "SELECT Year, Month, DayofMonth, count (DayofMonth) FROM flights GROUP BY Year, Month, DayofMonth" > count_day.csv

#Creating table with the daily number of flights for Carriers(done in hive):
hive -e "SELECT Year, Month, DayofMonth, UniqueCarrier,  count (UniqueCarrier) FROM flights GROUP BY Year, Month, DayofMonth, UniqueCarrier" > count_carrier_day.csv

#Creating table with the Year, Origin, Dest, Carrier
hive -e "SELECT Year,Origin, Dest, Carrier FROM flights" > OriDesCar.csv
