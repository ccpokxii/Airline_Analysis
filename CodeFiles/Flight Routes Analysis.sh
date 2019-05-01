#Creating table with the number airline routes:
hive -e "SELECT Year,Origin, Dest FROM flights" > route.csv

#Creating table with the number of flights for destinations(done in hive):
hive -e "SELECT Year, Dest, count (Dest) FROM flights GROUP BY Year, Dest" > dest.csv
