#Creating table with the number of flights for destinations(done in hive):
hive -e "SELECT Year, Dest, count (Dest) FROM flights GROUP BY Year, Dest" > dest.csv
