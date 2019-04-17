cd project # project is the location of the files

sed -i 's/"//g' airports.csv  #delete the double quotation marks in the airports. csv 

hive  

CREATE TABLE flights (Year INT, Month INT, DayofMonth INT,  DayOfWeek INT, DepTime INT, CRSDepTime INT, ArrTime INT, CRSArrTime INT, UniqueCarrier STRING, FlightNum INT, TailNum STRING, ActualElapsedTime INT, CRSElapsedTime INT, AirTime INT, ArrDelay INT, DepDelay INT, Origin STRING, Dest STRING, Distance INT, TaxiIn INT, TaxiOut INT,Cancelled INT, Diverted INT)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ',';

LOAD DATA LOCAL INPATH 'groupprojectcl.csv'
OVERWRITE INTO TABLE flights;
 ALTER TABLE flights set tblproperties("skip.header.line.count"="1");

CREATE TABLE airports (iata STRING, airport STRING, city STRING, state STRING, country STRING, lat DOUBLE, long DOUBLE)
ROW FORMAT DELIMITED
  FIELDS TERMINATED BY ',';
  
LOAD DATA LOCAL INPATH 'airports.csv'
OVERWRITE INTO TABLE airports;
 ALTER TABLE airports set tblproperties("skip.header.line.count"="1");

CREATE TABLE m
AS
SELECT flights.*, airports.airport as OrigAirport, airports.city as OrigCity, airports.state as OrigState, airports.lat as OrigLat, airports.long as OrigLong 
FROM flights 
JOIN airports ON flights.Origin=airports.iata;

quit; #quit hive

hive -e "SELECT m.*, airports.airport as DestAirport, airports.city as DestCity, airports.state as DestState, airports.lat as DestLat, airports.long as DestLong FROM m JOIN airports ON m.Dest=airports.iata" > flights_airport.csv

