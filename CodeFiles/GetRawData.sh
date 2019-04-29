# Downloading the necessary data (done in terminal)
wget https://raw.githubusercontent.com/coatless/stat490uiuc/master/airlines/airlines_data.sh
chmod u+x airlines_data.sh
./airlines_data.sh 2002 2002
mv airlines.csv groupproject.csv
./airlines_data.sh 1998 1998
tail -n+2 airlines.csv >> groupproject.csv


# Deleting columns with all data missing
cut -d ',' -f 23,25,26,27,28,29 --complement groupproject.csv > groupprojectcl.csv
