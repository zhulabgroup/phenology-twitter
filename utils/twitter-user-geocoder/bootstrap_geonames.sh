wget http://download.geonames.org/export/zip/US.zip
mv US.zip zip_US.zip
unzip zip_US.zip
mv US.txt zip_US.txt
rm -rf readme.txt
wget http://download.geonames.org/export/dump/US.zip
unzip US.zip
rm -rf readme.txt
wget http://download.geonames.org/export/dump/allCountries.zip
unzip allCountries.zip
rm -rf readme.txt
wget http://download.geonames.org/export/dump/cities1000.zip
unzip cities1000.zip
rm readme.txt
wget http://download.geonames.org/export/dump/cities5000.zip
unzip cities5000.zip
rm readme.txt
wget http://download.geonames.org/export/dump/cities15000.zip
unzip cities15000.zip
rm readme.txt

python3 process_geonames_us.py # this breaks all the time, so I run it manually
# rm *.zip
# rm *.txt
