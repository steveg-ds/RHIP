# RHIP

This R project contains all the code used in the thesis "Rurality and Robustness: Rural Areas and Housing Insecurity Risk" 

In the final crunch to finish my thesis all semblance of order went out the door. I am working to get it all organized and cleaned up as soon as possible but the code does get the job done. I am also going to set up the option to run the code in a docker container to avoid some potential installation hassles. 

How it works: 

ACS Data Collection.R

Accesses the Census API and collects processes American Community Survey (ACS) data. It verifies and sets the Census API key, and reads FIPS codes from a CSV file. It then collects the needed ACS variables for the year 2019. Finally, the data is saved as a CSV file for further analysis.
