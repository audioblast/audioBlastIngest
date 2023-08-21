[![Build Status](https://travis-ci.org/audioblast/audioBlastIngest.svg?branch=master)](https://travis-ci.org/audioblast/audioBlastIngest)

# audioBlastIngest
This R package provides the ingest functionality for the audioBlast! project. This includes harvesting of data from external sources, processing of these data sets, and upload of the processed data to audioBlast!.

## Installation
````R
library(devtools)
install_github("audioblast/audioBlastIngest")
````

## Usage
The ingest function requires a DBI object.
````R
library(devtools)
library(RMariaDB)
library(audioBlastIngest)

db <- dbConnect(RMariaDB::MariaDB(), 
 user='audioblast_ingest', 
 password='password', 
 dbname='audioblast', 
 host='localhost', 
 port=3306)

ingestR(db)
````
