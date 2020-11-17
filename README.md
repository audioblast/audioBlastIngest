[![Build Status](https://travis-ci.org/audioblast/audioBlastIngest.svg?branch=master)](https://travis-ci.org/audioblast/audioBlastIngest) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/2e19e5beab02477ea9a51f335e9b7b86)](https://app.codacy.com/gh/audioblast/audioBlastIngest?utm_source=github.com&utm_medium=referral&utm_content=audioblast/audioBlastIngest&utm_campaign=Badge_Grade)
 [![Coverage Status](https://coveralls.io/repos/github/audioblast/audioBlastIngest/badge.svg?branch=master)](https://coveralls.io/github/audioblast/audioBlastIngest?branch=master)

# audioBlastIngest
This R package provides the ingest functionality for the audioBLAST! project. This includes harvesting of data from external sources, processing of these data sets, and upload of the processed data to audioBLAST!.

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
