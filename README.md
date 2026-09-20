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

Harvesting from xeno-canto requires a [xeno-canto API key](https://xeno-canto.org/account) in the `XC_API_KEY` environment variable, e.g. set in `.Renviron`. Without it the xeno-canto source is skipped.

Harvest recordings from [Orthoptera Species File](https://orthoptera.speciesfile.org/about)
using its public TaxonWorks API (no personal API key required):

```R
recordings <- sourceR("orthoptera-speciesfile", orthopteraSpeciesFileR())
uploadRecordings(db, recordings)
```

The importer pages through sounds and resolves directly linked taxa and the
accepted identifications of linked specimens and field occurrences, caching
lookups. Collecting events resolve through their specimen and field-occurrence
records only when all records are identified and agree on one taxon. When an
indirect link remains unresolved, a binomial that is the whole recording title
or precedes a numbered recording label is matched exactly to a unique accepted
OSF taxon. Ambiguous or unverified matches remain blank.
It preserves sound IDs, audio URLs, titles, attribution labels, upload
dates and durations. Recording dates, coordinates, licences and MIME types are
left empty because the API does not supply them as structured sound fields.
Multiple linked taxa are separated by semicolons. Missing audio is omitted;
audio files are not downloaded. The default project token is the public token
published in the site's configuration and can be overridden with `token`.

To include this harvest in `ingestR()`, the external `list_sources` configuration
must supply a recordings module such as:

```json
{"type":"recordings","orthoptera":{"per_page":100,"pause":1},"process":["sourceR"]}
```

Network/server failures are retried; a failed harvest warns and skips this
source without uploading partial results. Offline tests use public API fixtures,
including all four indirectly linked recordings, retrieved on 2026-09-19.
