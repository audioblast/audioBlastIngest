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

A sound is conveyed on a taxon (an OTU), a specimen, a field observation or a
collecting event. Specimens and field observations are read as Darwin Core
records, which give both the taxon of the accepted determination and where and
when the recording was made: its date, coordinates, country, locality and
recordist. A collecting event is read through the Darwin Core records of its
occurrences, which say what was recorded only where they all agree. Records,
events and OTUs are each read once however many recordings share them.

The sound itself gives the id, audio URL, title, attribution, upload date,
duration and sample rate. The MIME type and size come from one HEAD request for
the audio, which is never downloaded; audio that cannot be reached leaves both
empty rather than failing the harvest. An attribution label such as
`(c)2020. Created by Holger Braun. License: CC BY 4.0` is read as the author,
the rights holder and the licence separately. Time of day, recording device and
number of channels are not in the API, and `info_url` is empty because the
Orthoptera Species File has no page for a sound.

Where an indirect link gives no taxon, a binomial that is the whole title or
precedes a numbered recording label is matched to a unique accepted OSF taxon.
The Orthoptera Species File does not identify those recordings, so the name is
**not** put in `taxon`, which holds the scientific name the source gives. It is
given as a link saying the identification was read from the title, qualified
`identificationBasis#RecordingTitle`, so that a reader can tell it from the
identifications OSF makes. A recording of more than one taxon is given links
too, as a scientific name is one name. The links are `attr(x, "links")`, which
`ingestR()` uploads as the harvesting source's own.

The default project token is the Orthoptera Species File's. It is not a
credential: <https://sfg.taxonworks.org/api/v1/> needs no authentication and
publishes the token of every open TaxonWorks project. Override it with `token`
if the site's token changes.

To include this harvest in `ingestR()`, the external `list_sources` configuration
must supply a recordings module such as:

```json
{"type":"recordings","orthoptera":{"per_page":100,"pause":1},"process":["sourceR"]}
```

Network and server failures are retried; a failed harvest warns and skips this
source without uploading partial results. Offline tests use public API
responses recorded on 2026-09-20: a page of sounds, and the sounds conveyed on
a specimen, on field observations and on collecting events.
