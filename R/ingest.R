#' Ingest sources
#'
#' Ingest data sources for audioBlast!
#'
#' @param db Database connection
#' @param verbose If TRUE says more about what's going on.
#' @export
#' @importFrom utils read.csv
ingestR <- function(db=NULL, verbose=FALSE) {
  sources <- getSources()

  #Get header files
  taxa <- getHeaders("taxa")
  traits <- getHeaders("traits")
  recordings <- getHeaders("recordings")
  deployments <- getHeaders("deployments")
  annOmate <- getHeaders("ann-o-mate")
  references <- getHeaders("references")
  links <- getHeaders("links")
  specimens <- getHeaders("specimens")
  details <- getHeaders("details")
  locations <- getHeaders("locations")
  descriptions <- getHeaders("descriptions")
  vernacular <- getHeaders("vernacularnames")
  onomatopoeia <- getHeaders("onomatopoeia")
  images <- getHeaders("images")

  for (i in 1:length(sources)) {
    source <- sources[[i]]

    if (verbose) print(paste("Source:", source$name))
    if (is.element("git", names(source))) {
      command <- paste0(
        "git -C \"",
        source$git$repo,
        "\" lfs pull || git clone https://github.com/",
        source$git$owner,"/",source$git$repo,".git")
      system(command)
      source$url <- paste0(source$git$repo,"/",source$git$file)
    }
    if (is.element("xenocanto", names(source))) {
      #A failed harvest skips this source rather than every source
      data <- tryCatch(
        xenocantoR(source$xenocanto$query, verbose=verbose),
        error=function(e) {
          warning(paste("Skipping source", source$name, "-", conditionMessage(e)))
          NULL
        })
      if (is.null(data)) next
    } else if (is.element("inaturalist", names(source))) {
      #A failed harvest skips this source rather than every source. Each taxon
      #group is a source of its own, so one that fails doesn't take the others
      #with it.
      data <- tryCatch(
        inaturalistR(source$inaturalist$taxon_id, verbose=verbose),
        error=function(e) {
          warning(paste("Skipping source", source$name, "-", conditionMessage(e)))
          NULL
        })
      if (is.null(data)) next
    } else if (is.element("orthoptera", names(source))) {
      #A failed harvest skips this source rather than every source
      data <- tryCatch(
        do.call(orthopteraSpeciesFileR, c(source$orthoptera, list(verbose=verbose))),
        error=function(e) {
          warning(paste("Skipping source", source$name, "-", conditionMessage(e)))
          NULL
        })
      if (is.null(data)) next
    } else if (source$type == "references") {
      #References are BibTeX (.bib) or else CSV. One that cannot be read skips
      #this source rather than every source.
      read <- if (grepl("\\.bib$", source$url, ignore.case=TRUE)) bibtexR else referencesR
      data <- tryCatch(
        read(source$url),
        error=function(e) {
          warning(paste("Skipping source", source$name, "-", conditionMessage(e)))
          NULL
        })
      if (is.null(data)) next
    } else {
      #Sources are UTF-8. Without declaring it, R sessions that are not UTF-8
      #(e.g. Windows R < 4.2) double-encode non-ASCII text on upload.
      data <- read.csv(source$url, colClasses = "character", encoding = "UTF-8")
    }

    #A harvest can give links as well as records, for what a record holds that
    #its own table has no column for. They are the harvesting source's links,
    #and are taken here because adding the source column drops attributes.
    harvested <- attr(data, "links")
    attr(data, "links") <- NULL
    if (!is.null(harvested) && nrow(harvested) > 0) {
      harvested$source <- source$name
      links <- rbind(links, harvested)
    }

    #Map source columns to standard columns (defined in module.php)
    if (is.element("mapping", names(source)) || is.element("override", names(source))) {
      data <- colmap(source, data)
    }

    if (length(source$process) > 0) {
      for (j in 1:length(source$process)) {
        if (source$process[[j]] == "sourceR") {
          data <- sourceR(source$name, data)
        }
        if (source$process[[j]] == "date2dateAndTime") {
          data <- date2dateAndTime(data)
        }
        if (source$process[[j]] == "hz2khz") {
          data <- hz2khz(data)
        }
      }
    }

    #Sources that don't give the columns at the end of their table are given
    #them empty: recordings lat and lon, then time_of_day, license, info_url
    #and device; traits Call.Part, Call.Type.Link and Call.Qualifier, then min
    #and max; descriptions topic_link; onomatopoeia kind_link; links reference.
    headers <- names(getHeaders(source$type))
    if (source$type %in% c("recordings", "traits", "descriptions", "onomatopoeia", "links") &&
        ncol(data) < length(headers)) {
      for (column in headers[-seq_len(ncol(data))]) {
        data[[column]] <- rep_len("", nrow(data))
      }
    }

    colnames(data) <- headers

    if (source$type == "taxa") {
      if (verbose) print(paste("  type: taxa"))
      taxa <- rbind(taxa, data)
    }
    if (source$type == "recordings") {
      if (verbose) print(paste("  type: recordings"))
      recordings <- rbind(recordings, data)
    }
    if (source$type == "traits") {
      if (verbose) print(paste("  type: traits"))
      traits <- rbind(traits, data)
    }
    if (source$type == "deployments") {
      if (verbose) print(paste("  type: deployments"))
      deployments <- rbind(deployments, data)
    }
    if (source$type == "ann-o-mate") {
      if (verbose) print(paste("  type: annOmate"))
      annOmate <- rbind(annOmate, data)
    }
    if (source$type == "references") {
      if (verbose) print(paste("  type: references"))
      references <- rbind(references, data)
    }
    if (source$type == "links") {
      if (verbose) print(paste("  type: links"))
      links <- rbind(links, data)
    }
    if (source$type == "specimens") {
      if (verbose) print(paste("  type: specimens"))
      specimens <- rbind(specimens, data)
    }
    if (source$type == "details") {
      if (verbose) print(paste("  type: details"))
      details <- rbind(details, data)
    }
    if (source$type == "locations") {
      if (verbose) print(paste("  type: locations"))
      locations <- rbind(locations, data)
    }
    if (source$type == "descriptions") {
      if (verbose) print(paste("  type: descriptions"))
      descriptions <- rbind(descriptions, data)
    }
    if (source$type == "vernacularnames") {
      if (verbose) print(paste("  type: vernacularnames"))
      vernacular <- rbind(vernacular, data)
    }
    if (source$type == "onomatopoeia") {
      if (verbose) print(paste("  type: onomatopoeia"))
      onomatopoeia <- rbind(onomatopoeia, data)
    }
    if (source$type == "images") {
      if (verbose) print(paste("  type: images"))
      images <- rbind(images, data)
    }
  }

  #Upload
  if (!is.null(db)) {
    #Traits are linked to the terms of the vocabulary at vocab.audioblast.org,
    #and uploaded unlinked if it can't be read
    if (nrow(traits) > 0) {
      traits <- tryCatch(linkTraits(traits), error=function(e) {
        warning(paste("Traits not linked to vocab.audioblast.org -", conditionMessage(e)))
        traits
      })
    }
    uploadTraits(db, seperatoR(traits))
    if (nrow(recordings) > 0) {
      recordings <- recordings[recordings$id != "",]
      uploadRecordings(db, recordings)
    }
    if (nrow(taxa) > 0) {
      uploadTaxa(db, taxonomiseR(taxa))
    }
    if (nrow(deployments) > 0) {
      uploadDeployments(db, deployments)
    }
    if (nrow(annOmate) > 0) {
      uploadAnnOmate(db, annOmate)
    }
    if (nrow(references) > 0) {
      uploadReferences(db, references)
    }
    if (nrow(locations) > 0) {
      uploadLocations(db, locations)
    }
    if (nrow(specimens) > 0) {
      uploadSpecimens(db, specimens)
    }
    if (nrow(descriptions) > 0) {
      uploadDescriptions(db, descriptions)
    }
    if (nrow(links) > 0) {
      uploadLinks(db, links)
    }
    if (nrow(details) > 0) {
      uploadDetails(db, details)
    }
    if (nrow(vernacular) > 0) {
      uploadVernacularNames(db, vernacular)
    }
    if (nrow(onomatopoeia) > 0) {
      uploadOnomatopoeia(db, onomatopoeia)
    }
    if (nrow(images) > 0) {
      uploadImages(db, images)
    }

  }
}

#' Get sources from audioBlast! API
#'
#' Uses the audioBlast! API to get a list of data sources
#'
#' @export
#' @importFrom rjson fromJSON
getSources <- function() {
  json_data <- fromJSON(file="http://api.audioblast.org/standalone/modules/list_sources/")
  sources <- list()
  for (i in 1:length(json_data$data)) {
    source <- names(json_data$data)[[i]]
    for (j in 1: length(json_data$data[[i]])) {
      row <- list(c(name=source, json_data$data[[i]][[j]]))
      sources <- c(row, sources)
    }
  }
  return(sources)
}

getHeaders <- function(type) {
  if (type == "taxa") {
    heads <-   col_names <- c("source", "id","taxon","Unit name 1","Unit name 2","Unit name 3","Unit name 4","Rank","parent_id","parent_taxon")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "deployments") {
    heads <-   col_names <- c("source","id","name","lat", "lon")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "recordings") {
    #author is who made the recording, and lat, lon, country and locality are
    #where it was made, which is not always where a specimen was collected
    heads <-   col_names <- c("source", "id","Title","taxon","file","author","post_date","size","size_raw","type","NonSpecimen","Date","Time","Duration", "deployment", "lat", "lon", "time_of_day", "license", "info_url", "device", "rights_holder", "country", "locality", "sample_rate", "channels")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "specimens") {
    #The specimens and observations that recordings are of, in columns named
    #after the Darwin Core terms they hold
    heads <- c("source","id","scientificName","basisOfRecord","institutionCode","collectionCode","catalogNumber","otherCatalogNumbers","typeStatus","sex","lifeStage","individualCount","recordedBy","eventDate","identifiedBy","dateIdentified","identificationQualifier","associatedSequences","locality","countryCode","decimalLatitude","decimalLongitude","occurrenceRemarks","info_url")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "descriptions") {
    #What a source says about something in prose, such as how a taxon behaves.
    #topic_link is the Species Profile Model info item the topic names, which
    #normaliseDescriptions() reads; a source needn't give it. What a description
    #is about, and the references it rests on, are links.
    heads <- c("source","id","topic","value","info_url","topic_link")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "details") {
    #What a record holds that has no column of its own: a name and a value,
    #with a unit where it is measured. type and id are the record's, and a
    #record's values of one name are numbered by delta.
    heads <- c("source","type","id","name","delta","value","unit")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "locations") {
    #The places records were made or collected, described once however many
    #records share them, in columns named after the Darwin Core terms they hold
    heads <- c("source","id","name","continent","countryCode","stateProvince","county","island","islandGroup","locality","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters","geodeticDatum","georeferenceRemarks","minimumElevationInMeters","maximumElevationInMeters","info_url")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "traits") {
    #min and max are the ends of the range a value is written as, which
    #seperatoR() reads; a source needn't give them
    heads <-   col_names <- c("source","traitID","taxonID","Taxonomic.name","Trait","Ontology.Link","Value","Call.Type","Sex","Temperature","Reference","Cascade","Annotation.ID","Call.Part","Call.Type.Link","Call.Qualifier","min","max")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "ann-o-mate") {
    heads <-   col_names <- c("source","source_id","annotator","annotation_id","annotation_date","annotation_info_url","recording_url","recording_info_url","time_start","time_end","taxon","type","lat","lon","contact")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "references") {
    #type is the BibTeX entry type (e.g. article), type_of_work its type field
    #and type_name the source's own name for the type (e.g. Journal Article)
    heads <- c("source","id","type","title","author","editor","year","month","journal","booktitle","series","howpublished","volume","number","pages","chapter","edition","publisher","organization","institution","school","address","type_of_work","note","isbn","issn","doi","url","attachments","keywords","abstract","type_name","journal_abbreviation","pmid","info_url")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "vernacularnames") {
    #The names a taxon is known by, in columns named after the Darwin Core
    #terms they hold. The taxon a name is for, and the reference it was taken
    #from, are links.
    heads <- c("source","id","vernacularName","language","locality","remarks")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "images") {
    #The images a source holds, each with the licence it is under, so that an
    #image is published with the terms it may be used on rather than as a bare
    #URL. subtype is what kind of image it is in the source's own words, such as
    #a photograph or a scanning electron micrograph. An image that several
    #records share is one image here, and what an image shows is links.
    heads <- c("source","id","title","file","subtype","creator","license","post_date","type","size_raw","width","height","caption")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "onomatopoeia") {
    #The words a human language renders an animal's sound with, such as bow-wow
    #for a dog barking. kind is what sort of rendering it is in the source's own
    #words, and kind_link is the class that names, which
    #normaliseOnomatopoeia() reads; a source needn't give it. The taxon whose
    #sound a rendering renders, and the reference it was taken from, are links.
    heads <- c("source","id","word","kind","language","sex","lifeStage","locality","remarks","info_url","kind_link")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "links") {
    #source is the source giving the link; a link's id is made by uploadLinks().
    #reference is the reference that established the link, which uploadLinks()
    #makes a link of rather than a column; a source needn't give one.
    heads <- c("source","subject_type","subject_source","subject_id","predicate","object_type","object_source","object_id","qualifier","remarks","reference")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
}
