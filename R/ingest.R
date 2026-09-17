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
    } else {
      #Sources are UTF-8. Without declaring it, R sessions that are not UTF-8
      #(e.g. Windows R < 4.2) double-encode non-ASCII text on upload.
      data <- read.csv(source$url, colClasses = "character", encoding = "UTF-8")
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

    #Recordings sources set up before lat and lon were added have 15 columns
    if (source$type == "recordings" && ncol(data) == 15) {
      data$lat <- rep_len("", nrow(data))
      data$lon <- rep_len("", nrow(data))
    }

    colnames(data) <- names(getHeaders(source$type))

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
  }

  #Upload
  if (!is.null(db)) {
    uploadTraits(db, seperatoR(traits))
    if (nrow(recordings) > 0) {
      recordings <- recordings[recordings$id != "",]
      uploadRecordings(db, recordings)
    }
    #uploadTaxa(db, taxonomiseR(taxa))
    if (nrow(deployments) > 0) {
      uploadDeployments(db, deployments)
    }
    if (nrow(annOmate) > 0) {
      uploadAnnOmate(db, annOmate)
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
    heads <-   col_names <- c("source", "id","Title","taxon","file","author","post_date","size","size_raw","type","NonSpecimen","Date","Time","Duration", "deployment", "lat", "lon")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "traits") {
    heads <-   col_names <- c("source","traitID","taxonID","Taxonomic.name","Trait","Ontology.Link","Value","Call.Type","Sex","Temperature","Reference","Cascade","Annotation.ID")
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
}
