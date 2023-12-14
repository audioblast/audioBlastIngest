#' Ingest sources
#'
#' Ingest data sources for audioBlast!
#'
#' @param db Database connection
#' @param verbose If TRUE says more about what's going on.
#' @export
#' @importFrom utils read.csv
#' @importFrom DBI dbDisconnect
ingestR <- function(db=NULL, verbose=FALSE) {
  sources <- getSources()

  #Get header files
  taxa <- getHeaders("taxa")
  traits <- getHeaders("traits")
  recordings <- getHeaders("recordings")
  deployments <- getHeaders("deployments")
  devices <- getHeaders("devices")
  sensors <- getHeaders("sensors")
  abiotic <- getHeaders("abiotic")
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
    data <- read.csv(source$url, colClasses = "character")

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
      }
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
    if (source$type == "devices") {
      if (verbose) print(paste("  type: devices"))
      devices <- rbind(devices, data)
    }
    if (source$type == "sensors") {
      if (verbose) print(paste("  type: sensors"))
      sensors <- rbind(sensors, data)
    }
    if (source$type == "abiotic") {
      if (verbose) print(paste("  type: abiotic"))
      abiotic <- rbind(abiotic, data)
    }
    if (source$type == "ann-o-mate") {
      if (verbose) print(paste("  type: annOmate"))
      annOmate <- rbind(annOmate, data)
    }
  }

  #Upload
  if (!is.null(db)) {
    #uploadTraits(db, seperatoR(traits))
    if (nrow(recordings) > 0) {
      uploadRecordings(db, recordings)
    }
    #uploadTaxa(db, taxonomiseR(taxa))
    if (nrow(deployments) > 0) {
      uploadDeployments(db, deployments)
    }
    #uploadDevices(db, devices)
    #uploadSensors(db, sensors)
    #uploadAbiotic(db, abiotic)
    uploadAnnOmate(db, annOmate)

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
  if (type == "recordings") {
    heads <-   col_names <- c("source", "id","Title","taxon","file","author","post_date","size","size_raw","type","NonSpecimen","Date","Time","Duration", "deployment")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "traits") {
    heads <-   col_names <- c("source","traitID","taxonID","Taxonomic name","Trait","Ontology Link","Value","Call Type","Sex","Temperature","Reference","Cascade","Annotation ID")
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
  if (type == "devices") {
    heads <-   col_names <- c("source","id","name","model","serial","hardware","hardware_version","os", "os version","software","software_version","firmware","firmware_version")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "sensors") {
    heads <-   col_names <- c("source","id","device","name","model","serial","property")
    df <- data.frame(matrix(ncol=length(heads), nrow=0))
    colnames(df) <- heads
    return(df)
  }
  if (type == "abiotic") {
    heads <-   col_names <- c("source","id","deployment","timestamp","file_source","file_id","file_relative_time","property","value")
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
