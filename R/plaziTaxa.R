#The taxa a Plazi treatment names, as the classification it puts its taxon in.
#
#audioBlast! holds a taxon once for every source that knows it, and reconciles
#them by linking each row to the Catalogue of Life (see colR()), so Plazi's
#taxa are Plazi's own rather than an attempt at one agreed taxonomy. That
#matters here, because Plazi is a corpus of papers and not a taxonomy: its
#treatments disagree with each other. Mecopoda is in Tettigoniidae in one
#paper and in Phaneropteridae in another, and Chorthippus in Acrididae and in
#Baissogryllidae, so one row cannot hold both. A name keeps the classification
#that most of the treatments naming it give, and the harvest says how many it
#had to choose between.
#
#The classification comes from the attributes of the treated name rather than
#its printed text, which is the one place attributes are the better source: the
#printed text is a binomial, and only the attributes say what family or order
#it sits in. The species attribute is repaired where Plazi could not parse the
#name (see plaziName()).

#The ranks Plazi gives, from the top down. kingdom, phylum, class, order and
#family were in every treatment sampled, genus in 98% and species in 83%, with
#no chain that had a genus but no family, so the chain can be walked as it
#comes.
plaziRanks <- c("kingdom", "phylum", "class", "order", "family", "subfamily",
                "tribe", "genus", "subgenus", "species", "subspecies")

#The taxa one treatment names, from its kingdom down to the taxon it treats,
#each with the one above it as its parent. The last row is the treated taxon,
#which is what a description of that treatment is about.
#' @importFrom xml2 xml_attr xml_find_first
#' @importFrom stringr str_to_title
plaziTaxa <- function(document, treatment) {
  taxa <- getHeaders("taxa")
  name <- xml_find_first(document, "//subSubSection[@type='nomenclature']//taxonomicName")
  if (length(name) == 0 || is.na(name)) return(taxa)

  at <- function(rank) plaziValue(xml_attr(name, rank))
  parts <- vapply(plaziRanks, at, character(1))
  #Plazi reads open nomenclature as undefined (see plaziName()), and the
  #printed name is the only place the epithet survives
  parts[["species"]] <- plaziEpithet(parts[["species"]], document)
  #The ranks this treatment gives, in order. parts keeps every rank, empty or
  #not, because a species needs its genus whether or not there is a subgenus
  #between them.
  chain <- names(parts)[parts != ""]
  if (length(chain) == 0) return(taxa)

  parent <- ""
  for (rank in chain) {
    row <- plaziTaxon(rank, parts, parent)
    taxa[nrow(taxa) + 1, ] <- row
    parent <- row$id
  }
  rownames(taxa) <- NULL
  return(taxa)
}

#One row of a classification: what the taxon is called, the parts its name is
#made of, and the taxon above it.
plaziTaxon <- function(rank, parts, parent) {
  units <- rep_len("", 4)
  part <- function(name) {
    value <- parts[[name]]
    if (is.null(value) || is.na(value)) return("")
    return(value)
  }
  if (rank %in% c("species", "subspecies")) {
    #A name below the genus is written out in full, with a subgenus in
    #brackets as audioBlast! writes one
    units[1] <- part("genus")
    below <- c(part("subgenus"), part("species"),
               if (rank == "subspecies") part("subspecies"))
    below <- below[below != ""]
    units[seq_along(below) + 1] <- below
    written <- units[units != ""]
    #A subgenus is written in brackets, as audioBlast! writes one: Mus (Mus)
    #musculus rather than Mus Mus musculus
    if (part("subgenus") != "" && length(written) > 1) {
      written[2] <- paste0("(", written[2], ")")
    }
    taxon <- paste(written, collapse=" ")
  } else {
    units[1] <- part(rank)
    taxon <- part(rank)
  }
  return(data.frame(
    source="", id=taxon, taxon=taxon,
    `Unit name 1`=units[1], `Unit name 2`=units[2],
    `Unit name 3`=units[3], `Unit name 4`=units[4],
    Rank=str_to_title(rank), parent_id=parent,
    parent_taxon=parent, stringsAsFactors=FALSE, check.names=FALSE))
}

#The species epithet, taken from the printed name where Plazi could not read
#it. A treatment of Gryllus sp.4 is marked up species="undefined-4"; the
#printed text says "Gryllus sp.4", and the epithet is what follows the genus.
#' @importFrom xml2 xml_find_all xml_text
plaziEpithet <- function(epithet, document) {
  if (!grepl("undefined", epithet, fixed=TRUE)) return(epithet)
  name <- xml_find_first(document, "//subSubSection[@type='nomenclature']//taxonomicName")
  printed <- plaziText(paste(xml_text(xml_find_all(name, ".//text()")), collapse=" "))
  #The name as printed is the genus and what follows it, before any authority
  match <- regmatches(printed, regexec("^[A-Z][a-z]+[[:space:]]+([^([:space:]]+)", printed))[[1]]
  if (length(match) == 2) return(match[2])
  return(epithet)
}

#The taxa of a treatment that the harvest has not already written, so that a
#name is held once however many treatments name it.
#
#The first classification wins rather than the commonest, because a streamed
#harvest has let every earlier treatment go by the time it reads the next and
#cannot count them. Only the ids are kept, which is a few thousand strings for
#the whole corpus, so the two paths give the same answer.
#
#Plazi's treatments disagree about where a name belongs - Mecopoda is in
#Tettigoniidae in one paper and Phaneropteridae in another - so the ones that
#contradict what was written are counted and reported. That is a disagreement
#between papers rather than a fault in the reading, and audioBlast! reconciles
#taxa by linking them to the Catalogue of Life (see colR()) rather than by
#choosing between sources here.
plaziFreshTaxa <- function(taxa, seen) {
  if (nrow(taxa) == 0) return(taxa)
  fresh <- rep_len(TRUE, nrow(taxa))
  for (i in seq_len(nrow(taxa))) {
    id <- taxa$id[i]
    was <- seen[[id]]
    if (is.null(was)) {
      assign(id, taxa$parent_id[i], envir=seen)
    } else {
      fresh[i] <- FALSE
      if (!identical(was, taxa$parent_id[i])) {
        assign("..disagreed..", c(get0("..disagreed..", envir=seen, ifnotfound=character()), id),
               envir=seen)
      }
    }
  }
  taxa <- taxa[fresh, , drop=FALSE]
  rownames(taxa) <- NULL
  return(taxa)
}

#How many names the treatments disagreed about, for the harvest to report
plaziDisagreed <- function(seen) {
  return(length(unique(get0("..disagreed..", envir=seen, ifnotfound=character()))))
}
