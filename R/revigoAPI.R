library(httr)
library(jsonlite)
library(stringi)

#' ReviGO output
#'
#' Run the Revigo algorithm on a set of GO terms
#' @param data The GO terms summarise; Can either be a single collum table with just the GO terms or a two collum table with the GO terms and the p.value
#' @param namespace GO terms category; 1 - BP, 2 - CC, 3 - MF
#' @param Cutoff <- propotion of terms to keep
#' @param speciesTaxon <- Species taxon n (9606 is human)
#' @param measure <- Semantic similarity measure 
#' @return List of GO terms after Revigo
#' @export
revigo <- function(data, namespace = '1', cutoff = 0.7, speciesTaxon = "9606", measure = "SIMREL"){
  temp_file_name <- "tempRevigo.csv"
  i <- 0
  while (file.exists(temp_file_name)){
    temp_file_name <- paste0(temp_file_name, "_", i)
    i <- i +1 
  }
  write.table(file = temp_file_name , data, quote = F, col.names = F, row.names = F)
  userData <- readChar(temp_file_name ,file.info(temp_file_name )$size)
  unlink(temp_file_name)
  
  
  httr::POST(
    url = "http://revigo.irb.hr/StartJob.aspx",
    body = list(
      cutoff = cutoff,
      speciesTaxon = speciesTaxon,
      measure = measure,
      goList = userData
    ),
    # application/x-www-form-urlencoded
    encode = "form"
  ) -> res
  
  dat <- httr::content(res, encoding = "UTF-8")
  
  jobid <- jsonlite::fromJSON(dat,bigint_as_char=TRUE)$jobid
  
  
  running <- "1"
  while (running != "0" ) {
    httr::POST(
      url = "http://revigo.irb.hr/QueryJobStatus.aspx",
      query = list( jobid = jobid )
    ) -> res2
    dat2 <- httr::content(res2, encoding = "UTF-8")
    running <- jsonlite::fromJSON(dat2)$running
    Sys.sleep(1)
  }
  
  # Fetch results
  httr::POST(
    url = "http://revigo.irb.hr/ExportJob.aspx",
    query = list(
      jobid = jobid, 
      namespace = namespace,
      type = "csvtable"
    )
  ) -> res3
  
  dat3 <- httr::content(res3, encoding = "UTF-8")
  
  # Write results to a file
  dat3 <- stringi::stri_replace_all_fixed(dat3, "\r", "")
  
  
  cat(dat3, file = temp_file_name, fill = FALSE, sep = ",")
  results <- read.csv(file = temp_file_name, sep =",")
  unlink(temp_file_name)
  

  results$Eliminated <- str_trim(results$Eliminated)
  results$Value <- str_trim(results$Value)
  return (results)
}
