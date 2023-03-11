library(rstudioapi)

#' @title NREL API
#' @description Returns user's NREL API key. If an NREL key is not found,
#' prompts user to add it to session variables or to create a key from
#' \link{https://developer.nrel.gov/signup/} and then add to session variables.
#' @examples \dontrun{
#' getNrelKey()
#' }
#' @export
getNrelKey <- function() {
  checkRenv()
  nrelKey <- Sys.getenv("NREL_KEY")
  if (nrelKey == "") {
    cat("No NREL API key found, please enter a selection to continue:\n\n1. Enter an API key\n2. Create an API key\n3: Create and save API key to .Renviron file (Don't need to re-enter key after restarting session)\n4. Add an Nrel API key to .Renviron\n5: Refresh Environment\n\nSelection: ")
    sel <- readline()
    if (sel == "1") {
      Sys.setenv("NREL_KEY" = rstudioapi::askForSecret(name = "NREL API Key"))
      out <- Sys.getenv("NREL_KEY")
    } else if (sel == "2") {
      browseURL("https://developer.nrel.gov/signup/")
      out <- ""
    } else if (sel == "3") {
      browseURL("https://developer.nrel.gov/signup/")
      tky <- rstudioapi::askForSecret(name = "NREL API Key")
      if (length(tky)>0){
        writeLines(paste0("NREL_KEY=", tky), ".Renviron")
      }
      out <- tky
    }  else if (sel == "4") {
      tky <- rstudioapi::askForSecret(name = "NREL API Key")
      if (length(tky)>0){
        writeLines(paste0("NREL_KEY=", tky), ".Renviron")
      }
      out <- tky
    } else if (sel == "5") {
      readRenviron(paste0(getwd(), "/.Renviron"))
      tky <- Sys.getenv("NREL_KEY")

      if (tky == "") {
        getNrelKey()
      }
      out <- tky
    } else {
      cat("Not a valid selection -- quitting. --")
      return("")
    }
  } else {
      out <- Sys.getenv("NREL_KEY")
  }
  Sys.setenv("NREL_KEY" = out)
  out
}

#' @title NREL API
#' @description Remove your NREL API key from system variables
#' @param perm If \code{TRUE} removes your NREL API key from your \code{.Renviron} file. Default is \code{FALSE}
#' @examples \dontrun{
#' # Remove from session only
#' removeNrelKey()
#'
#' # Remove from `.Renviron`
#' removeNrelKey(perm = TRUE)
#' }
#' @export
removeNrelKey <- function(perm = FALSE) {
  if (perm) {
    renvf <- readLines(paste0(getwd(), "/.Renviron"))
    renvf <- renvf[-grep("^NREL_KEY", aaa)]
    unlink(paste0(getwd(),"/",".Renviron"))
    writeLines(aaa[-grep("^NREL_KEY", aaa)], ".Renviron")
    readRenviron(paste0(getwd(), "/.Renviron"))
  }

  Sys.setenv("NREL_KEY" = "")
}

#' @title Check .gitignore for .Renviron
#' @description Check if `.Renviron` file is in `.gitignore`. If it
#' isn't, prompts user to add to `.gitignore` file not make sure api keys/other
#' secrets don't be exposed.
#' @examples \dontrun{
#' checkRenv()
#' }
#' @export
checkRenv <- function() {
  gitignore_path <- paste0(getwd(), "/.gitignore")
  if (!file.exists(gitignore_path)) {
    cat(".gitignore file not found!")
    return(FALSE)
  }
  con <- file(gitignore_path, open = "r")
  gitignore_lines <- readLines(con)
  if (!".Renviron" %in% gitignore_lines) {
    cat(".Renviron file not in .gitignore file, do you want to add it?\nThis will ensure any secret keys you might add to it remain secret.\n\n2: Yes\n1: No\n\nSelection:")
    chck <- readline()
    if (chck == "1") {
      cat("Doing nothing!\n\n")
      return(FALSE)
    } else if (chck == "2") {
      tryCatch(
        {
          cat(".Renviron", file = gitignore_path, append = TRUE)
          cat("\n", file = gitignore_path, append = TRUE)
          close(con)
        },
        error = function(e) {
          cat("Could not write to .gitignore file!")
          return(FALSE)
        }
      )
      cat("Done!\n\n")
      return(TRUE)
    } else {
      cat("Not a valid selection -- quitting!")
      return(FALSE)
    }
  }
  return(TRUE)
}

#' @title NREL API
#' @description Query the NREL vehicle API. Returns the dataset requested from
#' NREL (for a list of possible dat sets call \code{getNrelArgs()}). Requires
#' an NREL API key to use, calling \code{queryNrel()} will help walk you through
#' setting up the right credentials.
#' @param dataset The Nrel dataset to return, call \code{getNrelArgs()} for a
#' list of datasets. For help setting up API credentials call \code{queryNrel()}
#' @examples \dontrun{
#' # Get NREL fuels
#' queryNrel("fuels")
#'
#' # Get all vehicles
#' queryNrel("vehicles")
#'
#' # Set up credentials
#' queryNrel()
#' }
#' @export
queryNrel <- function(dataset = NULL, ...) {

  check <- TRUE
  if (is.null(dataset)) {
    cat("Please enter a NREL Dataset, here is a list")

    msg <- paste0(
      "Please pick a dataset to query from NREL."
    )
    cat(msg)
    check <- FALSE
    print(getNrelArgs())
  }

  if (check) {
    nrel_key <- getNrelKey()

    trycsv <- tryCatch({
      url <- paste0(
        "https://developer.nrel.gov/api/vehicles/v1/",
        dataset,
        ".csv?api_key=",
        nrel_key
      )
      readr::read_csv(url)
    },
    error = function(e) {
      NA
    }
    )
    tryjson <- tryCatch({
      url <- paste0(
        "https://developer.nrel.gov/api/vehicles/v1/",
        dataset,
        ".json?api_key=",
        nrel_key
      )
      jsonlite::read_json(url)
    },
    error = function(e) {
      NA
    }
    )
    if (any(!is.na(trycsv))) {
      return(trycsv)
    } else if (any(!is.na(tryjson))) {
      return(tryjson)
    } else {
      cat("No dataset found\n")
      return(FALSE)
    }
  }

}

#' @title NREL API
#' @description Get a list of possible datasets to query from NREL
#' @examples \dontrun{
#' getNrelArgs()
#' }
#' @export
getNrelArgs <- function() {

  # Sys.setenv("NREL_ARGS_CHECKED" = "CHECKED")
  nrelsiteYaml <- yaml::read_yaml("https://developer.nrel.gov/docs/transportation/vehicles-v1/spec.yml")

  possArgs0 <- utilityR::grextall(names(nrelsiteYaml$paths), "v1\\/[a-zA-Z\\_\\/\\{\\}]+\\.", simplify = TRUE)
  possArgs1 <- gsub("v1\\/|\\.", "", possArgs0)
  possible <- lapply(names(nrelsiteYaml$paths),
                     function(x){
                       data.frame(
                         `Summary` = nrelsiteYaml$paths[[x]]$get$summary
                       )
                     }
  )

  out <- do.call(rbind.data.frame, possible)
  out$`APIArgs` <- possArgs1
  return(htmlTable::htmlTable(out[,c(2,1)], , caption = "NREL API Parameters  (Full documentation can be found <a href = 'https://developer.nrel.gov/docs/transportation/vehicles-v1/'>here</a>)", align = "left"))
}
