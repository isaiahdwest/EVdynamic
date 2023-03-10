library(rstudioapi)

getNrelKey <- function() {
  checkRenv()
  nrelKey <- Sys.getenv("NREL_KEY")
  if (nrelKey == "") {
    cat("No NREL API key found, please enter a selection to continue:\n\n1. Enter an API key\n2. Create an API key\n3: Create and save API key to .Renviron file (Don't need to re-enter key after restarting session)\n4. Add an Nrel API key to .Renviron\n\nSelection: ")
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

checkRenv <- function() {
  gitignore_path <- paste0(getwd(), "/.gitignore")
  if (!file.exists(gitignore_path)) {
    cat(".gitignore file not found!")
    return(FALSE)
  }
  gitignore_lines <- readLines(gitignore_path)
  if (!".Renviron" %in% gitignore_lines) {
    cat(".Renviron file not in .gitignore file, do you want to add it?\nThis will ensure any secret keys you might add to it remain secret.\n\n2: Yes\n1: No\n\nSelection:")
    chck <- readline()
    if (chck == "1") {
      cat("Doing nothing!\n\n")
      return(FALSE)
    } else if (chck == "2") {
      tryCatch(
        {
          cat(".Renviron", "\n", file = gitignore_path, append = TRUE)
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

