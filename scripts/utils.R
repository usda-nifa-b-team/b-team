library(googledrive)
library(data.table)
library(stringr)

timedRead <- function (toread) {
  start <- Sys.time()
  frame <- read.csv(toread)
  end <- Sys.time()
  cat("Read ", nrow(frame), " rows from ", toread, " in ", (end - start), "s")
  frame
}

timedFread <- function (toread) {
  start <- Sys.time()
  frame <- data.table::fread(toread)
  end <- Sys.time()
  cat("Read ", nrow(frame), " rows from ", toread, " in ", (end - start), "s")
  # Otherwise traditional R indexing notation fails
  as.data.frame(frame)
}

timedWrite <- function (x, towrite) {
  start <- Sys.time()
  write.csv(x, towrite, na = "", row.names = FALSE)
  end <- Sys.time()
  cat("Written ", nrow(x), " rows to ", towrite, " in ", (end - start), "s")
}

downloadGdrive <- function (id, file_path) {
    if (!file.exists(file_path)) {
        drive_download(as_id(id), path = file_path, overwrite = FALSE)
    }
}

# Adapted from https://stackoverflow.com/a/64687628 - only downloads single folders
downloadGdriveFolder <- function (id, file_path) {
  if (!file.exists(file_path)) {
    dir.create(file_path)
    # folder link to id
    jp_folder = str_glue("https://drive.google.com/drive/folders/{id}")
    folder_id = drive_get(as_id(jp_folder))
    
    # find files in folder
    files = drive_ls(folder_id)
    
    cat("Fetching ", nrow(files), " files in folder ", folder_id$name)
    
    # loop dirs and download files inside them
    for (i in seq_along(files$name)) {
      # If there were subfolders, this would list them:
      # i_dir = drive_ls(files[i, ])
      
      target <- str_c(file_path, "/", files$name[i])
      
      try({
        drive_download(as_id(files$id[i]), path = target)
      })
    }
  } else {
    cat ("Path ", file_path, " already exists, skipping download")
  }
}
