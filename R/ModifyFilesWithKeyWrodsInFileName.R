#' Extended Batch-Modify-File Function
#'
#' More pararmeters included in this function, Details see the Examples!
#'
#' @param Dir The directory where you target files exist.
#' @param FileNameKeyWords key words for filenames.
#' @param FileNamepattern filename pattern, such as "*.Rmd",defaul is NULL.
#' @param FileName.ignore.case whether to ignore case of filename key words for searching files. default is FALSE.
#' @param KeyWordOldLocation key words used to find target line, could be a character vector.
#' @param Location.ignore.case whether to ignore case of line key words for searching files. default is FALSE.
#' @param keyWordOld The old word needs to be replaced.
#' @param keyWordNew The new word.
#' @param Replace Whether to replace the old file. Default is FALSE.
#' @param SaveOld Wheter to save the old file, this will suffix date to the file name.
#'
#' @return
#' @export
#'
#' @examples
#' # Better not use "../../" or "./" in Dir! for it probably will cause mistakes!
#' BatchModifyFileExtended("./man/",
#'                         FileNameKeyWords = c("a","m"),
#'                         FileNamepattern = NULL,
#'                         FileName.ignore.case = TRUE,
#'                         KeyWordOldLocation = c("a","b"),
#'                         Location.ignore.case = FALSE,
#'                         keyWordOld = "roxygen2",
#'                         keyWordNew = "####",
#'                         Replace = FALSE,
#'                         SaveOld = TRUE)
BatchModifyFileExtended <- function(Dir,
                                    FileNameKeyWords,
                                    FileNamepattern = NULL,
                                    FileName.ignore.case = FALSE,
                                    KeyWordOldLocation,
                                    Location.ignore.case = FALSE,
                                    keyWordOld,
                                    keyWordNew,
                                    Replace = FALSE,
                                    SaveOld = TRUE){
  ## check the prameters
  if ( !dir.exists(Dir)) {
    stop("Error, Please cheack the if the Dir exist!")
  }
  message("Using: <<",paste(FileNameKeyWords,collapse = ","),">> as key words to search for files!")
  ## search files with keywords
  filesFound <- FindFiles(Dir = Dir,
                          FileNameKeyWords = FileNameKeyWords,
                          ignore.case = FileName.ignore.case,
                          pattern = FileNamepattern)
  if (is.null(filesFound)) {
    stop("No files are found! Please check the key words used for seraching files!")
  }
  message(paste(rep("#",100),collapse = ""))
  message("1. Target files are found:")
  for (i in 1:length(filesFound)) {
    message(paste(">>",i,filesFound[i], "<<", sep =" "))
  }
  ## Try to replace key words in files
  message(paste(rep("#",100),collapse = ""))
  message("2. Trying to find target line and replace old word with new!")
  for (poss.files in filesFound) {
    message(paste("<",which(filesFound == poss.files),">","Checking file:", poss.files, sep = " "))
    message(rep("-",100))
    ModifyAndBackupFile(poss.files = poss.files,
                        KeyWordOldLocation = KeyWordOldLocation,
                        ignore.case = Location.ignore.case,
                        KeyWordOld = keyWordOld,
                        KeyWordNew = keyWordNew,
                        Replace = Replace,
                        SaveOld = SaveOld)
    message(paste("Checking for file:", poss.files, "finished",sep = " "))
    message(paste(rep("#",100),collapse = ""))
    if(Replace){
      if (SaveOld) {
        message("Congratulations! All files are modified successfully! And Old file have been backed-up!")
      }else {
        message("Congratulations! All files are modified successfully!")
      }
    } else {
      message("Checking finished!")
    }
  }
}

#' Search files with filename include specified key words
#'
#' @param Dir The directory where you target files exist.
#' @param FileNameKeyWords key words for filenames.
#' @param ignore.case whether to ignore case of filename key words for searching files. default is FALSE.
#' @param pattern  filename pattern, such as "*.Rmd",defaul is NULL.
#'
#' @return
#' @export
#'
#' @examples
FindFiles <- function(Dir,
                      FileNameKeyWords,
                      ignore.case,
                      pattern){
  all.dirs <- list.dirs(Dir)
  FilePaths <- c()

  for (poss.dir in all.dirs) {
    # Only list files without dirs - poss.files - not succesfully set!02
    #poss.dirs <- list.dirs(poss.dir,full.names = FALSE,recursive = FALSE)
    poss.files <- list.files(poss.dir,pattern = pattern)
    #poss.files <- poss.files[!poss.files %in% poss.dirs]

    if (length(poss.files) == 0) {
      break()
    }
    for (keyword in FileNameKeyWords) {
      poss.files <- grep(keyword, poss.files, value = TRUE, ignore.case = ignore.case)
      if (length(poss.files) == 0) {
        break()
      }
    }
    if (length(poss.files) != 0) {
      FilePaths <- append(FilePaths,paste(poss.dir,poss.files,sep = "/"))
    }
  }
  return(FilePaths)
}

#' Modify And Backup File
#'
#' @param poss.files The target file.
#' @param KeyWordOldLocation key words used to find target line, could be a character vector.
#' @param ignore.case whether to ignore case of line key words for searching files. default is FALSE.
#' @param keyWordOld The old word needs to be replaced.
#' @param keyWordNew The new word.
#' @param Replace Whether to replace the old file. Default is FALSE.
#' @param SaveOld Wheter to save the old file, this will suffix date to the file name.
#'
#' @return
#' @export
#'
#' @examples
ModifyAndBackupFile <- function(poss.files,
                                KeyWordOldLocation,
                                ignore.case,
                                KeyWordOld,
                                KeyWordNew,
                                Replace = TRUE,
                                SaveOld = TRUE){
  Dir <- gsub(gsub(".*/","",poss.files),"",poss.files)
  file.create(paste(Dir,"tmp.file",sep = ""))
  new <- file(paste(Dir,"tmp.file",sep = ""),"w")
  con <- file(poss.files, "r")
  line=readLines(con,n=1)
  index <- TRUE
  while( length(line) != 0 ) {
    ## Search lines with specified key words
    LineIndex <- TRUE
    for (keyword in KeyWordOldLocation) {
      LineIndex <- LineIndex & grepl(keyword,line, ignore.case = ignore.case)
    }
    if( LineIndex ){
      index <- FALSE
      message("### Found target line:")
      message(paste(">>",line,sep = ""))
      if (grepl(KeyWordOld,line)) {
        message("### Key Word Old found!")
        message("### Try to replace with new word:")
        message(paste(">>",gsub(KeyWordOld,KeyWordNew,line),sep = ""))
        writeLines(gsub(KeyWordOld,KeyWordNew,line),new)
      }else{
        message("### Key Word Old not found!")
        writeLines(line,new)
      }
      message(rep("-",100))
    }else{
      writeLines(line,new)
    }
    line=readLines(con,n=1) }
  close(con)
  close(new)
  if (index) {
    message(rep("-",100))
    message("### Target line are Not found in this file!")
    file.remove(paste(Dir,"tmp.file",sep = "")) }
  else if (Replace) {
    if (SaveOld) {
      file.rename(poss.files,paste(poss.files,".",format(Sys.time(), "%Y-%b-%d-%H:%M:%S"),sep = ""))
      file.rename(paste(Dir,"tmp.file",sep = ""),poss.files)
    } else {
      file.remove(poss.files)
      file.rename(paste(Dir,"tmp.file",sep = ""),poss.files)
    }

  }
  else { file.remove(paste(Dir,"tmp.file",sep = ""))}
}
