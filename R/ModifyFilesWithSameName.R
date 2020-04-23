#' Modify files in batch mode with fixed file name
#'
#' This function going to modify files in readline mode.
#' for example, when a package get update, you may need to change some parameters in your script to adapt to the new package version.
#'
#' @param Dir The directory where you target files exist.
#' @param FileName complete file name.
#' @param KeyWordOldLocation Key word used to find target lines.
#' @param KeyWordOld The old word needs to be replaced.
#' @param KeyWordNew The new word.
#' @param Replace Whether to replace the old file. Default is FALSE.
#' @param SaveOld Wheter to save the old file, this will suffix date to the file name.
#'
#' @return message info for the whole process
#' @export
#'
#' @examples
#' ## Attention! 'KeyWordOldLocation' should use "Escape Regex" in cases.
#' ## Sometimes when seurat get update, we may need to modify some parameters in your script to keep it right or updated!
#' ## examples bellow!
#'
#' ## replace ",coord.fixed = TRUE" with "" in all the app.R files under "./" directory.
#' ## first try to see if the modify is right with Replace = FALSE.
#' BatchModifyFile("./","app.R","DimPlot\\(",",coord.fixed = TRUE","") # Check the 'KeyWordOldLocation'!
#' ## if every thing is fine, then replace the old word and save file.
#' BatchModifyFile("./","app.R","DimPlot\\(",",coord.fixed = TRUE","",Replace = TRUE) # Take action!
#'
#' ## Another example:
#' BatchModifyFile("./","app.R","min.cutoff =",",min.cutoff = \"q9\"","") # Check the 'KeyWordOldLocation'! and the replacement!
#' BatchModifyFile("./","app.R","min.cutoff =",",min.cutoff = \"q9\"","",Replace = TRUE) # Take action!
#'
BatchModifyFile <- function(Dir, FileName, KeyWordOldLocation, keyWordOld, keyWordNew, Replace = FALSE, SaveOld = TRUE){
  ## check the prameters
  if ( !dir.exists(Dir)) {
    stop("Error, Please cheack the if the Dir exist!")
  }

  all.dirs <- list.dirs(Dir)
  file.find.index <- TRUE
  for (poss.dir in all.dirs) {
    if( FileName %in% list.files(poss.dir)){
      file.find.index <- FALSE
      message(paste("# Found",FileName,"in",poss.dir))
      message(paste("Checking file:",poss.dir,"/",FileName,sep = ""))
      message(rep("-",100))
      ModifyAndBackup(Dir = poss.dir,FileName = FileName,KeyWordOldLocation = KeyWordOldLocation,
                      KeyWordOld = keyWordOld, KeyWordNew = keyWordNew, Replace = Replace, SaveOld = SaveOld)
      message(paste("Checking for file:",poss.dir,"/",FileName," finished",sep = ""))
      message(paste(rep("#",100),collapse = ""))
    }
  }
  if (file.find.index) {
    message(paste("No files are found with file name",FileName,sep="-"))
  } else if(Replace){
    message("Congratulations! All files are modified successfully!")
  } else {
    message("Checking finished!")
  }
}

#' Modify a specified file with fixed file name
#'
#' @param Dir The directory where you target files exist.
#' @param FileName complete file name.
#' @param KeyWordOldLocation Key word used to find target lines.
#' @param KeyWordOld The old word needs to be replaced.
#' @param KeyWordNew The new word.
#' @param Replace Whether to replace the old file. Default is FALSE.
#' @param SaveOld Wheter to save the old file, this will suffix date to the file name.
#'
#' @return
#' @export
#'
#' @examples
ModifyAndBackup <- function(Dir,FileName,KeyWordOldLocation,KeyWordOld,KeyWordNew,Replace = TRUE, SaveOld = TRUE){
  file.create(paste(Dir,"tmp.file",sep = "/"))
  new <- file(paste(Dir,"tmp.file",sep = "/"),"w")
  con <- file(paste(Dir,FileName,sep = "/"), "r")
  line=readLines(con,n=1)
  index <- TRUE
  while( length(line) != 0 ) {
    if( grepl(KeyWordOldLocation,line)){
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
    file.remove(paste(Dir,"tmp.file",sep = "/")) }
  else if (Replace) {
    if (SaveOld) {
      file.rename(paste(Dir,FileName,sep = "/"),paste(Dir,"/",FileName,".",format(Sys.time(), "%Y-%b-%d-%H:%M:%S"),sep = ""))
      file.rename(paste(Dir,"tmp.file",sep = "/"),paste(Dir,FileName,sep = "/"))
    } else {
      file.remove(paste(Dir,FileName,sep = "/"))
      file.rename(paste(Dir,"tmp.file",sep = "/"),paste(Dir,FileName,sep = "/"))
    }

  }
  else { file.remove(paste(Dir,"tmp.file",sep = "/"))}
}
