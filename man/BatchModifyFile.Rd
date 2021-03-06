% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModifyFilesWithSameName.R
\name{BatchModifyFile}
\alias{BatchModifyFile}
\title{Modify files in batch mode with fixed file name}
\usage{
BatchModifyFile(
  Dir,
  FileName,
  KeyWordOldLocation,
  keyWordOld,
  keyWordNew,
  Replace = FALSE,
  SaveOld = TRUE
)
}
\arguments{
\item{Dir}{The directory where you target files exist.}

\item{FileName}{complete file name.}

\item{KeyWordOldLocation}{Key word used to find target lines.}

\item{Replace}{Whether to replace the old file. Default is FALSE.}

\item{SaveOld}{Wheter to save the old file, this will suffix date to the file name.}

\item{KeyWordOld}{The old word needs to be replaced.}

\item{KeyWordNew}{The new word.}
}
\value{
message info for the whole process
}
\description{
This function going to modify files in readline mode.
for example, when a package get update, you may need to change some parameters in your script to adapt to the new package version.
}
\examples{
## Attention! 'KeyWordOldLocation' should use "Escape Regex" in cases.
## Sometimes when seurat get update, we may need to modify some parameters in your script to keep it right or updated!
## examples bellow!

## replace ",coord.fixed = TRUE" with "" in all the app.R files under "./" directory.
## first try to see if the modify is right with Replace = FALSE.
BatchModifyFile("./","app.R","DimPlot\\\\(",",coord.fixed = TRUE","") # Check the 'KeyWordOldLocation'!
## if every thing is fine, then replace the old word and save file.
BatchModifyFile("./","app.R","DimPlot\\\\(",",coord.fixed = TRUE","",Replace = TRUE) # Take action!

## Another example:
BatchModifyFile("./","app.R","min.cutoff =",",min.cutoff = \"q9\"","") # Check the 'KeyWordOldLocation'! and the replacement!
BatchModifyFile("./","app.R","min.cutoff =",",min.cutoff = \"q9\"","",Replace = TRUE) # Take action!

}
