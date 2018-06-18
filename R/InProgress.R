# To-Do, see if we can improve and document this function
extractComments <- function(FileName, max=9, fromLine=1, ToLine=-1, tab='\t') {
  libinstandload('stringr')
  file <- readLines(FileName, n=ToLine)
  file <- str_extract(file[fromLine:length(file)], paste0('(##[0-',max,'].*)|(#{2,',max+1,'}[^#0-9]+)'))
  file <- file[!is.na(file)]
  for(n in 9:1) {
    file <- gsub(pattern=paste0(paste0(rep('#', times=n+1), collapse=''), '[^#0-9]'),
                 replacement=paste0('##',n),
                 x=file)
  }
  nrs <- as.numeric(substring(str_extract(file, '##[0-9]'),3,4))-1
  file <- paste0(sapply(nrs, function(n) {paste0(rep(tab, times=n), collapse='')}),'- ',gsub('##[0-9][[:space:]]*','',file))
  file <- gsub('\\\\t',tab,file)
  return(file)
}
