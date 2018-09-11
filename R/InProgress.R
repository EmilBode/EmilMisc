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

# Try to make a readline that continues after a set time
readline_time <- function(prompt, timeout = 3600, precision=.1, end=NA) {
  if(!require(future)) stop()
  if(timeout==0) return(readline(prompt))
  funenv <- environment()
  timer <- timeout
  cat(prompt)
  ansfun <- function() {
    tryCatch({
      assign('my_in', stdin(), pos=funenv)
      temp <- readLines(my_in, n=1)
      writeLines(temp, tempfile(tmpdir = '~/Desktop/'))
      return(temp)
      }, error=function(e) return(NA))
  }
  while(timer>0 && (timer==timeout || !resolved(ans) || length(value(ans))==0 || is.na(value(ans)))) {
    ans <- future(ansfun(), lazy=TRUE) %plan% multiprocess
    Sys.sleep(precision)
    timer <- timer-precision
    if(exists('my_in')) rm(my_in)
  }
  if(timer>0 && resolved(ans)) return(value(ans)) else return(end)
}
readline_time <- function(prompt, timeout = 3600, precision=.1, end=NA) {
  if(timeout==0) return(readline(prompt))
  timer <- timeout
  my_in <- file('stdin')
  open(my_in, blocking=FALSE)
  ans <- readLines(my_in, n=1)
  while(timer>0 && !length(ans)) {
    Sys.sleep(precision)
    timer <- timer-precision
    ans <- readLines(my_in, n=1)
  }
  close(my_in)
  return(ans)
}
waitForKey <- function(message='Continuing in {n} seconds, or press any key.', time=10, counter=.5, precision=.01) {
  if(!is.null(message)) message <- strsplit(message, split='\\{n\\}')[[1]]
  my_in <- file('stdin')
  open(my_in, blocking=FALSE)
  ans <- readLines(my_in, n=1)
  while(time>0 && !length(ans)) {
    if(!is.null(message) && round(time/counter, digits = 3) %% 1L==0L) {
      cat(message[1], format(round(time, digits=4), width=5, scientific = F), message[2], '\r', sep = '')
    }
    Sys.sleep(precision)
    time <- time-precision
    ans <- readLines(my_in, n=1)
  }
  close(my_in)
  return(invisible(0))
}

mywait <- function() {
  library(tcltk)
  tt <- tktoplevel()
  tkpack( tkbutton(tt, text='Continue', command=function()tkdestroy(tt)),
          side='bottom')
  tkbind(tt,'<Key>', function()tkdestroy(tt) )

  tkwait.window(tt)
}


