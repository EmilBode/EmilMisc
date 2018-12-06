.onAttach <- function(libname, pkgname) {
  if(identical(NA, getOption('checkMasking_Allowed', default=NA))) {
    options(checkMasking_Allowed=data.frame(
      name=c('src', 'n', 'par', 'tryCatch', 'args', 'stop', 'FunctionNameThatServesAsAnExample'),
      env=c('.GlobalEnv', '.GlobalEnv', '.GlobalEnv', 'package:EmilMisc', 'args', 'stop', 'any')))
  }
  cma <- getOption('checkMasking_Allowed')
  cma <- data.frame(lapply(cma, as.character), stringsAsFactors = FALSE)
  cma[nrow(cma)+1,] <- c('stop','package:EmilMisc')
  cma[nrow(cma)+1,] <- c('tryCatch','package:EmilMisc')
  cma[nrow(cma)+1,] <- c('withCallingHandlers','package:EmilMisc')
  cma[nrow(cma)+1,] <- c('args','package:EmilMisc')
  cma[nrow(cma)+1,] <- c('formalArgs','package:EmilMisc')
  cma[nrow(cma)+1,] <- c('[<-.data.frame','package:EmilMisc')
  cma[nrow(cma)+1,] <- c('write.table','package:EmilMisc')
  options(checkMasking_Allowed=cma)
  #if(is.null(getOption('envpushnames'))) {
  #  options(envpushnames='OrigGlobal')
  #}
  rm(cma)
  environment(write.table) <- environment(utils::write.table)
}

.onAttach()
myfun <- function(x, y, someopt=T, ...) {
  return(function(a,b) {x+y+a+b})
}
newfun <- myfun(1e6, 1e4)
if(F) {
  nargs <- function() {
    fun <- get('nargs')
    if(identical(environment(fun), parent.env(environment()))) {
      return(do.call(base::nargs, args=list(), envir = parent.frame()))
    } else {
      return(do.call(fun, list(), envir=parent.frame()))
    }
  }
  `<<-` <- function(x, value) {get('<<-')(x, value)}
}

