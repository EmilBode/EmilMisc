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

if (FALSE) { # Too experimental

  # envpush and envpop: simulate calling stack ----
  #' Simulate calling of functions: push and pop stack
  #'
  #' Simulate calling of functions and returning, for debugging purposes.\cr\cr
  #' It is often useful to go through functions step-by-step, such as can be done with browser()\cr
  #' However, when doing this, you can't edit the source code as you go.
  #' Therefore these functions make it possible to run parts of your code, and simulate the calling of functions.
  #' envpush simulates adding a layer to the stack: all variables in the "from"-environment are stored in a temporary environment,
  #' so it looks like you're working in a new layer.\cr
  #' envpop() reverses this operation, optionally mapping a return value to a new value in your environment\cr
  #' return() overrides the normal return, but is only used when called from the top-level, and when 'popping' environments are available.\cr
  #' popall() is for quickly returning to the top-level, without returning anything
  #'
  #' Some other functions are modified for simulating being in another layer:
  #' \itemize{
  #'   \item Arguments that are not specified are initialized to their default values, but with a "missing" attribute set to TRUE.\cr
  #'   The modified missing()-function also checks for this attribute
  #'   \item The "<<-"-operator no longer defaults to the gloabl environment, but to what was originally the global environment
  #' }
  #'
  #'
  #' Some care is still needed when working with '...', as some effort is made to mimic the right behaviour, but this hasn't been tested
  #' thoroughly.
  #'
  #' @param mycall A call to use, for which execution is simulated.\cr
  #' See examples
  #' @param from,to Environment to push from, and to pop back to. \cr
  #' This means all values are copied from/to this environment, to/from a temporary copy-environment, a parent from the new clean environment. \cr
  #' Defaults to the global environment, caution is needed for other environments.
  #' @param envname Name for the temporary environment to store the current scope. NA means default, the name as stored in getOption('envpushnames'),
  #' along with an incremental counter
  #' @param check Before popping, do we check if the parent.env() is one of the temporary-names ones. Useful to prevent popenv() from screwing with your workspace,
  #' set to FALSE when changing environment-names
  #' @param retvar Should a return variable be mapped to a new variable in the environment we pop back to?\cr
  #' For envpop(): A variable with the first name is assigned to the second name\cr
  #' For return(): The returned \emph{value} is assigned to a variable with this name
  #' @param value A value to assign to a variable with the name given by \code{retvar}
  #'
  #' @examples
  #' myfun <- function(x, y, someopt=T, ...) {paste0(x, y, z)}
  #' a <- 2
  #' y <- 14
  #' z <- 17
  #' envpush(myfun(a))
  #' # This causes all content of the global environment to be assigned to a new environment, the global environment to be cleared,
  #' # and the new environment to be set as the parent of the global environment (the new is 'squeezed' between).
  #' # And x is set to the value of a, y is set to missing, someopt to TRUE, and ... to missing as well.
  #' For completeness, someopt is also given the attribute 'missing', set to TRUE, which causes missing(someopt) to return TRUE as well
  # end ----
  envpush <- function(mycall=NULL, from=.GlobalEnv, envname=NA) {
    mycall <- substitute(mycall)
    mycall <- capture.output(mycall)
    stopifnot(is.null(mycall) || is.character(mycall) || is.call(mycall) || is.name(mycall) &&
                is.environment(from) && is.na(envname) || is.character(envname))
    if(is.na(envname)) {
      n <- 1
      envname <- paste0(getOption('envpushnames'), n)
      srch <- search()
      while(envname %in% srch) {
        n <- n+1
        envname <- paste0(getOption('envpushnames'), n)
      }
    }
    if(is.name(mycall)) mycall <- eval(mycall)
    if(is.character(mycall)) mycall <- parse(text=mycall)[[1]]
    newenv <- list2env(as.list(from), parent=parent.env(from))
    attr(newenv, 'name') <- envname
    if(!is.null(mycall)) {
      if(is.primitive(eval(mycall[[1]]))) {
        message('Argument matching with primitives may cause problems')
        fullcall <- mycall
        nms <- formalArgs(args(eval(mycall[[1]], envir=parent.frame())))
        if(length(nms)==length(mycall)-1) {
          names(fullcall) <- c('', nms)
        } else if(length(wh <- which(nms=='...'))) {
          fullcall <- as.list(fullcall)
          fullcall <- c(fullcall[[1]],
                        fullcall[seq_len(wh-1)+1],
                        `...`=list(fullcall[seq_len(length(mycall)-length(nms))+wh]),
                        fullcall[seq_len(length(nms)-wh)+length(fullcall)-wh])
          names(fullcall) <- c('', nms)
          fullcall <- as.call(fullcall)
          dots <- fullcall[['...']]
        } else {
          stop('Problem in argument matching')
        }
      } else {
        fullcall <- match.call(definition=eval(mycall[[1]]), mycall, envir=from, expand.dots = FALSE)
      }
      for(nm in names(fullcall)[-1]) {
        tryCatch({
          assign(nm, eval(fullcall[[nm]], newenv), from, inherits=FALSE)
          if(nm=='...') assign('dots', eval(fullcall[[nm]], newenv), from, inherits=FALSE)
        }, error=function(e) {
          if(e$message=="argument is missing, with no default") {
            assign(nm, fullcall[[nm]], from, inherits=FALSE)
            if(nm=='...') assign('dots', fullcall[[nm]], from, inherits=FALSE)
          } else {
            base::stop('\rError in envpush(', deparse(mycall), '): ', e$message, call. = FALSE)
          }
        })
      }
      miss <- formals(eval(fullcall[[1]], newenv))
      miss <- miss[names(miss) %!in% names(fullcall)[-1]]
      for(nm in names(miss)) {
        tryCatch({
          val <- eval(miss[[nm]], from)
          if(is.null(val)) {
            assign('.missingNULLs', c(get0('.missingNULLs', from, inherits=FALSE), nm), from, inherits=FALSE)
          } else {
            attr(val, 'missing') <- TRUE
          }
          assign(nm, val, from, inherits=FALSE)
          if(nm=='...') assign('dots', val, from, inherits=FALSE)
        },
        error=function(e) {
          if(e$message=="argument is missing, with no default") {
            assign(nm, miss[[nm]], from, inherits=FALSE)
            if(nm=='...') assign('dots', miss[[nm]], from, inherits=FALSE)
          } else {
            stop('Unexpected error')
          }
        })
      }
      do.call(rm, list(list=ls(from, all.names=TRUE)[ls(from, all.names=TRUE) %!in% c(names(fullcall), names(miss), 'dots', '.missingNULLs')], pos=from))
    } else {
      do.call(rm, list(list=ls(from, all.names=TRUE), pos=from))
    }
    parent.env(from) <- environment(eval(mycall[[1]]))
    assign('parent.frame', function(n) {
      if(n>1) {
        return(get('parent.frame', newenv)(n-1))
      } else {
        return(newenv)
      }
    }, from, inherits=FALSE)
    if(!exists('nargs',from, inherits=FALSE)) {
      assign("nargs", function() {
        return(length(mycall)-1)
      }, from, inherits = FALSE)
    }
    if(!exists('<<-', from, inherits=FALSE)) {
      assign("<<-", function(x, value) {
        if(exists(as.character(substitute(x)), parent.env(parent.frame()), inherits=TRUE)) {
          do.call(base::`<<-`, list(substitute(x), value), envir = parent.frame())
        } else {
          assign(as.character(substitute(x)), value, paste0(getOption('envpushnames'), 1), inherits = FALSE)
        }
      }, from, inherits = FALSE)
    }
    return(invisible(0))
  }
  envpop <- function(to=.GlobalEnv, check=TRUE, retvar=c('ret', 'ret')) {
    if(check && !grepl(paste0('^',getOption('envpushnames'),'[0-9]+$'), environmentName(parent.env(to)))) stop('Unexpected top-environment-name')
    retmiss <- missing(retvar)
    if(length(retvar)) {
      if(retvar[1] %in% ls(to)) {
        assign(retvar[2], get(retvar[1], to, inherits=FALSE))
      } else if(!retmiss) {
        warning('Return-variable not found')
      }
    }
    rm(list=ls(to, all.names=TRUE)[ls(to, all.names=TRUE)!=retvar[2]], pos=to)
    for(v in ls(parent.env(to), all.names = TRUE)) {
      if(do.call(missing, list(v), envir=parent.env(to))) {
        assign(v, formals(base::`-.Date`)[[1]], pos=to) # The first base-function with a missing argument
      } else {
        assign(v, get(v, parent.env(to)), pos=to)
      }
    }
    parent.env(to) <- parent.env(parent.env(to))
    base::return(invisible(0))
  }
  return <- function(value, retvar='ret') {
    if(missing(value)) value <- NULL else value <- withVisible(value)
    if(length(sys.calls())==1 && any(grepl(paste0('^',getOption('envpushnames'), '[0-9]+$'), search()))) {
      if(!is.null(retvar))
        assign(retvar, value$value, parent.env(parent.frame()))
      envpop(to=parent.frame(), retvar=c())
      cat('Popping!\n')
    } else if(is.null(value) || value$visible) {
      do.call(base::return, list(value$value), envir=parent.frame())
    } else {
      do.call(function(value) {base::return(invisible(value))}, list(value$value), envir=parent.frame())
    }
  }
  popall <- function(to=.GlobalEnv) {
    while(grepl(getOption('envpushnames'), environmentName(parent.env(to)), fixed=TRUE)) {
      if(exists('ret', where = to, inherits=FALSE)) rm('ret', pos = to, inherits=FALSE)
      envpop(to)
    }
  }
  missing <- function(var) {
    if(length(sys.calls())>1) {
      do.call(base::missing, list(substitute(var)), envir = parent.frame())
    } else if(base::missing(var)) {
      return(TRUE)
    } else if(!is.null(attr(var, 'missing'))) {
      base::return(attr(var, 'missing'))
    } else if(!is.null(var)) {
      base::return(FALSE)
    } else {
      base::return(as.character(substitute(var)) %in% get0('.missingNULLs', parent.frame(), inherits=FALSE))
    }
  }
  # fmultimatch(x, table, nomatch=integer(), incomparables=NULL, simplify=FALSE): Return multiple matches, with fmatch help ----
  #' Combination of \code{\link[fastmatch:fmatch]{fastmatch::fmatch}} and \code{\link[S4Vectors:findMatches]{S4Vectors::findMatches}}
  #'
  #' Werkt niet, subset van table moet elke keer nieuwe hash bedenken
  #'
  #' It uses \code{fmatch}'s hash to execute faster, but returns multiple matches.
  #' May not be very fast in the case of many matches or many values to look up, but will work for up to a few matches
  #' It may be neccesary to first set up your table with \code{fastmatch::fmatch.hash(x, table)} first
  #'
  #' @param x Values to look up, should be unique
  #' @param table values to be matched against
  #' @param nomatch value to be returned in the case of no matches. Note that this differs from \code{fastmatch::fmatch}
  #' @param incomparables a vector of values that cannot be matched. Compatibilty feature, any other value other then NULL is discouraged (as fastmatch won't be used in that case)
  #' @param simplify Should the result be simplified to a vector or matrix (when possible)
  #'
  #' @return A list with matched values of x (or an array when simplify=TRUE and possible)
  #'
  #' @example
  #' set.seed(1)
  #' mytable <- sample(1:100, size=50, replace=TRUE)
  #' fmultimatch(1:10, mytable)
  #'
  #' @export
  fmultimatch <- function(x, table, nomatch=integer(), incomparables=NULL, simplify=FALSE) {
    stopifnot(!anyDuplicated(x))
    ret <- rep(list(integer()), length(x))
    found <- integer()
    while(TRUE) {
      newmtches <- fastmatch::fmatch(x, table[-found], incomparables = incomparables)
      if (all(is.na(newmtches))) break
      found <- c(found, newmtches[!is.na(newmtches)])
      ret <- mapply(c, ret, newmtches)
    }
  }
  # More functions ----
}































