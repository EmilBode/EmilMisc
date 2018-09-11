#' Readline with waiting limit
#'
#' Normally, when using readline, execution is halted until input is supplied, and if it is not, the script hangs forever
#' This functions waits a set time for the user to provide input, then returns control
#'
#' @param prompt prompt for the user, similar to the one in readline(). Usually ends with a space (or newline).
#' @param timeout timer in seconds. If set to 0, base::readline is used
#' @param precision Polling interval in seconds. \cr
#' Internally, every \code{precision} seconds the script checks if valid input is received.
#' Smaller values increase responsiveness, but also increase CPU-usage.
#' @return Input provided by the user, if provided in time (and finished with RETURN)\cr
#' If the timer has expired, \code{character(0)}, even if the user has started typing but has not yet pressed RETURN.
#' If used in a non-interactive session, NULL, and if the timer has expired, character(0).\cr
#' If timeout==0, the same as provided by readline()
#' @note Unfortunately, does not work on all platforms/in all environments.\cr
#' It is dependent on being able to read from file('stdin'), which is different from stdin(), see \code{\link[base]{file}} for details.
#' In particular, it does not work in RStudio for MacOS, or Rgui for Windows or MacOS.
#' It does confirmed work on R run from terminal on MacOS 10.13.6\cr
#' Problems manifest by file('stdin') not being connected to anything, i.e. no input is received, so this function always returns \code{character(0)},
#' and any input typed is interpreted as a new command. To test, try with a small value of timeout first.
#' @seealso \code{\link{waitForKey}} for a function that simply waits for a keypress for a set time.
#' @export
readline_time <- function(prompt, timeout = 3600, precision=.1) {
  stopifnot(length(prompt)<=1,
            is.numeric(timeout),
            length(timeout)==1,
            !is.na(timeout),
            timeout>=0,
            is.numeric(precision),
            length(precision)==1,
            !is.na(precision),
            precision>0)
  if(!interactive()) return(NULL)
  if(timeout==0) return(readline(prompt))
  timer <- timeout
  my_in <- file('stdin')
  open(my_in, blocking=FALSE)
  cat(prompt)
  ans <- readLines(my_in, n=1)
  while(timer>0 && !length(ans)) {
    Sys.sleep(precision)
    timer <- timer-precision
    ans <- readLines(my_in, n=1)
  }
  close(my_in)
  return(ans)
}

#' Wait for set time, or continue on input.
#'
#' Waits until a timer has expired, or the user has provided some input (pressed RETURN)\cr
#' A message is shown, with a countdown-timer if desired.
#' @param message Message to be shown during countdown. Use {n} as a placeholder for the number of seconds left.
#' @param time Time to wait, in seconds (approximate)
#' @param counter Interval to update the message, in seconds. 0 to remain static. Must otherwise be a multiple of precision to work reliably.
#' @param precision Polling interval to use when checking for a keypress.
#' @return (invisibly) Either the string 'key' or 'timer', signifying what caused the return
#' @note Unfortunately, does not work on all platforms/in all environments.\cr
#' It is dependent on being able to read from file('stdin'), which is different from stdin(), see \code{\link[base]{file}} for details.
#' In particular, it does not work in RStudio for MacOS, or Rgui for Windows or MacOS.
#' It does confirmed work on R run from terminal on MacOS 10.13.6\cr
#' Problems manifest by file('stdin') not being connected to anything, i.e. no input is received, so this function always returns \code{character(0)},
#' and any input typed is interpreted as a new command. To test, try with a small value of time first.
#' @seealso \code{\link{readline_time}} for a function that also collects your input.
#' @export
waitForKey <- function(message='Continuing in {n} seconds, or press any key.', time=10, counter=.5, precision=.01) {
  stopifnot(is.character(message), length(message)==1, !is.na(message),
            is.numeric(time), length(time)==1, !is.na(time), time>0,
            is.numeric(counter), length(counter)==1, !is.na(counter), counter>=0,
            is.numeric(precision), length(precision)==1, !is.na(precision), precision>0)
  my_in <- file('stdin')
  open(my_in, blocking=FALSE)
  ans <- readLines(my_in, n=1)
  if(counter==0) cat(message)
  while(time>0 && !length(ans)) {
    if(counter>0 && !is.null(message) && round(time/counter, digits = 3) %% 1L==0L) {
      cat(gsub('\\{n\\}', format(round(time, digits=4), width=5, scientific = F), message), '\r', sep = '')
    }
    Sys.sleep(precision)
    time <- time-precision
    ans <- readLines(my_in, n=1)
  }
  close(my_in)
  if(length(ans)) {
    return(invisible('key'))
  } else {
    return(invisible('timer'))
  }
}
