# Alternatives for base-functions, and some bugfixes

# write.table(...): Alternative for write.table (also helps for write.csv(2)) ----
#' Alternative for write.table
#'
#' Bugfix for the standard utils::write.table-function, copied from the standard-function and adapted.
#' When using nested data.frames and quote=TRUE, the original function first looks
#' at which "columns" are character of factor, and marks these for quoting, then flattens the structure to a matrix.
#' This functions also inspects columns that are data.frames or comparable. See the example for the difference.
#'
#' @param x,file,append,quote,sep,eol,na,dec See \code{\link[utils]{write.table}}
#' @param row.names,col.names,qmethod,fileEncoding See \code{\link[utils]{write.table}}
#' @examples
#' df <- data.frame(a='One;Two;Three',
#'                  b=I(data.frame(c="OtherVal",
#'                  d='Four;Five;Six',
#'                  e=4)))
#' \dontshow{environment(write.table) <- environment(utils::write.table)}
#' write.table(df, "~/Desktop/Tempfile.csv", quote = TRUE, col.names = NA,
#'             sep = ";", dec = ",", qmethod = "double")
#' # This fails for utils::write.table, because Four;Five;Six is unquoted, rendering the csv useless
#' @export
write.table <- function (x, file = "", append = FALSE, quote = TRUE, sep = " ",
                         eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE,
                         qmethod = c("escape", "double"), fileEncoding = "")
{
  qmethod <- match.arg(qmethod)
  if (is.logical(quote) && (length(quote) != 1L || is.na(quote)))
    stop("'quote' must be 'TRUE', 'FALSE' or numeric")
  quoteC <- if (is.logical(quote))
    quote
  else TRUE
  qset <- is.logical(quote) && quote
  if (!is.data.frame(x) && !is.matrix(x))
    x <- data.frame(x)
  makeRownames <- isTRUE(row.names)
  makeColnames <- is.logical(col.names) && !identical(FALSE,
                                                      col.names)
  if (is.matrix(x)) {
    p <- ncol(x)
    d <- dimnames(x)
    if (is.null(d))
      d <- list(NULL, NULL)
    if (is.null(d[[1L]]) && makeRownames)
      d[[1L]] <- seq_len(nrow(x))
    if (is.null(d[[2L]]) && makeColnames && p > 0L)
      d[[2L]] <- paste0("V", 1L:p)
    if (qset)
      quote <- if (is.character(x))
        seq_len(p)
    else numeric()
  }
  else {
    if (any(sapply(x, function(z) length(dim(z)) == 2 &&
                   dim(z)[2L] > 1))) {
      if (qset) {
        quote <- which(rapply(x, function(x) is.character(x) || is.factor(x)))
      }
      c1 <- names(x)
      x <- as.matrix(x, rownames.force = makeRownames)
      d <- dimnames(x)
    }
    else {
      if (qset)
        quote <- if (length(x))
          which(unlist(lapply(x, function(x) is.character(x) ||
                                is.factor(x))))
      else numeric()
      d <- list(if (makeRownames) row.names(x), if (makeColnames) names(x))
    }
    p <- ncol(x)
  }
  nocols <- p == 0L
  if (is.logical(quote))
    quote <- NULL
  else if (is.numeric(quote)) {
    if (any(quote < 1L | quote > p))
      stop("invalid numbers in 'quote'")
  }
  else stop("invalid 'quote' specification")
  rn <- FALSE
  rnames <- NULL
  if (is.logical(row.names)) {
    if (row.names) {
      rnames <- as.character(d[[1L]])
      rn <- TRUE
    }
  }
  else {
    rnames <- as.character(row.names)
    rn <- TRUE
    if (length(rnames) != nrow(x))
      stop("invalid 'row.names' specification")
  }
  if (!is.null(quote) && rn)
    quote <- c(0, quote)
  if (is.logical(col.names)) {
    if (!rn && is.na(col.names))
      stop("'col.names = NA' makes no sense when 'row.names = FALSE'")
    col.names <- if (is.na(col.names) && rn)
      c("", d[[2L]])
    else if (col.names)
      d[[2L]]
    else NULL
  }
  else {
    col.names <- as.character(col.names)
    if (length(col.names) != p)
      stop("invalid 'col.names' specification")
  }
  if (file == "")
    file <- stdout()
  else if (is.character(file)) {
    file <- if (nzchar(fileEncoding))
      file(file, ifelse(append, "a", "w"), encoding = fileEncoding)
    else file(file, ifelse(append, "a", "w"))
    on.exit(close(file))
  }
  else if (!isOpen(file, "w")) {
    open(file, "w")
    on.exit(close(file))
  }
  if (!inherits(file, "connection"))
    stop("'file' must be a character string or connection")
  qstring <- switch(qmethod, escape = "\\\\\"", double = "\"\"")
  if (!is.null(col.names)) {
    if (append)
      warning("appending column names to file")
    if (quoteC)
      col.names <- paste0("\"", gsub("\"", qstring, col.names),
                          "\"")
    writeLines(paste(col.names, collapse = sep), file, sep = eol)
  }
  if (nrow(x) == 0L)
    return(invisible())
  if (nocols && !rn)
    return(cat(rep.int(eol, NROW(x)), file = file, sep = ""))
  if (is.matrix(x) && !is.atomic(x))
    mode(x) <- "character"
  if (is.data.frame(x)) {
    x[] <- lapply(x, function(z) {
      if (is.object(z) && !is.factor(z))
        as.character(z)
      else z
    })
  }
  invisible(.External2(utils:::C_writetable, x, file, nrow(x), p,
                       rnames, sep, eol, na, dec, as.integer(quote), qmethod !=
                         "double"))
}

# `[<-.data.frame`(x, i, j, value): Bugfix from normal assignment ----
#' Bugfix for normal data.frame assignment
#'
#' See also \href{https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=17504}{issue #17504 at R-bugzilla}
#' Can be used as a stand-in for the normal data.frame-assignment-method, but works around a bug:
#' When assigning multiple columns and specifying rows, now there is no error thrown.
#' Based on base::`[<-.data.frame` from R-devel, revision 75615 (so also fixes bug 17483)
#'
#' @param x The data.frame into which to assign
#' @param i,j Indices for row(s) and column(s)
#' @param value The new values for replacement
#'
#' @seealso \code{\link[base]{[<-.data.frame}}
#' @export
`[<-.data.frame` <- function(x, i, j, value)
{
  if(!all(names(sys.call()) %in% c("", "value")))
    warning("named arguments are discouraged")

  nA <- nargs() # 'value' is never missing, so 3 or 4.
  if(nA == 4L) { ## df[,] or df[i,] or df[, j] or df[i,j]
    has.i <- !missing(i)
    has.j <- !missing(j)
  }
  else if(nA == 3L) {
    ## this collects both df[] and df[ind]
    if (is.atomic(value) && !is.null(names(value)))
      names(value) <- NULL
    if(missing(i) && missing(j)) { # case df[]
      i <- j <- NULL
      has.i <- has.j <- FALSE
      ## added in 1.8.0
      if(is.null(value)) return(x[logical()])
    } else { # case df[ind]
      ## really ambiguous, but follow common use as if list
      ## except for two column numeric matrix or full-sized logical matrix
      if(is.numeric(i) && is.matrix(i) && ncol(i) == 2) {
        # Rewrite i as a logical index
        index <- rep.int(FALSE, prod(dim(x)))
        dim(index) <- dim(x)
        tryCatch(index[i] <- TRUE,
                 error = function(e) stop(conditionMessage(e), call.=FALSE))
        # Put values in the right order
        o <- order(i[,2], i[,1])
        N <- length(value)
        if (length(o) %% N != 0L)
          warning("number of items to replace is not a multiple of replacement length")
        if (N < length(o))
          value <- rep(value, length.out=length(o))
        value <- value[o]
        i <- index
      }
      if(is.logical(i) && is.matrix(i) && all(dim(i) == dim(x))) {
        nreplace <- sum(i, na.rm=TRUE)
        if(!nreplace) return(x) # nothing to replace
        ## allow replication of length(value) > 1 in 1.8.0
        N <- length(value)
        if(N > 1L && N < nreplace && (nreplace %% N) == 0L)
          value <- rep(value, length.out = nreplace)
        if(N > 1L && (length(value) != nreplace))
          stop("'value' is the wrong length")
        n <- 0L
        nv <- nrow(x)
        for(v in seq_len(dim(i)[2L])) {
          thisvar <- i[, v, drop = TRUE]
          nv <- sum(thisvar, na.rm = TRUE)
          if(nv) {
            if(is.matrix(x[[v]]))
              x[[v]][thisvar, ] <- if(N > 1L) value[n+seq_len(nv)] else value
            else
              x[[v]][thisvar] <- if(N > 1L) value[n+seq_len(nv)] else value
          }
          n <- n+nv
        }
        return(x)
      }  # end of logical matrix
      if(is.matrix(i))
        stop("unsupported matrix index in replacement")
      j <- i
      i <- NULL
      has.i <- FALSE
      has.j <- TRUE
    }
  }
  else # nargs() <= 2
    stop("need 0, 1, or 2 subscripts")

  if ((has.j && !length(j)) ||	# "no", i.e. empty columns specified
      (has.i && !length(i) && !has.j))# empty rows and no col.   specified
    return(x)

  cl <- oldClass(x)
  ## delete class: S3 idiom to avoid any special methods for [[, etc
  class(x) <- NULL
  new.cols <- NULL
  nvars <- length(x)
  nrows <- .row_names_info(x, 2L)
  if(has.i && length(i)) { # df[i, ] or df[i, j]
    rows <- NULL  # indicator that it is not yet set
    if(anyNA(i))
      stop("missing values are not allowed in subscripted assignments of data frames")
    if(char.i <- is.character(i)) {
      rows <- attr(x, "row.names")
      ii <- match(i, rows)
      nextra <- sum(new.rows <- is.na(ii))
      if(nextra > 0L) {
        ii[new.rows] <- seq.int(from = nrows + 1L, length.out = nextra)
        new.rows <- i[new.rows]
      }
      i <- ii
    }
    if(!is.logical(i) &&
       (char.i && nextra  ||  all(i >= 0L) && (nn <- max(i)) > nrows)) {
      ## expand
      if(is.null(rows)) rows <- attr(x, "row.names")
      if(!char.i) {
        nrr <- (nrows + 1L):nn
        if(inherits(value, "data.frame") &&
           (dim(value)[1L]) >= length(nrr)) {
          new.rows <- attr(value, "row.names")[seq_along(nrr)]
          repl <- duplicated(new.rows) | match(new.rows, rows, 0L)
          if(any(repl)) new.rows[repl] <- nrr[repl]
        }
        else new.rows <- nrr
      }
      x <- xpdrows.data.frame(x, rows, new.rows)
      rows <- attr(x, "row.names")
      nrows <- length(rows)
    }
    iseq <- seq_len(nrows)[i]
    if(anyNA(iseq)) stop("non-existent rows not allowed")
  }
  else iseq <- NULL

  if(has.j) {
    if(anyNA(j))
      stop("missing values are not allowed in subscripted assignments of data frames")
    if(is.character(j)) {
      if("" %in% j) stop("column name \"\" cannot match any column")
      jseq <- match(j, names(x))
      if(anyNA(jseq)) {
        n <- is.na(jseq)
        jseq[n] <- nvars + seq_len(sum(n))
        new.cols <- j[n]
      }
    }
    else if(is.logical(j) || min(j) < 0L)
      jseq <- seq_along(x)[j]
    else {
      jseq <- j
      if(max(jseq) > nvars) {
        new.cols <- paste0("V",
                           seq.int(from = nvars + 1L, to = max(jseq)))
        if(length(new.cols)  != sum(jseq > nvars))
          stop("new columns would leave holes after existing columns")
        ## try to use the names of a list `value'
        if(is.list(value) && !is.null(vnm <- names(value))) {
          p <- length(jseq)
          if(length(vnm) < p) vnm <- rep_len(vnm, p)
          new.cols <- vnm[jseq > nvars]
        }
      }
    }
  }
  else jseq <- seq_along(x)

  ## empty rows and not (a *new* column as in  d[FALSE, "new"] <- val )  :
  if(has.i && !length(iseq) && all(1L <= jseq & jseq <= nvars))
    return(`class<-`(x, cl))

  ## addition in 1.8.0
  if(anyDuplicated(jseq))
    stop("duplicate subscripts for columns")
  n <- length(iseq)
  if(n == 0L) n <- nrows
  p <- length(jseq)
  if (is.null(value)) {
    value <- list(NULL)
  }
  m <- length(value)
  if(!is.list(value)) {
    if(p == 1L) {
      N <- NROW(value)
      if(N > n)
        stop(sprintf(ngettext(N,
                              "replacement has %d row, data has %d",
                              "replacement has %d rows, data has %d"),
                     N, n), domain = NA)
      if(N < n && N > 0L)
        if(n %% N == 0L && length(dim(value)) <= 1L)
          value <- rep(value, length.out = n)
        else
          stop(sprintf(ngettext(N,
                                "replacement has %d row, data has %d",
                                "replacement has %d rows, data has %d"),
                       N, nrows), domain = NA)
        if (!is.null(names(value))) names(value) <- NULL
        value <- list(value)
    } else {
      if(m < n*p && (m == 0L || (n*p) %% m))
        stop(sprintf(ngettext(m,
                              "replacement has %d item, need %d",
                              "replacement has %d items, need %d"),
                     m, n*p), domain = NA)
      value <- matrix(value, n, p)  ## will recycle
      ## <FIXME split.matrix>
      value <- split(c(value), col(value))
    }
    dimv <- c(n, p)
  } else { # a list
    ## careful, as.data.frame turns things into factors.
    ## value <- as.data.frame(value)
    value <- unclass(value) # to avoid data frame indexing
    lens <- vapply(value, NROW, 1L)
    for(k in seq_along(lens)) {
      N <- lens[k]
      if(n != N && length(dim(value[[k]])) == 2L)
        stop(sprintf(ngettext(N,
                              "replacement element %d is a matrix/data frame of %d row, need %d",
                              "replacement element %d is a matrix/data frame of %d rows, need %d"),
                     k, N, n),
             domain = NA)
      if(N > 0L && N < n && n %% N)
        stop(sprintf(ngettext(N,
                              "replacement element %d has %d row, need %d",
                              "replacement element %d has %d rows, need %d"),
                     k, N, n), domain = NA)
      ## these fixing-ups will not work for matrices
      if(N > 0L && N < n) value[[k]] <- rep(value[[k]], length.out = n)
      if(N > n) {
        warning(sprintf(ngettext(N,
                                 "replacement element %d has %d row to replace %d rows",
                                 "replacement element %d has %d rows to replace %d rows"),
                        k, N, n), domain = NA)
        value[[k]] <- value[[k]][seq_len(n)]
      }
    }
    dimv <- c(n, length(value))
  }
  nrowv <- dimv[1L]
  if(nrowv < n && nrowv > 0L) {
    if(n %% nrowv == 0L)
      value <- value[rep_len(seq_len(nrowv), n),,drop = FALSE]
    else
      stop(sprintf(ngettext(nrowv,
                            "%d row in value to replace %d rows",
                            "%d rows in value to replace %d rows"),
                   nrowv, n), domain = NA)
  }
  else if(nrowv > n)
    warning(sprintf(ngettext(nrowv,
                             "replacement data has %d row to replace %d rows",
                             "replacement data has %d rows to replace %d rows"),
                    nrowv, n), domain = NA)
  ncolv <- dimv[2L]
  jvseq <- seq_len(p)
  if(ncolv < p) jvseq <- rep_len(seq_len(ncolv), p)
  else if(p != 0L && ncolv > p) {
    warning(sprintf(ngettext(ncolv,
                             "provided %d variable to replace %d variables",
                             "provided %d variables to replace %d variables"),
                    ncolv, p), domain = NA)
    new.cols <- new.cols[seq_len(p)]
  }
  if(length(new.cols)) {
    ## extend and name now, as assignment of NULL may delete cols later.
    nm <- names(x)
    rows <- .row_names_info(x, 0L)
    a <- attributes(x); a["names"] <- NULL
    x <- c(x, vector("list", length(new.cols)))
    attributes(x) <- a
    names(x) <- c(nm, new.cols)
    attr(x, "row.names") <- rows
  }
  if(has.i)
    for(jjj in seq_len(p)) {
      jj <- jseq[jjj]
      vjj <- value[[ jvseq[[jjj]] ]]
      if(jj <= nvars) {
        ## if a column exists, preserve its attributes
        if(length(dim(x[[jj]])) != 2L) x[[jj]][iseq] <- vjj
        else x[[jj]][iseq, ] <- vjj
      } else {
        ## try to make a new column match in length: may be an error
        x[[jj]] <- vjj[FALSE]
        if(length(dim(vjj)) == 2L) {
          length(x[[jj]]) <- nrows * ncol(vjj)
          dim(x[[jj]]) <- c(nrows, ncol(vjj))
          x[[jj]][iseq, ] <- vjj
        } else {
          length(x[[jj]]) <- nrows
          x[[jj]][iseq] <- vjj
        }
      }
    }
  else if(p > 0L)
    for(jjj in p:1L) { # we might delete columns with NULL
      ## ... and for that reason, we'd better ensure that jseq is increasing!
      o <- order(jseq)
      jseq <- jseq[o]
      jvseq <- jvseq[o]

      jj <- jseq[jjj]
      v <- value[[ jvseq[[jjj]] ]]
      ## This is consistent with the have.i case rather than with
      ## [[<- and $<- (which throw an error).  But both are plausible.
      if (!is.null(v) && nrows > 0L && !length(v)) length(v) <- nrows
      x[[jj]] <- v
      if (!is.null(v) && is.atomic(x[[jj]]) && !is.null(names(x[[jj]])))
        names(x[[jj]]) <- NULL
    }
  if(length(new.cols) > 0L) {
    new.cols <- names(x) # we might delete columns with NULL
    ## added in 1.8.0
    if(anyDuplicated(new.cols)) names(x) <- make.unique(new.cols)
  }
  class(x) <- cl
  x
}

# format.Date and print.Date: bugfixes/alternatives to base ----
#' Alternative for format.Date
#'
#' On the R-devel mailinglist, it was noted that as.Date(Inf, origin='1970-01-01') is stored as a valid Date-object,
#' and is.na() returns FALSE, but when printing this object it shows 'NA", which is confusing.
#' It turns out this is because when formatting a Date-object it is converted to a POSIXlt-object, which fails for Inf, as well as other out-of-range values.
#' Therefore this function defaults to a numerical value if the date is outside the range 1-1-1 up till 9999-12-31, with a warning
#'
#' @param x Date to format
#' @param ... Other arguments passed on to format.POSIXlt
#'
#' @export
format.Date <- function (x, ...) {
  xx <- format(as.POSIXlt(x), ...)
  names(xx) <- names(x)
  if(any(!is.na(x) & (-719162>as.numeric(x) | as.numeric(x)>2932896))) {
    xx[!is.na(x) & (-719162>as.numeric(x) | as.numeric(x)>2932896)] <-
      paste('Date with numerical value',as.numeric(x[!is.na(x) & (-719162>as.numeric(x) | as.numeric(x)>2932896)]))
    warning('Some dates are not in the interval 01-01-01 and 9999-12-31, showing numerical value.')
  }
  xx
}

#' Different approach-route for print.Date
#'
#' A bit of a hack, redefining print.Date.
#' It's the same as in base (R 3.5.0), but this is calling EmilMisc::format.Date
#' \cr\cr Note that the the interaction between this declaration and the S3-dispatch-system is a bit messy, and getS3method('print', class = 'Date') will be probably not give you this function.
#' If you do want the source-code, simply call EmilMisc:::printDate (with three colons)
#'
#' @param x Date to format
#' @param max Maximum number of dates to print. NULL to use getOption("max.print", 9999L)/default maximum
#' @param ... Other arguments passed on to print
#'
#' @export
print.Date <- function (x, max = NULL, ...)
{
  if (is.null(max))
    max <- getOption("max.print", 9999L)
  if (max < length(x)) {
    print(format(x[seq_len(max)]), max = max, ...)
    cat(" [ reached getOption(\"max.print\") -- omitted",
        length(x) - max, "entries ]\n")
  }
  else if (length(x))
    print(format(x), max = max, ...)
  else cat(class(x)[1L], "of length 0\n")
  invisible(x)
}

# stop(..., quiet=FALSE): Customized from base, quiet options and with line-numbers ----
#' Customized stop, gives lines-numbers if called from script
#'
#' Same as base::stop(), but when called from a sourced script, it also output the filename and linenumber
#' @param ... arguments passed on to base::stop()
#' @param quiet Useful for controlled stopping from a script. \cr
#' @param call. numeric or logical.\cr
#' For a logical: same as in \code{base::stop}, indicating if the call should become part of the error message. Ignored if quiet = TRUE\cr
#' For a numeric: number of steps to skip in error-reporting. This is useful if a function calls another, but you want to show it as an error from the caller.
#' Be careful that call.=0 is equivalent to call.=TRUE
#' If \code{TRUE}, no output is printed, and a recover-function as provided in options(error=) is bypassed.
#' @export
stop <- function(..., quiet=FALSE, call. = TRUE) {
  if(quiet) {
    cat(...)
    opt <- options(show.error.messages = FALSE, error=NULL)
    on.exit(options(opt))
  }
  callidx <- call. - 2
  if(length(sys.call(callidx))>0 && sys.call(callidx)=='eval(ei, envir)' && sys.call(1)[[1]]=='source') {
    base::stop('\rError in ',strtrim(utils::getSrcFilename(sys.call(), full.names = TRUE), getOption('width')-20),' (line ',utils::getSrcLocation(sys.call()),'):\n  ', ..., call.=FALSE)
  } else if(is.numeric(call.)) {
    base::stop('\rError in ',deparse(sys.call(callidx)[[call.+1]])[[1]],':\n  ', ..., call. = FALSE)
  } else if(call.) {
    base::stop('\rError in ',deparse(sys.call(callidx)[[1]])[[1]],':\n  ', ...,call. = FALSE)
  } else {
    base::stop(..., call. =-1)
  }
}

# %mod%: Modulo with provisions for near-equality ----
#' Modulo-operator with near-equality
#'
#' The \code{\link[base:Arithmetic]{`\%\%`}} operator calculates the modulo, but sometimes has rounding errors, e.g. "\code{(9.1/.1) \%\% 1}" gives ~ 1, instead of 0.\cr
#' Comparable to what all.equal does, this operator has some tolerance for small rounding errors.\cr
#' If the answer would be equal to the divisor within a small tolerance, 0 is returned instead.
#'
#' For integer x and y, the normal \%\%-operator is used
#'
#' @usage `\%mod\%`(x, y, tolerance = sqrt(.Machine$double.eps))
#' @section Alternative usage:
#' \code{x \%mod\% y} may be most useful in practice
#' @param x,y numeric vectors, similar to those passed on to \%\%
#' @param tolerance numeric, maximum difference, see \code{\link[base]{all.equal}}. The default is ~ \code{1.5e-8}
#' @return identical to the result for \%\%, unless the answer would be really close to y, in which case 0 is returned
#' @note To specify tolerance, use the call \code{`\%mod\%`(x,y,tolerance)}
#' @note The precedence for \code{\%mod\%} is the same as that for \code{\%\%}
#'
#' @name mod
#' @rdname mod
#'
#' @export
`%mod%` <- function(x,y, tolerance = sqrt(.Machine$double.eps)) {
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(tolerance),
            !is.na(tolerance), length(tolerance)==1, tolerance>=0)
  if(is.integer(x) && is.integer(y)) {
    return(x %% y)
  } else {
    ans <- x %% y
    return(ifelse(abs(ans-y)<tolerance | abs(ans)<tolerance, 0, ans))
  }
}

# args(name): rewrite which also returns something workable for all primitives ----
#' Rewrite of args(), which also returns something workable for all primitives
#'
#' The function \code{\link[base]{args}} may return NULL for some primitives, e.g. `[`.
#' This function checks for that, and in that case returns the most general function possible:
#' \code{function(...) NULL}
#' Otherwise the return is identical
#'
#' @param name A function, or a character string with the name of a function (which is found using the scope of the caller).
#' @return Identical as that of \code{\link[base]{args}}, except when called with a primitive, and args() returns NULL.
#' In that case, an empty function
#'
#' @export
args <- function(name) {
  if(is.character(name)) name <- get(name, parent.frame(), mode='function')
  if(!is.function(name)) return(NULL)
  ret <- base::args(name)
  if(is.null(ret) && is.primitive(name)) {
    ret <- function(...) NULL
    environment(ret) <- parent.frame()
  }
  return(ret)
}

# formalArgs(def): Now looks in the parent.frame if def is given as a character ----
#' Bugfix for formalArgs(def)
#'
#' In the methods-package (loaded by default), formalArgs(def) uses the search path from namespace:methods, which can lead to confusing behaviour if you overwrite something,
#' especially as formals(def) DOES use the environment of the caller.
#'
#' Example of different behaviour:\cr
#' \code{by <- function(a, b ,c) {"Bye-bye!"}
#' formalArgs(by) # As expected, by does not have any quotes
#' names(formals('by')) # Also works as expected
#' methods::formalArgs('by') # Bug: by is found in the base-package
#' formalArgs('by') # Result as expected
#' }
#'
#' @param def Either a function, or a character naming a function
#' @export
formalArgs <- function(def) names(do.call(formals, list(def), envir=parent.frame()))

# tryCatch(expr, ..., finally) and withCallingHandlers(expr, ...) ----
# With support for expressions, to be evaluated in the same environment as expr
#' Extensions of base::tryCatch and base::withCallingHandlers
#'
#' The regular \code{\link[base:tryCatch]{base::tryCatch}} and \code{\link[base]{withCallingHandlers}}
#' calls a function whenever \code{expr} generates an error, which means this function gets its own environment.
#' However, sometimes it's easier to evaluate any code in the same environment as \code{expr} (see examples).\cr
#' Therefore these extensions can work with expressions as well, which are then evaluated in the calling context.
#' If this expression returns a function, then that function is called with the condition-object.
#' Note that 'expression' here is used in the sense of 'some R-code', so \code{error=function(e) {e$message}} is seen as a very simple expression, which
#' returns a function, which is then called. This means that you can still use the same calls as in \code{base::tryCatch} and
#' \code{withCallingHandlers}.
#'
#' For use of the condition-object in the main expressions, you can access it under the name "cond" if there is no variable under that name yet.
#' If there is one, this variable is left as-is (with a warning), and the current condition-object can be accessed with \code{get('cond', parent.frame(2))}.\cr
#' The latter form can always be used (for cases when you're unsure of its existence)
#'
#' @note All current variables are potentially modified by the condition-throwing expression, which may be very desirable (for debugging) or
#' very undesirable/confusing (as some objects can be in an unexpected/corrupted state)
#'
#' @param expr Expressions to evaluate that might throw an error
#' @param ... Handlers to call if expr throws a condition
#' @param finally expression that is always evaluated before returning or exiting
#'
#' @section Note on backwards compatibility:
#' This function is meant as a stand-in replacements for the base-functions, but there are differences in the calling stack.
#' See for example the difference in the options you can choose from in the following calls:\cr
#' \code{base::tryCatch(stop(), error=function(e) recover())}\cr
#' vs\cr
#' \code{tryCatch(stop(), error=function(e) recover()}\cr\cr
#' Therefore there may be some differences in debugging code as well, and code should not rely on any output
#' of parent.frame(n) or length(sys.calls()) etc.
#'
#' @examples
#' errorlog <- character(0) # Or some other previous log
#' tryCatch({step <- 1;stop('SomeError');step <- 2},
#'   warning=function(w) print(w),
#'   error={errorlog <- c(errorlog, paste("\nError occured:\n", cond$message, "\nat step:", step))
#'          step <- 0
#'          function(e) {err <- getOption('error'); if(!is.null(err)) eval(err)}
#'   })
#'
#' @export
tryCatch <- function(expr, ..., finally) {
  parenv <- parent.frame()
  handlers <- lapply(substitute(list(...))[-1], function(h) {
    function(cond) {
      if(!exists('cond', where = parenv, inherits=FALSE)) {
        assign('cond', cond, pos = parenv, inherits=FALSE)
        on.exit(rm('cond', pos = parenv, inherits = FALSE))
      } else {
        warning('Variable "cond" already exists, so unable to assign')
      }
      ret <- eval(h, parenv)
      if(is.function(ret)) return(ret(cond)) else return(ret)
    }
  })
  do.call(base::tryCatch, args=c(substitute(expr), handlers, if(!missing(finally)) substitute(finally)), envir = parenv)
}
#' @rdname tryCatch
#' @export
withCallingHandlers <- function (expr, ...) {
  parenv <- parent.frame()
  handlers <- lapply(substitute(list(...))[-1], function(h) {
    function(cond) {
      if(!exists('cond', where = parenv, inherits=FALSE)) {
        assign('cond', cond, pos = parenv, inherits=FALSE)
        on.exit(rm('cond', pos = parenv, inherits = FALSE))
      } else {
        warning('Variable "cond" already exists, so unable to assign')
      }
      ret <- eval(h, parenv)
      if(is.function(ret)) return(ret(cond)) else return(ret)
    }
  })
  do.call(base::withCallingHandlers, args=c(substitute(expr), handlers), envir = parenv)
}

# vmapply: A combination of mapply and vapply ----
#' Combination of vapply and mapply
#'
#' As \code{\link[base]{mapply}} is a multivariate version of \code{\link[base]{sapply}}, this is a multivariate version of
#' \code{\link[base]{vapply}}.\cr
#' FUN is applied to the first elements in ..., the second, and so on. Arguments are recycled if necessary.
#'
#' @param FUN function to apply, found via \code{\link[base]{match.fun}}
#' @param FUN.VALUE a (generalized) vector; a template for the return value from FUN. See \code{\link[base]{vapply}}
#' @param ... arguments to vectorize over (vectors or lists of strictly positive length, or all of zero length). See \code{\link[base]{mapply}}
#' @param MoreArgs a list of other arguments to FUN. Note that this is always evaluated once, even if all arguments in ... are length-zero.
#' Use of an anonymous function may be safer.
#' @param USE.NAMES logical; use names if the first ... argument has names, or if it is a character vector, use that character vector as the names.
#' Since this argument follows ... its name cannot be abbreviated.
#'
#' @export
vmapply <- function (FUN, FUN.VALUE, ..., MoreArgs = NULL, USE.NAMES = TRUE) {
  answer <- mapply(FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE, USE.NAMES = USE.NAMES)
  vapply(answer, identity, FUN.VALUE, USE.NAMES=FALSE)
}

# Room for more functions ----



