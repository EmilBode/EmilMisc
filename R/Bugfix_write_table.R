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

if(!exists('.onAttach'))
  .onAttach <- function(libname, pkgname) {
    environment(write.table) <- environment(utils::write.table)
  }
