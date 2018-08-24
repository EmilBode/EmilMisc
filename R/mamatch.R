#' Extension to stringdist::amatch, returning multiple matched
#'
#' When trying to find matches in large sets, often mutliple results are possible/likely.
#' Especially when using multiple criteria, it can be useful to first have a broad search. \cr
#' An example is trying to match a list of people to another list of people:
#' a first approach would be matching family-names, later extended to include first names, place of origin, etc.\cr
#' Or if you're unsure what exact method to use, you can experiment with one first, then use others to further limit results, without
#' having to check your entire dataset again.
#' So this function gives the most likely matches: the \emph{maxmatch} lowest distance matches, up to \emph{maxDist} away.\cr
#' For ties, the first matches in table are returned
#'
#' @param x,table,matchNA,method,useBytes,weight,maxDist,q,p,bt,nthread See \code{\link[stringdist]{amatch}}
#' @param nomatch See also amatch, but for returnAs=='list', it can be NULL
#' @param maxmatch Maximum number of matches to return. \cr Capped at length(table)
#' @param limitMem Limit memory usage. For large x and table, a lot of memory is needed for the matrix with distances.
#' (Internally, this script calls stringdistmatrix, which means a matrix of \code{length(x) * length(table) * 8} bytes.\cr
#' You can provide this parameter to chunk processing, with matrices of
#' ~ limitMem bytes. 0 (default) means unlimited, otherwise size in bytes. \cr
#' Note that this is not all the needed memory, but for large x and table, the size of the matrix is the dominant factor.
#' @param returnAs comparable to simplify in sapply: should result be returned as a list or an matrix? \cr
#' "matrix" fills any non-matches with \emph{nomatch}, so it always returns a matrix (even when sapply wouldn't)
#' @param dupls Are there possibly duplicates present? Decides what kind of algorithm is used.\cr
#' if \code{TRUE}, only the distance between the unique values are calculated, but more overhead is used to return the mapping to the original values.\cr
#' if \code{FALSE}, all distances are calculated, which takes longer if duplicates are present. But there is less overhead.\cr\cr
#' Note that the results may differ in detail in case of ties. To be precise: for dupls==TRUE, indices equal to a first match are returned before differing indices, e.g:\cr
#' \code{mamatch('abcde', c('abcd1','abcd2','abcd3','xxx','xxx','abcd1'), method='jw', p=.1)[,1]}\cr
#' returns c(1,2,3,6,NA,NA) for dupls==FALSE, and c(1,2,6,3,NA,NA) for dupls==TRUE, as all are tied
#' @return For returnAs=="list", a list of length(x), with elements of length between length(nomatch) and \emph{maxmatch},
#' with indices of closest matches in table. \cr\cr
#' For returnAs=="matrix", an matrix of length(x) columns and \emph{maxmatch} rows (even if no elements have that many matches).
#' Non-matches are filled in with \emph{nomatch}.\cr\cr
#' In both cases, for ties the first match gets priority.
#' @seealso \code{\link[stringdist]{amatch}}
#'
#' @examples
#' set.seed(1)
#' x <- replicate(paste(letters[ceiling(runif(n = 20)*26)], collapse=''), n = 50)
#' table <- replicate(paste(letters[ceiling(runif(n = 20)*26)], collapse=''), n = 200)
#' normal_amatch <- stringdist::amatch(x, table, method='jw', p=.1, maxDist=.5)
#' multi_match <- mamatch(x, table, method='jw', p=.1, maxDist = .5, maxmatch=10, returnAs='matrix')
#' print(identical(normal_amatch, multi_match[1,]))
#' # What do the closest matches for number 1 look like?
#' print(x[1])
#' print(table[multi_match[,1]])
#'
#' @export
mamatch <- function (x, table, nomatch = NA, matchNA = TRUE, method = c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                                                                        "cosine", "jaccard", "jw", "soundex"), useBytes = FALSE, weight = c(d = 1, i = 1, s = 1, t = 1),
                     maxDist = 0.1, q = 1, p = 0, bt = 0, nthread = getOption("sd_num_thread"), maxmatch=10, limitMem=0, returnAs=c('matrix', 'list'), dupls=TRUE) {
  if(limitMem==0) limitMem <- length(x)*length(table)*8
  returnAs <- match.arg(returnAs)
  if(dupls) {
    maxmatch_orig <- min(maxmatch, length(table))
    table_orig <- table
    table <- unique(table)
    x_orig <- x
    x <- unique(x)
  }
  maxmatch <- min(maxmatch, length(table))
  if(returnAs=='matrix' && is.null(nomatch)) stop('nomatch can only be NULL if returnAs is "list"')
  if(returnAs=='matrix') {
    out <- matrix(nrow=maxmatch, ncol=length(x))
  } else {
    out <- vector('list', length(x))
  }
  for(i in 1:ceiling(8*length(table)*length(x)/limitMem)) {
    wh <- floor(limitMem/(8*length(table)))
    wh <- ((i-1)*wh+1):min(i*wh,length(x))
    mat <- stringdist::stringdistmatrix(a=table, b=x[wh], method=method, useBytes=useBytes, weight=weight, q=q, p=p, bt=bt, useNames='none', nthread=nthread)
    ret <- lapply(1:ncol(mat), function(j) {
      col <- mat[,j]
      ord <- sort(col, partial=1:maxmatch)[1:maxmatch]
      idcs <- lapply(ord[ord<=maxDist], function(o) {which(col==o)})
      idcs <- as.integer(unique(unlist(idcs)))[1:maxmatch] # As.integer to force NULLs to integer(0), and result to NAs
    })
    if(returnAs=='matrix') {
      ret <- simplify2array(ret)
      if(!is.na(nomatch))
        ret[is.na(ret)] <- nomatch
      out[,wh] <- ret
    }
    if(returnAs=='list') {
      ret <- lapply(ret, function(e) {
        if(is.na(e[1])) {
          return(nomatch)
        } else {
          return(e[!is.na(e)])
        }
      })
      out[wh] <- ret
    }
  }
  if(dupls) {
    # Matches are now with the unique-fied x and table, time to restore
    map <- fastmatch::fmatch(table_orig, table)
    map <- split(1:length(table_orig), map)
    if(returnAs=='matrix') {
      out <- apply(out, 2, function(e) {
        mt <- integer(0)
        i <- 1
        while(length(mt)<maxmatch_orig && !identical(e[i], as.integer(nomatch))) {
          mt <- c(mt, map[[e[i]]])
          i <- i+1
        }
        mt <- mt[1:maxmatch_orig]
        mt[is.na(mt)] <- nomatch
        return(mt)
      })
      out <- as.matrix(out[,fastmatch::fmatch(x_orig, x)])
    }
    if(returnAs=='list') {
      out <- lapply(out, function(e) {
        if(identical(e, nomatch)) return(e)
        mt <- integer(0)
        i <- 1
        while(length(mt)<maxmatch_orig && i<=length(e)) {
          mt <- c(mt, map[[e[i]]])
          i <- i+1
        }
        if(length(mt)>maxmatch_orig) mt <- mt[1:maxmatch_orig]
        return(mt)
      })
      out <- out[fastmatch::fmatch(x_orig, x)]
    }
  }
  return(out)
}
