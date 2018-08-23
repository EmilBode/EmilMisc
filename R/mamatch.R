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
#' @param limitMem Limit memory usage to approx. this many bytes, plus amount needed for return value. \cr
#' Internally, this script calls stringdistmatrix, which means potentially a large amount of memory is needed
#' (length(x) X length(table) X 8 bytes). You can provide this parameter to chunk processing, limiting memory usage to
#' ~ limitMem + returnvalue bytes. 0 (default) means unlimited, otherwise size in bytes.
#' @param returnAs comparable to simplify in sapply: should result be returned as a list or an array? \cr
#' "array" fills any non-matches with \emph{nomatch}, so it always returns an array (even when sapply wouldn't)
#' @return For returnAs=="list", a list of length(x), with elements of length between length(nomatch) and \emph{maxmatch},
#' with indices of closest matches in table. \cr\cr
#' For returnAs=="array", an array of length(x) columns and \emph{maxmatch} rows (even if no elements have that many matches).
#' Non-matches are filled in with \emph{nomatch}.\cr\cr
#' In both cases, for ties the first match gets priority.
#' @seealso \code{\link[stringdist]{amatch}}
#'
#' @examples
#' set.seed(1)
#' x <- replicate(paste(letters[ceiling(runif(n = 20)*26)], collapse=''), n = 50)
#' table <- replicate(paste(letters[ceiling(runif(n = 20)*26)], collapse=''), n = 200)
#' normal_amatch <- stringdist::amatch(x, table, method='jw', p=.1, maxDist=.5)
#' multi_match <- mamatch(x, table, method='jw', p=.1, maxDist = .5, maxmatch=10, returnAs='array')
#' print(identical(normal_amatch, multi_match[1,]))
#' # What do the closest matches for number 1 look like?
#' print(x[1])
#' print(table[multi_match[,1]])
#'
#' @export
mamatch <- function (x, table, nomatch = NA, matchNA = TRUE, method = c("osa", "lv", "dl", "hamming", "lcs", "qgram",
                                                                        "cosine", "jaccard", "jw", "soundex"), useBytes = FALSE, weight = c(d = 1, i = 1, s = 1, t = 1),
                     maxDist = 0.1, q = 1, p = 0, bt = 0, nthread = getOption("sd_num_thread"), maxmatch=10, limitMem=0, returnAs=c('array', 'list')) {
  if(limitMem==0) limitMem <- length(x)*length(table)*8
  maxmatch <- min(maxmatch, length(table))
  returnAs <- match.arg(returnAs)
  if(returnAs=='array' && is.null(nomatch)) stop('nomatch can only be NULL if returnAs is "list"')
  if(returnAs=='array') {
    out <- array(dim=c(maxmatch, length(x)))
  } else {
    out <- vector('list', length(x))
  }
  for(i in 1:ceiling(8*length(table)*length(x)/limitMem)) {
    wh <- limitMem/(8*length(table))
    wh <- ((i-1)*wh+1):min(i*wh,length(x))
    mat <- stringdist::stringdistmatrix(a=table, b=x[wh], method=method, useBytes=useBytes, weight=weight, q=q, p=p, bt=bt, useNames='none', nthread=nthread)
    ret <- apply(mat, 2, function(col) {
      ord <- sort(col, partial=1:maxmatch)[1:maxmatch]
      idcs <- lapply(ord[ord<=maxDist], function(o) {which(col==o)})
      idcs <- as.integer(unique(unlist(idcs)))[1:maxmatch]
      if(returnAs=='array') {
        if(!is.na(nomatch))
          idcs[is.na(idcs)] <- nomatch
      } else {
        idcs <- as.list(idcs[!is.na(idcs)])
        if(identical(idcs, list())) idcs <- nomatch
      }
      return(idcs)
    })
    if(returnAs=='list') {
      out[wh] <- lapply(ret, unlist)
    } else {
      out[,wh] <- ret
    }
  }
  return(out)
}
