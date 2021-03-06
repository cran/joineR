#' Sample from a \code{jointdata} x
#' 
#' @description Generic function used to sampling a subset of data from an x of
#'   class \code{jointdata}, with a specific size of number of subjects.
#' 
#' @param x an object of class \code{jointdata}.
#' @param size number of subjects to include in the sampled subset.
#' @param replace should sampling be with replacement? Default is \code{replace
#'   = TRUE}.
#'   
#' @author Ines Sousa
#' @seealso \code{\link{sample}}, \code{\link{jointdata}}, 
#'   \code{\link{UniqueVariables}}.
#' @keywords data
#'   
#' @return An object of class \code{jointdata}, with data only on the subjects 
#'   sampled.
#' @export
#' 
#' @examples
#' data(heart.valve)
#' heart.surv <- UniqueVariables(heart.valve, 
#'                               var.col = c("fuyrs", "status"), 
#'                               id.col = "num")
#' heart.valve.jd <- jointdata(survival = heart.surv, 
#'                             id.col = "num", 
#'                             time.col = "time")
#' sample.jointdata(heart.valve.jd, size = 10)
sample.jointdata <- function(x, size, replace = FALSE) {
  
  if (!inherits(x, "jointdata")) {
    stop("Data must be of class 'jointdata'\n")
  }
  
  origid <- x$subject
  id <- sample(x$subject, size = size, replace = replace)
  re <- x
  
  if (replace) { # with replacement
    re$subject <- re$subject[match(id, re$subject)]
    if (is.data.frame(re$longitudinal)) {
      nn <- diff(match(unique(x$longitudinal[, 1]), x$longitudinal[, 1]))
      nn[length(nn) + 1] <- length(x$longitudinal[, 1]) - sum(nn)
      mid <- match(id, origid)
      geti <- match(id, re$longitudinal[[re$subj.col]])
      getirep <- rep(geti, nn[mid])
      ninc <- sequence(nn[mid]) - 1
      lid <- getirep + ninc
      re$longitudinal <- re$longitudinal[lid, ]
      re$longitudinal[[re$subj.col]] <- rep(1:length(id), nn[mid])
      row.names(re$longitudinal) <- 1:(dim(re$longitudinal)[1])
    }
    if (is.data.frame(re$survival)) {
      re$survival <- re$survival[mid, ]
      re$survival[[re$subj.col]] <- 1:length(id)
      row.names(re$survival) <- 1:(dim(re$survival)[1])
    }
    if (is.data.frame(re$baseline)) {
      re$baseline <- re$baseline[mid, ]
      re$baseline[[re$subj.col]] <- 1:length(id)
      row.names(re$baseline) <- 1:(dim(re$baseline)[1])
    }
  } else { # without replacement
    re$subject <- re$subject[re$subject %in% id]
    if (is.data.frame(re$longitudinal)) {
      re$longitudinal <- re$longitudinal[re$longitudinal[[re$subj.col]] %in% id, ]
      row.names(re$longitudinal) <- 1:(dim(re$longitudinal)[1])
    }
    if (is.data.frame(re$survival)) {
      re$survival <- re$survival[re$survival[[re$subj.col]] %in% id, ]
      row.names(re$survival) <- 1:(dim(re$survival)[1])
    }
    if (is.data.frame(re$baseline)) {
      re$baseline <- re$baseline[re$baseline[[re$subj.col]] %in% id, ]
      row.names(re$baseline) <- 1:(dim(re$baseline)[1])
    }
  }
  
  class(re) <- c("jointdata", "list")
  return(re)
  
}
