#' Mental health trial data
#' 
#' @description The data is obtained from a trial in which chronically ill
#'   mental health patients were randomized across two treatments: placebo and
#'   an active drug. A questionnaire instrument was used to assess each
#'   patient's mental state at weeks 0, 1, 2, 4, 6 and 8 post-randomisation, a
#'   high recorded score implying a severe condition. Some of the 100 patients
#'   dropped out of the study for reasons that were thought to be related to
#'   their mental state, and therefore potentially informative; others dropped
#'   out for reasons unrelated to their mental state.
#' 
#' @usage data(mental)
#' @format A balanced data set with respect to the times at which observations 
#'   recorded. The data consists of the following variables on each patient:
#'   
#'   \describe{
#'   
#'   \item{\code{id}}{integer: patient identifier.}
#'   
#'   \item{\code{Y.t0}}{integer: mental state assessment in week 0. Coded
#'   \code{NA} if missing.}
#'   
#'   \item{\code{Y.t1}}{integer: mental state assessment in week 1. Coded
#'   \code{NA} if missing.}
#'   
#'   \item{\code{Y.t2}}{integer: mental state assessment in week 2. Coded
#'   \code{NA} if missing.}
#'   
#'   \item{\code{Y.t4}}{integer: mental state assessment in week 4. Coded
#'   \code{NA} if missing.}
#'   
#'   \item{\code{Y.t6}}{integer: mental state assessment in week 6. Coded
#'   \code{NA} if missing.}
#'   
#'   \item{\code{Y.t8}}{integer: mental state assessment in week 8. Coded
#'   \code{NA} if missing.}
#'   
#'   \item{\code{treat}}{integer: treatment allocation. Coded as \code{0 =
#'   }placebo; \code{1 = }active drug.}
#'   
#'   \item{\code{n.obs}}{integer: number of non-missing mental state 
#'   assessments.}
#'   
#'   \item{\code{surv.time}}{numeric: imputed dropout time in weeks. Coded as
#'   \code{surv.time = 8.002} for completers.}
#'   
#'   \item{\code{cens.ind}}{integer: censoring indicator. Coded as \code{0 =
#'   }completer or non-informative dropout; \code{1 = }potentially informative
#'   dropout.}
#'   
#'   }
#' @keywords datasets
#' @seealso \code{\link{heart.valve}}, \code{\link{liver}}, 
#'   \code{\link{epileptic}}.
#' @source Peter J. Diggle (p.diggle@@lancaster.ac.uk)
#' @docType data
#' @references
#' 
#' Henderson R, Diggle PJ, Dobson A. Joint modelling of longitudinal 
#' measurements and event time data. \emph{Biostatistics.} 2000; \strong{1(4)}: 
#' 465-480.
#' 
#' Diggle PJ, Farewell D, Henderson R. Longitudinal data with dropout:
#' objectives, assumptions and a proposal (with Discussion). \emph{Applied
#' Statistics.} 2007; \strong{56}: 499-550.
"mental"



