#' joineR
#' 
#' @description The joineR package implements methods for analyzing data from
#'   longitudinal studies in which the response from each subject consists of a
#'   time-sequence of repeated measurements and a possibly censored
#'   time-to-event outcome. The modelling framework for the repeated
#'   measurements is the linear model with random effects and/or correlated
#'   error structure (Laird and Ware, 1982). The model for the time-to-event
#'   outcome is a: Cox proportional hazards model with log-Gaussian frailty
#'   (Cox, 1972). A cause-specific hazards model is used when competing risks
#'   are present. Stochastic dependence is captured by allowing the Gaussian
#'   random effects of the linear model to be correlated with the frailty term
#'   of the Cox proportional hazards model. The methodology used to fit the
#'   model is described in Henderson et al. (2002) in the case of a single event
#'   time, and by Williamson et al. (2008) in the case of competing risks data.
#'   Both models exploit the general methodology proposed by Wulfsohn and
#'   Tsiatis (1997).
#' 
#' The package offers several types of functions for the analysis of joint data.
#' 
#' @section Data manipulation functions:
#'   
#'   There are several functions, including \code{jointdata},
#'   \code{sample.jointdata}, \code{subset.jointdata}, \code{to.balanced},
#'   \code{to.unbalanced}, and \code{UniqueVariables}, which offer the ability
#'   to construct a joint model dataset and manipulate it, e.g. take a sample
#'   according to a baseline covariate or outcome.
#'   
#' @section Plot functions:
#'   
#'   The plot function can be applied to \code{jointdata} and \code{vargm}
#'   (variogram) objects. In addition, \code{points} and \code{lines} can also
#'   be used with \code{jointplot} objects.
#'   
#' @section Model fitting functions:
#'   
#'   The primary function for fitting a joint model is \code{joint}. Standard
#'   errors can be estimated using \code{jointSE}.
#'   
#' @note Further details on the package are given in the vignette. To access
#'   this, run \code{vignette("joineR")}.
#'   
#' @references
#' 
#' Wulfsohn MS, Tsiatis AA. A joint model for survival and longitudinal data 
#' measured with error. \emph{Biometrics.} 1997; \strong{53(1)}: 330-339.
#' 
#' Henderson R, Diggle PJ, Dobson A. Joint modelling of longitudinal 
#' measurements and event time data. \emph{Biostatistics.} 2000; \strong{1(4)}: 
#' 465-480.
#' 
#' Cox DR. Regression models and life-tables. \emph{J R Stat Soc Ser B Stat
#' Methodol.} 1972; \strong{34(2)}: 187-220.
#' 
#' Laird NM, Ware JH. Random-effects models for longitudinal data.
#' \emph{Biometrics.} 1982; \strong{38(4)}: 963-974.
#' 
#' Williamson PR, Kolamunnage-Dona R, Philipson P, Marson AG. Joint modelling of
#' longitudinal and competing risks data. \emph{Stat Med.} 2008; \strong{27}:
#' 6426-6438.
#' 
#' @docType package
#' @name joineR
NULL