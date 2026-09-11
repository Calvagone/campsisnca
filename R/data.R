#' Simulated Pharmacokinetic Dataset (Multiple-Dose Bolus)
#'
#' A simulated 2-compartment oral dataset containing 200 subjects administered
#' 1000 mg bolus doses every 24 hours for 7 days. Includes rich sampling on
#' Day 1 and Day 7, sparse sampling on intermediate days, and weight-based
#' allometric scaling on clearance.
#'
#' @format A tibble with 5,000 rows and 16 variables:
#' \describe{
#'   \item{ID}{Subject identifier (1–200)}
#'   \item{TIME}{Time after initial dose (hours)}
#'   \item{ARM}{Study arm identifier}
#'   \item{A_DEPOT}{Amount in the depot (absorption) compartment (mg)}
#'   \item{A_CENTRAL}{Amount in the central compartment (mg)}
#'   \item{A_PERIPHERAL}{Amount in the peripheral compartment (mg)}
#'   \item{A_OUTPUT}{Eliminated amount (mg)}
#'   \item{BW}{Body weight covariate (kg), sampled from Uniform(50, 100)}
#'   \item{CL}{Individual clearance (L/h), allometrically scaled with body weight}
#'   \item{V2}{Central volume of distribution (L)}
#'   \item{Q}{Inter-compartmental clearance (L/h)}
#'   \item{V3}{Peripheral volume of distribution (L)}
#'   \item{KA}{Absorption rate constant (1/h)}
#'   \item{CP}{True plasma concentration in central compartment (mg/L or mcg/mL)}
#'   \item{OBS_CP}{Observed plasma concentration with ~15.8\% proportional residual variability}
#'   \item{Y}{Observation variable (identical to \code{OBS_CP})}
#' }
#' @source Simulated using the \code{campsis} package (ADVAN4/TRANS4 model).
#' @examples
#' data(pk_bolus_md)
#' head(pk_bolus_md)
"pk_bolus_md"
