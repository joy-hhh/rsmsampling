# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' RSM Orb MUS Sampling
#'
#' @param SR Significant Risk : "Yes" or "No".
#' @param RC Reliance on Control : "Yes" or "No".
#' @param PL Planned Level of Assurance from Substantive Analytical Procedures : "Analytical.Procedures.Not.Performed" 4 , "High" 1 , "Low" 2, "Moderate" 3.
#' @param PM Tolerable Misstatement(generally performance materiality) : Integer number.
#' @param EA Expected misstatement : generally Tolerable Misstatement * 0.05.
#' @param pop Population Data.
#' @param am Amount Column Name : Character.
#'
#' @return sampling data.frame.
#' @export
#'
#' @examples
#' mus_sampling("Yes", "Yes", 4, 700000000, 0.05, pop, "amount")
mus_sampling <- function(SR, RC , PL, PM, EA, pop, am){

  # mus_sampling 함수 실행을 위하여 금액 열 이름을 amount로 변경.
  if(am != 'amount'){
    names(pop)[names(pop) == am] <- 'amount'
  }

  ## Assurance Factor 산정
  assurance_factor_raw <- data.frame(
    Significant.Risk = c("Yes", "No","Yes","No"),
    Reliance.on.Controls = c("No","No","Yes","Yes"),
    High = c(1.1, 0, 0, 0),
    Moderate = c(1.6, 0.5, 0.2, 0),
    Low = c(2.8, 1.7, 1.4, 0.3),
    Analytical.Procedures.Not.Performed = c(3, 1.9, 1.6, 0.5)
  )

  assurance_factor_long <- stats::reshape(assurance_factor_raw,
                                   varying = c("High", "Moderate", "Low", "Analytical.Procedures.Not.Performed"),
                                   direction = "long",
                                   v.names = "Assurance_Factor"
  )

  AF <-  assurance_factor_long[assurance_factor_long[,"Significant.Risk"] == SR & assurance_factor_long[, "Reliance.on.Controls"] == RC & assurance_factor_long[, "time"] == PL,"Assurance_Factor"]

  ## Sampling Interval = (Tolerable Misstatement – Expected Misstatement) / Assurance Factor
  sampling_interval = (PM - EA) / AF

  ## Consideration of Zero or Negative Amounts
  pop <- pop[pop$amount > 0,]

  ## High Value
  high_value_items <- pop[pop$amount >= PM,]

  pop_remain <- pop[pop$amount < PM,]

  ## Expected Sample Size = (Population Subject to Sampling X Assurance Factor) / (Tolerable Misstatement – Expected Misstatement)
  pop_amount <- sum(pop_remain$amount)
  sample_size <- round(pop_amount * AF / (PM- EA))

  sampling_row <- seq(sample_size)
  sampling_n <- seq(sample_size) * sampling_interval

  pop_remain$cum <- cumsum(pop_remain$amount)

  for (i in seq_along(sampling_n)) {
    sampling_row[i] <- which(pop_remain$cum > sampling_n[i])[1]
  }

  sampling_row <- unique(sampling_row)

  ## 샘플링 객체 생성
  sampling_remain <- pop_remain[sampling_row,]
  sampling_remain <- sampling_remain[,!names(sampling_remain) %in% "cum"]
  sampling <- rbind(high_value_items, sampling_remain)
  return(sampling)
}


