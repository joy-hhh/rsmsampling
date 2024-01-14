#' RSM Orb MUS Sampling
#'
#' @param pop Population Dataset.
#' @param am Amount Column Name : Character.
#' @param SR Significant Risk : "Yes" or "No".
#' @param RC Reliance on Control : "Yes" or "No".
#' @param PL Planned Level of Assurance from Substantive Analytical Procedures : "Analytical.Procedures.Not.Performed"-4 , "High"-1 , "Low"-2, "Moderate"-3.
#' @param PM Tolerable Misstatement(generally performance materiality) : Integer number.
#' @param EA Expected misstatement : generally 0.05.
#'
#' @return Sampling result data.frame.
#' @export
#'
#' @examples
#' mus_sampling(pop, "CR", "Yes", "Yes", 4, 700000000, 0.05)
mus_sampling <- function(pop, am, SR, RC , PL, PM, EA){

  plas <- c("High", "Moderate", "Low", "Analytical.Procedures.Not.Performed")

  cat("Amount Column name :", am, "\n")
  cat("Significant Risk :", SR, "\n")
  cat("Reliance on Control :", RC, "\n")
  cat("Planned Level of Assurance from Substantive Analytical Procedures :", plas[PL], "\n")
  cat("Tolerable Misstatement :", PM, "\n")
  cat("Expected Misstatement Rate :", EA, "\n")

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
  cat("\n")
  cat("Assurance Factor Table :", "\n")
  print(assurance_factor_raw)

  assurance_factor_long <- stats::reshape(assurance_factor_raw,
                                   varying = c("High", "Moderate", "Low", "Analytical.Procedures.Not.Performed"),
                                   direction = "long",
                                   v.names = "Assurance_Factor"
  )

  AF <-  assurance_factor_long[assurance_factor_long[,"Significant.Risk"] == SR & assurance_factor_long[, "Reliance.on.Controls"] == RC & assurance_factor_long[, "time"] == PL,"Assurance_Factor"]

  cat("\n")
  cat(paste("Assurance Factor : ", AF))

  ## Sampling Interval = (Tolerable Misstatement – Expected Misstatement) / Assurance Factor
  sampling_interval = (PM - PM*EA) / AF

  ## Consideration of Zero or Negative Amounts
  pop <- pop[pop$amount > 0,]

  ## High Value
  high_value_items <- pop[pop$amount >= PM,]

  pop_remain <- pop[pop$amount < PM,]

  ## Expected Sample Size = (Population Subject to Sampling X Assurance Factor) / (Tolerable Misstatement – Expected Misstatement)
  pop_amount <- sum(pop_remain$amount)
  sample_size <- round(pop_amount * AF / (PM - PM*EA))

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


#' RSM Orb Monetary size Random Sampling
#'
#' @param pop Population Dataset.
#' @param am Amount Column Name : Character.
#' @param SR Significant Risk : "Yes" or "No".
#' @param RC Reliance on Control : "Yes" or "No".
#' @param PL Planned Level of Assurance from Substantive Analytical Procedures : "Analytical.Procedures.Not.Performed"-4 , "High"-1 , "Low"-2, "Moderate"-3.
#' @param PM Tolerable Misstatement(generally performance materiality) : Integer number.
#' @param EA Expected misstatement : generally 0.05.
#'
#' @return Sampling result data.frame.
#' @export
#'
#' @examples
#' m_ran_sampling(pop, "CR", "Yes", "Yes", 4, 700000000, 0.05)
m_ran_sampling <- function(pop, am, SR, RC , PL, PM, EA){

  plas <- c("High", "Moderate", "Low", "Analytical.Procedures.Not.Performed")

  cat("Amount Column name :", am, "\n")
  cat("Significant Risk :", SR, "\n")
  cat("Reliance on Control :", RC, "\n")
  cat("Planned Level of Assurance from Substantive Analytical Procedures :", plas[PL], "\n")
  cat("Tolerable Misstatement :", PM, "\n")
  cat("Expected Misstatement Rate:", EA, "\n")

  # m_ran_sampling 함수 실행을 위하여 금액 열 이름을 amount로 변경.
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

  cat("\n")
  cat("Assurance Factor Table :", "\n")
  print(assurance_factor_raw)

  assurance_factor_long <- stats::reshape(assurance_factor_raw,
                                          varying = c("High", "Moderate", "Low", "Analytical.Procedures.Not.Performed"),
                                          direction = "long",
                                          v.names = "Assurance_Factor"
  )

  AF <-  assurance_factor_long[assurance_factor_long[,"Significant.Risk"] == SR & assurance_factor_long[, "Reliance.on.Controls"] == RC & assurance_factor_long[, "time"] == PL,"Assurance_Factor"]

  cat("\n")
  cat(paste("Assurance Factor : ", AF))

  ## High Value
  high_value_items <- pop[pop$amount >= PM,]

  pop_remain <- pop[pop$amount < PM,]

  ## Expected Sample Size = (Population Subject to Sampling X Assurance Factor) / (Tolerable Misstatement – Expected Misstatement)
  pop_amount <- sum(pop_remain$amount)
  sample_size <- round(pop_amount * AF / (PM - PM*EA))

  sampling_row <- sample(seq(nrow(pop_remain)), sample_size , replace=F)
  sampling_row <- sort(sampling_row)

  ## 샘플링 객체 생성
  sampling_remain <- pop_remain[sampling_row,]
  sampling <- rbind(high_value_items, sampling_remain)
  return(sampling)
}


#' Random Sampling by input Sample Size
#'
#' @param pop Population Dataset.
#' @param n Sample Size.
#'
#' @return Sampling result data.frame.
#' @export
#'
#' @examples ran_sampling(pop, 25)
ran_sampling <- function(pop, n){
  sampling_row <- sample(seq(nrow(pop)), n, replace=F)
  sampling_row <- sort(sampling_row)

  ## 샘플링 객체 생성
  sampling <- pop[sampling_row,]
  return(sampling)
}



