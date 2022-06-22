#' Generic function for obtaining choices for dropdowns 
#'
#' @param dataset Data frame containing data
#' @param lable Dropdown label, used to construct "All XXX" option
#' @return Reactive character array
get_field <- function(dataset,label) {
  return(
    reactive({
      uniques <- unlist(unique( dataset()[[label]] ))
      if (length(uniques) == 0) {
        vals <- c('Not Applicable')
      } else if (length(uniques) == 1) {
        vals <- uniques
      } else {
        if (label == "StationCode" || label == "Parameter") {
          vals <- c(sprintf('Select %s', label), uniques )
        } else {
          vals <- c(sprintf('All %s', label), uniques )
        }
      }
      vals
    })
  )
}

#' Generic function for filtering data using unique column values.
#'
#' @param dataset Data frame containing data
#' @param lable Dropdown label, used to construct "All XXX" option
#' @param value Column value for filtering
#' @return Reactive data frame
filter_data <- function(dataset,label,value){
  return(
    reactive({
      if (grepl("All ",value()) || grepl("Select ",value())){
        # Return unfiltered dataset
        dataset
      } else {
        # Filter dataset
        dataset()[dataset()[label] == value(),]
      }
    })
  )
}

#' Check for a column name with value "Fraction".
#'
#' @param dataset Data frame containing data
#' @return Reactive reactive boolean value
has_fraction <- function(dataset){
  return(
    reactive({
      "Fraction" %in% colnames(dataset())
    })
  )
}

#' Fitlers dataset by unique Watershed value
#'
#' @param dataset Data frame containing data
#' @param value Column value for filtering
#' @return Reactive data frame
filter_by_watershed <- function(dataset, value){
  return(
    reactive({
      if (grepl("All ",value()) || grepl("Not Applicable",value())){
        dataset()
      } else {
        dataset()[dataset()$Watershed == value(),]
      }
    })
  )
}

#' Fitlers dataset by unique Station value
#'
#' @param dataset Data frame containing data
#' @param value Column value for filtering
#' @return Reactive data frame
filter_by_station <- function(dataset,value){
  return(
    reactive({
      if (grepl("All ",value()) || grepl("Select ",value()) || grepl("Not Applicable",value())){
        dataset()
      } else {
        dataset()[dataset()$StationCode == value(),]
      }
    })
  )
}

#' Fitlers dataset by unique Parameter value
#'
#' @param dataset Data frame containing data
#' @param value Column value for filtering
#' @return Reactive data frame
filter_by_parameter <- function(dataset,value){
  return(
    reactive({
      if (grepl("All ",value()) || grepl("Select ",value()) || grepl("Not Applicable",value())){
        dataset()
      } else {
        dataset()[dataset()$Parameter == value(),]
      }
    })
  )
}

#' Fitlers dataset by unique Unit value
#'
#' @param dataset Data frame containing data
#' @param value Column value for filtering
#' @return Reactive data frame
filter_by_unit <- function(dataset,value){
  return(
    reactive({
      if (grepl("All ",value()) || grepl("Not Applicable",value())){
        dataset()
      } else {
        dataset()[dataset()$Units == value(),]
      }
    })
  )
}

#' Fitlers dataset by unique Unit value
#'
#' @param dataset Data frame containing data
#' @param value Column value for filtering
#' @return Reactive data frame
filter_by_fraction <- function(dataset,value){
  return(
    reactive({
      if (grepl("All ",value()) || grepl("Not Applicable",value())){
        # print(dataset())
        dataset()
      } else {
        dataset()[dataset()$Fraction == value(),]
      }
    })
  )
}

# simulate time series given annual, seasonal variance component of input data
# chg is desired annual change trend
# eff is sample effort as proportion from input data
# sims is numer of time series to simulate
simvals_opt <- function(powdat, resdvar, chg = 0.5, eff = 1, sims = 100){
  
  # LF: Original code -------------
  # total obs, simulation effort
  # ntot <- nrow(powdat)
  # simeff <- round(ntot * eff, 0)

  # LF: Revised N -----------------
  num_years <- ceiling(interval(min( powdat$Date ), max( powdat$Date )) / years(1))
  ntot <- nrow(powdat)
  n_per_year <- ntot / num_years
  ntot_sim <- floor(n_per_year * 10)
  simeff <- round(ntot_sim * eff, 0)
  #--------------------------------

  # estimate annual linear trend given actual signal
  # chg is desired change from starting value
  # strt is starting value, arbitrary
  # a is rate of change per step
  # ntot is total obs to estimate
  # tot is vector of values to estimate
  # N is the result
  No <- median(log(1 + powdat$Result), na.rm = T)
  a <- -1 * chg * No / simeff
  tot <- 1:simeff
  N <- No + tot * a - a
  
  # base simulation to add to total annual change, scaled by desired effort
  # seasonal component from gam plus rnorm terms for variance stochasticity
  dtrng <- range(powdat$Date)
  start_date <- as.Date(as.character(dtrng[1]), tryFormats = c("%Y-%m-%d","%m/%d/%y"))
  # end_date <- as.Date(as.character(dtrng[2]), tryFormats = c("%Y-%m-%d","%m/%d/%y"))
  end_date <- start_date + years(10)
  basedts <- seq.Date(start_date, end_date, length.out = simeff)
  basedts <- data.frame(
    Date = basedts, 
    Year = year(basedts), 
    Season = yday(basedts), 
    Month = month(basedts),
    dectime = decimal_date(basedts)
  )
  
  # seasonal component from model
  # seascmp <- predict(modin, type = 'terms', exclude = 'Year', newdata = basedts) %>% 
  #   as.numeric
  # trndcmp <- predict(modin, newdata = basedts)
  
  # simdat
  out <- basedts %>% 
    mutate(
      # trndcmp = trndcmp#,
      annscmp = N#,
      # seascmp = seascmp
    ) %>% 
    crossing(
      sims = 1:sims
    ) %>% 
    mutate(
      simresd = rnorm(simeff * sims, 0, resdvar),
      # simseas = rnorm(simeff * sims, 0, seasvar),
      # simyear = rnorm(simeff * sims, 0, yearvar),
      # simrand = annscmp + seascmp + simresd + simseas + simyear
      simrand = annscmp + simresd
    ) %>% 
    arrange(sims, Date)
  
  return(out)
  
}

# simulate time series given annual, seasonal variance component of input data
# chg is desired annual change trend
# eff is sample effort as proportion from input data
# sims is numer of time series to simulate
simvals <- function(powdat, chg = 0.5, eff = 1, sims = 100){
  
  # LF: Original code -------------
  # total obs, simulation effort
  ntot <- nrow(powdat)
  simeff <- round(ntot * eff, 0)

  # LF: Revised N -----------------
  # num_years <- interval(min( powdat$Date ), max( powdat$Date )) / years(1)
  # ntot <- nrow(powdat)
  # n_per_year <- ntot / num_years
  # ntot_sim <- round(n_per_year * 10)
  # simeff <- round(ntot_sim * eff, 0)
  #--------------------------------
  
  # add year, season
  powdat <- powdat %>%
    mutate(
      Year = year(Date), 
      Season = yday(Date),
      dectime = as.numeric(decimal_date(Date))
    )
    
  # model to estimate variance components
  # modin <- gam(log(Result) ~ Year + s(Season, bs = 'cc'), data = powdat)
  modin <- lm(log(1 + Result) ~ dectime, data = powdat)

  # total variation is the sum of annual, seasonal, and residual variation
  resdvar <- resid(modin) %>% var
  # seasvar <- gam.vcomp(modin, rescale = F)[[1]]
  # yearvar <- (summary(modin)$se[['Year']] * sqrt(ntot)) ^ 2

  # estimate annual linear trend given actual signal
  # chg is desired change from starting value
  # strt is starting value, arbitrary
  # a is rate of change per step
  # ntot is total obs to estimate
  # tot is vector of values to estimate
  # N is the result
  No <- median(log(1 + powdat$Result), na.rm = T)
  a <- -1 * chg * No / simeff
  tot <- 1:simeff
  N <- No + tot * a - a
  
  # base simulation to add to total annual change, scaled by desired effort
  # seasonal component from gam plus rnorm terms for variance stochasticity
  dtrng <- range(powdat$Date)
  start_date <- as.Date(as.character(dtrng[1]), tryFormats = c("%Y-%m-%d","%m/%d/%y"))
  # end_date <- as.Date(as.character(dtrng[2]), tryFormats = c("%Y-%m-%d","%m/%d/%y"))
  end_date <- start_date + years(10)
  basedts <- seq.Date(start_date, end_date, length.out = simeff)
  basedts <- data.frame(
    Date = basedts, 
    Year = year(basedts), 
    Season = yday(basedts), 
    Month = month(basedts),
    dectime = decimal_date(basedts)
  )
  
  # seasonal component from model
  # seascmp <- predict(modin, type = 'terms', exclude = 'Year', newdata = basedts) %>% 
  #   as.numeric
  # trndcmp <- predict(modin, newdata = basedts)
  
  # simdat
  out <- basedts %>% 
    mutate(
      # trndcmp = trndcmp#,
      annscmp = N#,
      # seascmp = seascmp
    ) %>% 
    crossing(
      sims = 1:sims
    ) %>% 
    mutate(
      simresd = rnorm(simeff * sims, 0, resdvar),
      # simseas = rnorm(simeff * sims, 0, seasvar),
      # simyear = rnorm(simeff * sims, 0, yearvar),
      # simrand = annscmp + seascmp + simresd + simseas + simyear
      simrand = annscmp + simresd
    ) %>% 
    arrange(sims, Date)
  
  return(out)
  
}

# get power estimates for seasonal kendall using output form simvals function
powfun <- function(simdat, alpha = 0.05){

  # print(summary(simdat))
  
  powest <- simdat %>% 
    group_by(sims) %>% 
    nest %>% 
    mutate(
      pval = purrr::map(data, function(x){
        
        # Construct linear model, gather F-statistic information
        mod <- lm(simrand ~ dectime, data = x) %>% summary %>% .$fstatistic

        # Calculate area under the curve for the f-distribution
        pf(mod[1], mod[2], mod[3], lower.tail = F)
        
      }),
    ) %>% 
    select(-data) %>% 
    unnest(pval)
  
  pow <- sum(powest$pval < alpha) / nrow(powest)
  
  return(pow)
  
}

# optimal sample effort
# datin is power results for one station, one parameter
# pow is desired level of power
getopt <- function(datin, pow = 0.5){
  
  p <- ggplot(datin) +
    geom_contour(aes(x = eff, y = chg, z = pow), breaks = pow) 
    # geom_contour(aes(x = eff, y = chg, z = pow))
  
  dat <- ggplot_build(p)$data[[1]]
  
  if(nrow(dat) == 0) {
    print("getopt: nrow(dat) == 0")
    return(NA)
  }
    
  
  # check if multiple pieces
  if(length(unique(dat$piece)) > 1){
    print("getopt: Multiple pieces.")
    return(NA)
  }
    
  
  dat <- dat %>% 
    arrange(x)
  
  # check if not monotonic
  chk <- diff(dat$y)
  if(any(sign(chk) == 1)){
    print("getopt: not monotonic")
    return(NA)
  }
    
  
  slopey <- diff(dat$y) / diff(dat$x)
  slopex <- diff(dat$x) / diff(dat$y)
  loc <- which(slopey > slopex)[1]
  
  out <- dat[loc + 1, ] %>% 
    dplyr::select(eff = x, chg = y)
  
  return(out)
  
}

getopt_i <- function(datin, pow = 0.5){
  
  p <- ggplot(datin) +
    geom_contour(aes(x = eff, y = chg, z = pow), breaks = pow) 
    # geom_contour(aes(x = eff, y = chg, z = pow))
  
  dat <- ggplot_build(p)$data[[1]]
  
  if(nrow(dat) == 0) {
    print("getopt: nrow(dat) == 0")
    return(NA)
  }
    
  
  # check if multiple pieces
  if(length(unique(dat$piece)) > 1){
    print("getopt: Multiple pieces.")
    return(NA)
  }
    
  
  dat <- dat %>% 
    arrange(x)
  
  # check if not monotonic
  chk <- diff(dat$y)
  if(any(sign(chk) == 1)){
    print("getopt: not monotonic")
    return(NA)
  }
    
  
  slopey <- diff(dat$y) / diff(dat$x)
  slopex <- diff(dat$x) / diff(dat$y)
  loc <- which(slopey > slopex)[1]
  
  return(loc + 1)
  
}

# Contour data
# datin is power results for one station, one parameter
# contour_data is the countour line at specified power level.
get_contour_data <- function(datin, pow = 0.5){
  
  p <- ggplot(datin) +
    geom_contour(aes(x = eff, y = chg, z = pow), breaks = pow) 
    # geom_contour(aes(x = eff, y = chg, z = pow))
  
  dat <- ggplot_build(p)$data[[1]]
  
  if(nrow(dat) == 0) {
    print("get_countor_data: nrow(dat) == 0")
    return(NA)
  }
    
  
  # check if multiple pieces
  if(length(unique(dat$piece)) > 1){
    print("get_countor_data: Multiple pieces.")
    return(NA)
  }
    
  
  dat <- dat %>% 
    arrange(x)
  
  # check if not monotonic
  chk <- diff(dat$y)
  if(any(sign(chk) == 1)){
    print("get_countor_data: not monotonic")
    return(NA)
  }

  out <- dat %>% 
    dplyr::select(eff = x, chg = y)
  
  return(out)
  
}
