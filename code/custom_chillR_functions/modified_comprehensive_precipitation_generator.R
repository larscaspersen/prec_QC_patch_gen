modified_ComprehensivePrecipitationGenerator <- function (station = c("T0001", "T0010", "T0099"), 
          prec_all, 
          mean_climate_prec = NULL, 
          year_max = 1990, 
          year_min = 1961, 
          leap = TRUE,
          nmonth = 12, 
          cpf = NULL, 
          verbose = TRUE,
          p = 1, 
          type = "none", 
          lag.max = NULL, 
          ic = "AIC", 
          activateVARselect = FALSE, 
          exogen = NULL,
          exogen_sim = NULL, 
          is_exogen_gaussian = FALSE, 
          year_max_sim = year_max, 
          year_min_sim = year_min,
          mean_climate_prec_sim = NULL, 
          onlygeneration = FALSE,
          varmodel = NULL,
          type_quantile = 3, 
          qnull = NULL, 
          valmin = 0.5, 
          step = 0, 
          n_GPCA_iteration = 0, 
          n_GPCA_iteration_residuals = n_GPCA_iteration, 
          sample = NULL, 
          extremes = TRUE, 
          exogen_all = NULL, 
          exogen_all_col = station, 
          no_spline = FALSE, 
          nscenario = 1, 
          seed = NULL, 
          noise = NULL) 
{
  # mean_climate_prec = NULL
  # leap = TRUE
  # nmonth = 12
  # cpf = NULL
  # verbose = TRUE
  # type = "none"
  # lag.max = NULL
  # ic = "AIC"
  # activateVARselect = FALSE
  # exogen = NULL
  # exogen_sim = NULL
  # is_exogen_gaussian = FALSE
  # onlygeneration = FALSE
  # varmodel = NULL
  # type_quantile = 3
  # qnull = NULL
  # valmin = 0.5
  # step = 0
  # extremes = TRUE
  # exogen_all = NULL
  # exogen_all_col = station
  # nscenario = 1
  # seed = NULL
  # noise = NULL
  
  
  useVAR = TRUE
  origin <- paste(year_min, "1", "1", sep = "/")
  origin_sim <- paste(year_min_sim, "1", "1", sep = "/")
  prec_mes <- as.data.frame(extractyears(prec_all, year_min = year_min, 
                                         year_max = year_max, station = station))
  nyear <- year_max - year_min + 1
  if (!is.monthly.climate(mean_climate_prec, nstation = length(station), 
                          nmonth = nmonth, verbose = verbose)) 
    mean_climate_prec <- getMonthlyMean(prec_all, year_min = year_min, 
                                        year_max = year_max, station = station)
  MEAN_CLIMATE_prec_SAVED <- mean_climate_prec
  prec_spline <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val = mean_climate_prec, 
                                                                              start_year = year_min, nyear = nyear, leap = leap, no_spline = no_spline))
  names(prec_spline) <- names(mean_climate_prec)
  if (min(prec_spline) <= 0) {
    
    
    prec_spline[prec_spline[,1] < 0,1] <- 0
    prec_spline[prec_spline[,2] < 0,2] <- 0
    
    print("Spline interpolation of daily precipitation less than 0!")
    # print("Error in Precipitation generator: spline interpolation of daily precipitation less than 0!")
    # return(-1)
  }
  data_prec <- normalizeGaussian_severalstations(x = prec_mes, 
                                                 data = prec_mes, sample = sample, cpf = cpf, step = step, 
                                                 origin_x = origin, origin_data = origin, extremes = extremes)
  if (!onlygeneration) {
    if (!is.null(exogen_all)) {
      exogen <- as.data.frame(extractyears(exogen_all, 
                                           year_min = year_min, year_max = year_max, station = exogen_all_col))
      is_exogen_gaussian = FALSE
    }
    if (is.null(exogen_sim)) 
      exogen_sim <- exogen
    if (!is_exogen_gaussian) {
      exogen0 <- exogen
      if (!is.null(exogen)) 
        exogen <- normalizeGaussian_severalstations(x = exogen0, 
                                                    data = exogen0, sample = sample, cpf = cpf, 
                                                    origin_x = origin, origin_data = origin)
    }
    var <- getVARmodel(data = data_prec, suffix = NULL, 
                       sep = "", p = p, type = type, exogen = exogen, lag.max = lag.max, 
                       ic = ic, activateVARselect = activateVARselect, 
                       n_GPCA_iteration_residuals = n_GPCA_iteration_residuals, 
                       n_GPCA_iteration = n_GPCA_iteration, extremes = extremes)
    if (activateVARselect) 
      return(var)
  }
  else {
    var <- varmodel
  }
  if (!is.null(noise)) {
    if (noise == "residuals") 
      noise <- residuals(var)
  }
  if (is.null(mean_climate_prec_sim)) 
    mean_climate_prec_sim <- mean_climate_prec
  nyear_sim <- year_max_sim - year_min_sim + 1
  nyear_max <- max(nyear_sim, nyear)
  prec_spline_sim <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val = mean_climate_prec_sim, 
                                                                                  start_year = year_min_sim, nyear = nyear_max, leap = leap, 
                                                                                  no_spline = no_spline))
  prec_spline_sim2 <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val = mean_climate_prec_sim, 
                                                                                   start_year = year_min_sim, nyear = nyear_sim, leap = leap, 
                                                                                   no_spline = no_spline))
  names(prec_spline_sim) <- colnames(mean_climate_prec_sim)
  names(prec_spline_sim2) <- colnames(mean_climate_prec_sim)
  if (is.null(exogen_sim)) 
    exogen_sim <- exogen
  if (!is.null(exogen_sim) & (!is_exogen_gaussian)) {
    exogen_sim0 <- exogen_sim
    exogen_sim <- normalizeGaussian_severalstations(x = exogen_sim0, 
                                                    data = exogen_sim0, sample = sample, cpf = cpf, 
                                                    origin_x = origin_sim, origin_data = origin_sim, 
                                                    extremes = extremes)
  }
  if (!is.null(seed)) 
    set.seed(seed)
  data_prec_gen <- newVARmultieventRealization(var, exogen = exogen_sim, 
                                               nrealization = nrow(prec_spline_sim2), extremes = extremes, 
                                               type = type_quantile, noise = noise)
  precrows <- 1:(min(c(nrow(prec_mes), nrow(prec_spline), 
                       nrow(prec_spline_sim))))
  prec_mes_rescaled <- prec_mes[precrows, ]/prec_spline[precrows, 
  ] * prec_spline_sim[precrows, ]
  prec_gen <- as.data.frame(normalizeGaussian_severalstations(x = data_prec_gen, 
                                                              data = prec_mes_rescaled, inverse = TRUE, type = type_quantile, 
                                                              step = step, sample = sample, origin_x = origin_sim, 
                                                              origin_data = origin, extremes = extremes))
  names(prec_gen) <- names(prec_spline_sim)
  colnames(data_prec_gen) <- names(prec_spline_sim)
  out <- NULL
  if (onlygeneration) {
    names_out <- c("prec_gen", "prec_spline_sim", "data_prec_gen", 
                   "mean_climate_prec_sim", "prec_mes", "prec_spline", 
                   "prec_mes_rescaled")
    for (it in names_out) {
      if (!exists(it)) 
        assign(it, NULL)
    }
    out <- list(prec_gen, prec_spline_sim, data_prec_gen, 
                mean_climate_prec_sim, prec_mes, prec_spline, prec_mes_rescaled)
    names(out) <- names_out
  }
  else {
    names_out <- c("prec_mes", "prec_spline", "data_prec", 
                   "prec_gen", "prec_spline_sim", "data_prec_gen", 
                   "mean_climate_prec", "mean_climate_prec_sim", "var")
    for (it in names_out) {
      if (!exists(it)) 
        assign(it, NULL)
    }
    out <- list(prec_mes, prec_spline, data_prec, prec_gen, 
                prec_spline_sim, data_prec_gen, mean_climate_prec, 
                mean_climate_prec_sim, var)
    names(out) <- names_out
  }
  if (nscenario > 1) {
    for (kk in 1:nscenario) {
      data_prec_gen <- newVARmultieventRealization(var, 
                                                   exogen = exogen_sim, nrealization = nrow(prec_spline_sim), 
                                                   extremes = extremes, type = type_quantile)
      colnames(data_prec_gen) <- names(prec_spline_sim)
      precrows <- 1:(min(c(nrow(prec_mes), nrow(prec_spline), 
                           nrow(prec_spline_sim))))
      prec_mes_rescaled <- prec_mes[precrows, ]/prec_spline[precrows, 
      ] * prec_spline_sim[precrows, ]
      prec_gen <- as.data.frame(normalizeGaussian_severalstations(x = data_prec_gen, 
                                                                  data = prec_mes_rescaled, inverse = TRUE, type = type_quantile, 
                                                                  step = step, sample = sample, origin_x = origin_sim, 
                                                                  origin_data = origin, extremes = extremes))
      names(prec_gen) <- names(prec_spline_sim)
      prec_index <- sprintf("prec_gen%05d", kk)
      out[[prec_index]] <- prec_gen
    }
  }
  return(out)
}
