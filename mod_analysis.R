#' Primary function for trend simulation and power analysis.
#' 
#' @param dataset Filtered dataset
#' @return List containing optimum values, power data and contour data.
get_effort_data <- function(dataset, session, options){
  reactive({
    data <- NULL

    df <- dataset$filtered_by_fraction()

    if (length(df$Result) > options$min_n() && length(unique(df$StationCode)) == 1 && length(unique(df$Parameter)) == 1 && var(df$Result) > options$min_variance()){
      # Set parameters
      sig_level <- 0.05
      power <- 0.8
      n_simulations <- 1000

      # Get dataset
      # df <- dataset

      # Update Date and Result columns
      df$Date <- as.Date(as.character(df$Date), tryFormats = c("%Y-%m-%d","%m/%d/%y"))
      df$Result <- as.numeric(df$Result)
      df$Detrended_Results <- detrend(df$Result, 'linear')

      scns <- crossing(
        sta = unique(df$StationCode),
        par = unique(df$Parameter),
        chg = seq(0.1, 1, length = 10),
        eff = seq(0.1, 2,length = 10), 
      )
      
      strt <- Sys.time()

      power_data <- df %>% 
          arrange(Date)

      # Updating powdat
      # add year, season
      power_data <- power_data %>%
        mutate(
          Year = year(Date), 
          Season = yday(Date),
          dectime = as.numeric(decimal_date(Date))
        )

      # Linear Model
      # model to estimate variance components
      modin <- lm(log(1 + Result) ~ dectime, data = power_data)

      # Residuals
      # total variation is the sum of annual, seasonal, and residual variation
      resdvar <- resid(modin) %>% var


      # print("starting simulations")
      res <- foreach(i = 1:nrow(scns), .packages = c('lubridate', 'tidyverse', 'mgcv')) %do% {

        # print(Sys.time()-strt)
      
        sta <- scns[i, ][['sta']]
        par <- scns[i, ][['par']]
        chg <- scns[i, ][['chg']]
        eff <- scns[i, ][['eff']]
        
        # print("Simulating Vals...")
        # print(summary(topow))
        simdat <- try({simvals_opt(power_data, resdvar, chg = chg, eff = eff, sims = n_simulations)}) 
        
        if(inherits(simdat, 'try-error'))
          return(NA)

        out <- try({powfun(simdat)})
        
        if(inherits(out, 'try-error'))
          return(NA)
        
        return(out)
        
      }

      # combine results with scns
      pows <- scns %>% 
        mutate(
          pow = unlist(res)
        )

      # Identify optimal point values
      tryCatch({
          opts <- pows %>% 
            group_by(sta) %>% 
            nest %>% 
            mutate(
              opt = purrr::map(data, getopt, pow = as.numeric(power))
            ) %>% 
            dplyr::select(-data) %>% 
            unnest(opt) %>% 
            dplyr::select(sta, eff, chg) %>% 
            na.omit %>% 
            ungroup
        },
        error = function(e){
          return(NA)
        }
      )

      # Obtain contour data
      tryCatch({
          contour_data <- pows %>% 
            group_by(sta) %>% 
            nest %>% 
            mutate(
              cont = purrr::map(data, get_contour_data, pow = as.numeric(power))
            ) %>% 
            dplyr::select(-data) %>% 
            unnest(cont) %>% 
            dplyr::select(sta, eff, chg) %>% 
            na.omit %>% 
            ungroup
        },
        error = function(e){
          return(NA)
        }
      )

      # simulation_complete <<- FALSE
      current_effort_index <- which.min(abs(1-contour_data$eff))
      # pos <- round(contour_data$chg[current_effort_index]*100, digits = 2)
      current_effort <- contour_data$eff[current_effort_index]
      updateSliderInput(session, "mod_analysis_gauge_effort_n_input-val", value = current_effort)

      # print("contour_data=")
      # print(contour_data)

      # Return list containing optimal, power, and contour data
      data <- list(
        opts = opts,
        pows = pows,
        contours = contour_data
      )

      
    }

    return(data)

  })
}

#' Effort analysis chart UI module layout
#' @param id Namespace ID for scoping
#' @return Namespaced UI elements
mod_effort_analysis_chart_ui <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(color="#0dc5c1", plotlyOutput(ns("effort_chart")))
    # plotlyOutput(ns("effort_chart"))
  ) 
}

#' Effort analysis chart server logic
#' @param id Namespace ID for scoping
#' @param dataset Filtered dataset
#' @param data Calculated values from simulation
#' @param options User entered options
#' @return ggplot object
mod_effort_analysis_chart_server <- function(id, dataset, data, options, validation_messages){
  moduleServer(id, function(input, output, session) {
    
    output$effort_chart <- renderPlotly({
      # Validation checks
      validate(
        need(length(dataset()$Result) > options$min_n(), validation_messages["too_few"])
      )
      validate(
        need(length(unique(dataset()$StationCode)) == 1, validation_messages["station"])
      )
      validate(
        need(var(dataset()$Result) > options$min_variance(), validation_messages["no_variance"])
      )
      validate(
        need(nrow(data$effort()$opts) > 0, validation_messages["no_opts"])
      )

      df <- dataset()
      user_n <- as.numeric(options$n_per_year())
      user_effort <- user_n / length(df$Result)

      # Get currently selected effort
      selected_effort <- as.numeric(options$n_per_year())

      power_data <- data$effort()$pows
      optimum <- data$effort()$opts


      thm3 <- theme_bw(base_size = 12) + 
        theme(
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          legend.position = 'bottom', 
          panel.grid = element_blank()
        )

      p <- ggplot(power_data) + ylim(0,1.0) +
        scale_y_continuous(labels = scales::percent) +
        geom_contour(aes(x = eff, y = chg, z = pow), breaks = as.numeric(0.8), size = 0.5) +
        geom_vline(xintercept=optimum$eff, colour="#228b22") +
        geom_point(data = optimum, aes(x = eff, y = chg, size = 10), pch = 21, fill = "#228b22", colour = 'black') +
        geom_vline(xintercept=selected_effort, colour="blue") +
        geom_vline(xintercept=1.0, colour="red", linetype="dotted") +
        scale_size_continuous(guide = F, trans = 'reverse', limits = c(NA, 0)) + 
        scale_x_continuous(limits = c(0.1, 2)) + 
        thm3 +
        labs(
          x = 'Relative sample effort', 
          y = paste0('Magnitude of trend change (%)')
        )

      ggplotly(p)
    })
  })
}

#' Basic gauge plot UI module layout
#' Plot obtained from https://plotly.com/r/gauge-charts/
#' @param id Namespace ID for scoping
#' @return Namespaced UI elements
mod_analysis_gauge_effort_ui <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(color="#0dc5c1", plotOutput(ns("gauge")))
  ) 
}

#' Complex gauge plot server logic
#' Plot obtained from https://plotly.com/r/gauge-charts/
#' Gradient colors obtained from https://colordesigner.io/gradient-generator
#' @param id Namespace ID for scoping
#' @return Module server object
mod_analysis_gauge_effort_server <- function(id, data, is_user_n, breaks, options, validation_messages){
  moduleServer(id, function(input, output, session) {
    
    #https://stackoverflow.com/questions/24900903/how-to-draw-gauge-chart-in-r
    #https://www.r-bloggers.com/2020/10/multiple-gauge-plots-with-facet-wrap/
    gg.gauge <- function(pos,n,breaks=c(0,30,70,100)) {
      colors <- c("#228b22","#2a8d21","#319020","#37921f","#3d941d","#43961c","#49981b","#4e9b19","#549d18","#599f16","#5fa114","#64a313","#69a511","#6fa80e","#74aa0c","#79ac09","#7eae07","#84b004","#89b201","#8eb400","#94b600","#99b800","#9fba00","#a4bc00","#a9be00","#afbf00","#b4c100","#bac300","#c0c500","#c5c700","#cbc800","#d0ca00","#d6cc00","#dcce00","#e2cf00","#e7d100","#edd200","#f3d400","#f9d500","#ffd700")
      colors <- c( rev( colors ), colors )
      get.poly <- function(a,b,r1=0.5,r2=1.0) {
        th.start <- pi*(1-a/100)
        th.end   <- pi*(1-b/100)
        th       <- seq(th.start,th.end,length=100)
        x        <- c(r1*cos(th),rev(r2*cos(th)))
        y        <- c(r1*sin(th),rev(r2*sin(th)))
        return(data.frame(x,y))
      }

      p <- ggplot()+ 
          geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="gold")

      range <- breaks[3] - breaks[2]
      inc <- range / length( colors )
      cur <- breaks[2]
      for (color in colors) {
        p <- p + geom_polygon(data=get.poly(cur,cur+inc),aes(x,y),fill=color)
        cur <- cur+inc
      }

      p <- p + geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="gold") +
        geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
                  aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
        annotate("text",x=0,y=0,label=paste0("approx effort = ",round(n*100),"%"),vjust=0,size=8,fontface="bold")+
        coord_fixed()+
        theme_bw()+
        theme(axis.text=element_blank(),
              axis.title=element_blank(),
              axis.ticks=element_blank(),
              panel.grid=element_blank(),
              panel.border=element_blank()) 

      if (is_user_n) {
        p <- p + geom_polygon(data=get.poly(pos-0.5,pos+0.5,0.2),aes(x,y),fill="blue")
      } else {
        p <- p + geom_polygon(data=get.poly(pos-0.5,pos+0.5,0.2),aes(x,y),fill="red")
      }
      p
    }

    output$gauge <- renderPlot({

      # Validation checks
      validate(
        need(length(data$filtered_by_fraction()$Result) > options$min_n(), validation_messages["too_few"])
      )
      validate(
        need(length(unique(data$filtered_by_fraction()$StationCode)) == 1, validation_messages["station"])
      )
      validate(
        need(var(data$filtered_by_fraction()$Result) > options$min_variance(), validation_messages["no_variance"])
      )
      validate(
        need(nrow(data$effort()$opts) > 0, validation_messages["no_opts"])
      )

      # Gather UI values
      # df <- data$filtered_by_fraction()
      selected_effort <- as.numeric(options$n_per_year())
      # df$dates <- as.POSIXct(as.character(df$Date), tryFormats = c("%Y-%m-%d","%m/%d/%y","%d/%b/%Y"))
      # df$vals <- as.numeric(df$Result)
      power_data <- data$effort()$pows
      optimum <- data$effort()$opts
      contour_data <- data$effort()$contours
      
      # Handle green region for optimum segment
      optimum_effort <- optimum$eff
      optimum_index <- which(contour_data$eff == optimum_effort)

      if (optimum_index == 0){
        sweet_spot_max = round(contour_data$chg[0]*100, digits = 2)
      } else {
        sweet_spot_max = round(contour_data$chg[optimum_index - 1]*100, digits = 2)
      }
      if (optimum_index == nrow(contour_data)) {
        sweet_spot_min = round(contour_data$chg[nrow(contour_data)]*100, digits = 2)
      } else {
        sweet_spot_min = round(contour_data$chg[optimum_index + 1]*100, digits = 2)
      }

      
      # print(paste0("sweet_spot = ",contour_data$eff[optimum_index]))
      # print(paste0("sweet_spot_min = ",sweet_spot_min))
      # print(paste0("sweet_spot_max = ",sweet_spot_max))
      # print(paste0("pos = ",pos))

      
      # TODO: Cover needle cases
      if (is_user_n) { # Selected effort
        selected_effort_index <- which.min(abs(selected_effort-contour_data$eff))
        pos <- round(contour_data$chg[selected_effort_index]*100, digits = 2)
        effort <- selected_effort
      } else { # Current effort
        current_effort_index <- which.min(abs(1-contour_data$eff))
        pos <- round(contour_data$chg[current_effort_index]*100, digits = 2)
        effort <- contour_data$eff[current_effort_index]
      }
      
      # Array holding section breaks for gauge charts.
      breaks <- c(0, sweet_spot_min, sweet_spot_max, 100)

      # Render gauge chart calling internal helper function.
      return(
        gg.gauge(
          pos = pos,
          n = effort,
          breaks=breaks
        )
      )
    })
  })
}

#' Effort selection slider, name left as n_slider for convenience.
#' @param id Namespace ID for scoping
#' @return Namespaced UI elements
mod_analysis_n_slider <- function(id, label){
  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("val"),
      label = label,
      min = 0.1,
      max = 2,
      value = 0.94,
      # step = 0.21,
      step = 0.1,
      width = "100%",
      round = 2
    )
  ) 
}

#' Effort slider server
#' 
#' @param id Namespace ID for scoping
#' @param object Dropdown which triggers select input choice changes
#' @param choices Reactive character array containing selection choices
#' @return Reactive user choice input
mod_analysis_n_slider_server <- function(id, data) {
  moduleServer( id, function(input, output, session){
    return(
      reactive({
        input$val
      })
    )
  })
}

#' Summary table UI module layout
#' 
#' @param id Namespace ID for scoping
#' @return Namespaced UI elements
mod_pwr_analysis_mr_ui <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(color="#0dc5c1", verbatimTextOutput(ns("summary")))
  ) 
}

#' Model summary server logic
#' 
#' @param id Namespace ID for scoping
#' @param dataset Dataframe containing dataset
#' @return Module server object
mod_lm_summary_server <- function(id, dataset, options){
  moduleServer(id, function(input, output, session) {
    output$summary <- renderPrint({
      df <- dataset()
      regression_method <- options$regression_method()

      # Populate vectors for calculating linear model
      df$dates <- as.numeric(as.POSIXct(as.character(df$Date), tryFormats = c("%Y-%m-%d","%m/%d/%y","%d/%b/%Y")))
      df$vals <- as.numeric(df$Result)

      # Calculate model
      if (regression_method == "lm - Linear Model") {
        model <- lm(formula = df$vals ~ df$dates)
        return(summary(model))
      } else if (regression_method == "loess - Polynomial Fit") {
        loess_mod <- loess(vals ~ dates, df)
        model <- predict(loess_mod, df, se=TRUE)
        return(model)
      }

      
    })
  })
}


#' UI element for mod_analysis_num_years_server output.
#'
#' @param id Id for UI element
mod_analysis_gauge_effort_title_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("title"))
  )
}

#' Outputs loaded data information relevant to gauge charts.
#'
#' @param id Id for UI element
#' @param data data frame containing all data
mod_analysis_gauge_effort_title_server <- function(id, options) {
  moduleServer( id, function(input, output, session){    
    output$title <- renderUI({
      HTML(paste0("<h3>Current level of effort for ", options$parameter(), " at ", options$station(), ".</h3>"))
    })
  })
}

#' UI element for mod_analysis_num_years_server output.
#'
#' @param id Id for UI element
mod_analysis_num_years_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("date"))
  )
}

#' Outputs effort information for the user to evaluate.
#'
#' @param id Id for UI element
#' @param data data frame containing all data
mod_analysis_num_years_server <- function(id, data, options, validation_messages) {
  moduleServer( id, function(input, output, session){    
    output$date <- renderUI({

      validate(
        need(length(data$filtered_by_fraction()$Result) > options$min_n(), validation_messages["blank"])
      )
      validate(
        need(length(unique(data$filtered_by_fraction()$StationCode)) == 1, validation_messages["blank"])
      )
      validate(
        need(var(data$filtered_by_fraction()$Result) > options$min_variance(), validation_messages["blank"])
      )
      validate(
        need(nrow(data$effort()$opts) > 0, validation_messages["blank"])
      )

      # Calculate parameters for UI
      filtered_data <- data$filtered_by_fraction()
      dates_as_dates <- as.POSIXct(as.character(filtered_data$Date), tryFormats = options$date_formats())
      num_years <- round(interval(min( dates_as_dates ), max( dates_as_dates )) / years(1))
      n <- length( data$filtered_by_fraction()$Result )
      obs_per_year <-  n / num_years

      contour_data <- data$effort()$contours
      sim_1_idx <- which.min(abs(1-contour_data$eff))
      perc_mag_chg <- round(contour_data$chg[sim_1_idx]*100, digits=2)
      sim_94_eff <- contour_data$eff[sim_1_idx]
      sim_n_per_year <- sim_94_eff * obs_per_year
      sim_n <- sim_n_per_year * 10

      # Text output for UI
      HTML(paste0("<p>The currently selected data, sampled across <b>", num_years, "</b> years, gives a total of <b>", n, "</b> observations and averages <b>", round(obs_per_year, digits = 2 ), "</b> per year.</p><p>The closest simulated effort, <b>94%</b>, computed over a <b>10 years</b> timespan corresponds to a <b>", perc_mag_chg,"%</b> magnitude of trend change, roughly <b>", round(sim_n_per_year, digits = 2 ), "</b> observations per year and <b>", floor(sim_n),"</b> total observations.</p>"))
    })
  })
}

#' UI element for mod_analysis_num_years_server output.
#'
#' @param id Id for UI element
mod_analysis_num_years_n_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("date"))
  )
}

#' Selection UI Server
#'
#' @param id Id for UI element
#' @param data data frame containing all data
mod_analysis_num_years_n_server <- function(id, data, options) {
  moduleServer( id, function(input, output, session){    
    output$date <- renderUI({
      # num_years <- round(interval(min( data$dates_as_dates() ), max( data$dates_as_dates() )) / years(1))
      # num_years <- ceiling(interval(min( data$dates_as_dates() ), max( data$dates_as_dates() )) / years(1))
      HTML(paste0("Select number of samples over <b>10</b> years:"))
    })
  })
}

#' UI element for mod_analysis_num_years_server output.
#'
#' @param id Id for UI element
mod_analysis_num_years_n_per_year_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("date"))
  )
}

#' Optimal simulated effort text
#'
#' @param id Id for UI element
#' @param data data frame containing all data
mod_analysis_num_years_n_per_year_server <- function(id, data, options, validation_messages) {
  moduleServer( id, function(input, output, session){    
    output$date <- renderUI({

      # Validation checks
      validate(
        need(length(data$filtered_by_fraction()$Result) > options$min_n(), validation_messages["blank"])
      )
      validate(
        need(length(unique(data$filtered_by_fraction()$StationCode)) == 1, validation_messages["blank"])
      )
      validate(
        need(var(data$filtered_by_fraction()$Result) > options$min_variance(), validation_messages["blank"])
      )
      validate(
        need(nrow(data$effort()$opts) > 0, validation_messages["blank"])
      )

      # Gather variables
      optimum <- data$effort()$opts
      selected_effort <- options$n_per_year()
      filtered_data <- data$filtered_by_fraction()
      contour_data <- data$effort()$contours
      
      # Calculate values for UI text
      selected_index <- which.min(abs(selected_effort-contour_data$eff))
      selected_change <- round(contour_data$chg[selected_index]*100, digits=2)
      selected_effort_approx <- round(contour_data$eff[selected_index]*100, digits=2)

      n <- length( filtered_data$Result )
      dates_as_dates <- as.POSIXct(as.character(filtered_data$Date), tryFormats = options$date_formats())
      num_years <- ceiling(interval(min( dates_as_dates ), max( dates_as_dates )) / years(1))
      n_per_year <- round(n / num_years, digits = 2)


      # selected_n <- floor(selected_effort*n*10)
      selected_n_per_year <- selected_effort*n_per_year
      selected_n <- selected_n_per_year * 10
      selected_effort_perc <- round(selected_effort*100, digits = 2)
      

      # selected_n_per_year <- round(as.numeric(selected_n) / 10, digits = 2)
      # obs_per_year <- round( selected_n_per_year / num_years )
      HTML(paste0("<p>The selected effort <b>", selected_effort_perc, "%</b> approximates most closely with a simulated effort of <b>", selected_effort_approx, "</b> which will yield a <b>", selected_change, "%</b> magnitude of trend change.</p><p>This level of effort corresponds to <b>", n_per_year, "</b> observations per year. If sampled over <b>10 years</b> the selected effort averages <b>", round(selected_n_per_year, digits = 2), "</b> observations per year and roughly <b>", floor(selected_n),"</b> total observations.</p><p><i>The optimal point identified during the analysis corresponds to an effort of <b>", round(optimum$eff*100, digits=2), "%</b> and a <b>", round(optimum$chg*100), "%</b> magnitude of trend change.</i></p>"))
    })
  })
}