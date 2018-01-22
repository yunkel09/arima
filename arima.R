#   ____________________________________________________________________________
#   FORCAST MATERIALES: ARIMA                                               ####

  options(scipen = 999)
 


  # Install packages
  pacman::p_load(forecast,   # Most popular forecasting pkg
                 sweep,      # Broom tidiers for forecast pkg
                 timetk,     # Working with time series in R
                 tidyquant,  # Get's data from FRED, loads tidyverse behind the scenes
                 geofacet,   # facet_geo() for visualizing facets organized as states
                 data.table, # function fread
                 ggpubr,     # graphic already formatted
                 outliers,   # detect outliers
                 openxlsx)   # print excel

#   ____________________________________________________________________________
#   READ DATASET                                                            ####

  # read dataset
  osp <- fread(
          input            = 'osp_materials_v2.csv',
          data.table       = FALSE,
          na.strings       = c('', '#N/A','NULL', ' '),
          encoding         = 'UTF-8',
          blank.lines.skip = TRUE,
          verbose          = TRUE
         ) %>% as.tibble()
  
  # save object in a binary file
  save(osp, file = 'osp.rda')
  load('osp.rda')
  
  # remove outliers analysed by Bryan
  anomalies <- fread(
          input            = 'anomalies.csv',
          data.table       = FALSE,
          na.strings       = c('', '#N/A','NULL', ' '),
          encoding         = 'UTF-8',
          blank.lines.skip = TRUE,
          verbose          = TRUE
        ) %>% as.tibble()
  
  save(anomalies, file = 'anomalies.rda')
   
##  ............................................................................
##  MATERIALS TO FORECAST                                                   ####
  
  codigo <- c('OSP0000111',   # FIBER OPTIC CABLE ADSS FOR 150 METER
              'OSP0000193',   # MUFA_48
              'OSP0000185',   # BANDEJAS
              'OSP0000186',   # MUFA_24
              'OSP0001420',   # MUFA COYLCC-F008 12 THREADS
              'OSP0001142'    # HERRAJE VKOM
              )
  
  # herraje vkon, fibra de 8 y mufas de 12 OSP0001420
  orden  <- c('ticket', 'fecha', 'cantidad', 'material')
  
  codigo_ebs <-
    c('MUFA PARA FIBRA 48 HILOS VERTICAL'                             = 'MUFA_48',
      'MUFA P FIBRA COYOTE PLP DE 24 EMPALMES SIN BANDEJAS'           = 'MUFA_24',
      'BANDEJAS PARA MUFAS PLP DE 24 O 48 PARA 12 FIBRAS'             = 'BANDEJAS',
      'FIBER OPTIC CABLE ADSS FOR 150 METER GAPS 48 THREADS'          = 'FIBRA_48',
      '800622 MUFA P FIBRA COYOTE PUP 48 HILOS SIN BANDEJAS'          = 'MUFA_48',
      'AT-3BE27DT-048-CLCB AGS N/A FIBER OPTICAL 48H ZWP'             = 'FIBRA_48',
      'CABLE DE FIBRA OPTICA ADSS DE 48 P/VANOS DE 150 MTRS 13.44 MM' = 'FIBRA_48',
      'Mufa Coyote COYLCC-F008'                                       = 'MUFA_12',
      'H007 VKOM H007 RETENTION SPIRAL FOR FIBER 12.7mm--13.2mm'      = 'HERRAJE_VKOM')
  
  # crear funcion para reemplazar nombres
  conv.material <- . %>% str_replace_all(codigo_ebs)
  
  
  # parsear data
  osp.2 <-
    osp %>%
    mutate_at('fecha_uso', dmy) %>%
    rename(material = nombre,
           fecha    = fecha_uso,
           item     = codigo_actual) %>%
    filter(tipo  == 'PLEX',
           item  %in% codigo,
           fecha > '2014-12-31', fecha < '2018-01-01') %>%
    mutate_at('material', conv.material) %>%
    arrange(fecha) %>%
    select(orden)
  
  # save for later use
  save(osp.2, file = 'osp.2.rda')
  load('osp.2.rda')
 
  
##  ............................................................................
##  EXPLORATORY DATA ANALYSIS                                               ####

   
  # geom
  osp.2 %>%
    ggplot(aes(x = fecha, y = cantidad, color = material)) +
    geom_point() +
    facet_wrap(~ material, ncol = 2, scales = 'free_y') +
    scale_color_tq() +
    theme_tq() +
    theme(legend.position = 'none')
  

  # boxplot
  osp.2 %>% 
    ggplot(aes(x = material, y = cantidad)) +
    geom_boxplot(outlier.colour = 'red', outlier.shape = 1) +
    theme_bw()
  
  # fiber optic's histogram
  gghistogram(data = subset(osp.2, material == 'FIBRA_48'), x = "cantidad", fill = "lightgray",
              add = "mean", rug = TRUE)
  
  
##  ............................................................................
##  OUTLIER TREATMENT                                                       ####

  # delete anomalies
  osp.3 <- osp.2[-which(osp.2$ticket %in% anomalies$ticket & osp.2$material == 'FIBRA_48'), ]
  
  # Delete most extreme outlier for MUFA_24
  osp.3 <- osp.3[-which(osp.2$material == 'MUFA_24' & osp.2$cantidad == outlier(osp.2$cantidad[osp.2$material == 'MUFA_24'])),  ]
  
  
  # first we need to transfor the current tidy df into transversal in order to
  # get the outlier for each one.
  
  osp_wide <- osp.3 %>%
    mutate(i = row_number()) %>%          # necesary due repeated index
    spread(key = material, value = cantidad)
  
  # get outliers for each material
  out <- lapply(osp_wide[, 3:8], function(x) sort(boxplot.stats(x)$out)) 
  
##  ............................................................................
##  AGREGACION MENSUAL                                                      ####

  # agregacion mensual
  # critical part:  materials has to be grouped to being able to apply tq_transmute
  
  osp.mensual <- 
    osp.3 %>%
    group_by(material) %>%
      tq_transmute(
      select     = cantidad,
      mutate_fun = apply.monthly,
      FUN        = sum,
      na.rm      = TRUE,
      col_rename = 'total'
    )
  
  osp.wide_monthly <- osp.mensual %>%
    mutate(i = row_number()) %>%
    spread(key = material, value = total) %>%
    select(-i)
  
  out_monthly <- map(.x = osp.wide_monthly[, 2:7], .f = function(x) sort(boxplot.stats(x)$out))
  
##  ............................................................................
##  VISUALIZACION MENSUAL                                                   ####

  # visualizar el uso de materiales de forma mensual
  osp.mensual %>%
    ggplot(aes(x = fecha, y = total, color = material)) +
    # Data
    geom_point() +
    geom_smooth(method = 'loess') +
    facet_wrap(~ material, ncol = 2, scale = "free_y") +
    # Aesthetics
    labs(title = "Consumo promedio de materiales por mes", x = "",
         subtitle = "Febrero-2015 a Diciembre-2017", y = 'Media de Consumo Mensual',
         caption = "Departamento de Tx Support") +
    expand_limits(y = 0) +
    scale_color_tq() +
    theme_tq() +
    theme(legend.position = "none")
  
  
  # boxplot
  osp.mensual %>% filter(material == 'FIBRA_48') %>%
    ggplot(aes(x = material, y = total)) +
    geom_boxplot(outlier.colour = 'red', outlier.shape = 1) +
    theme_bw()
  
#   ____________________________________________________________________________
#   MATERIALS FORECASTING                                                   ####

  
  # ajuste y pronostico
  osp.mensual.2 <-
    osp.mensual %>%
    nest() %>%                             
    mutate(
      data_ts = map(.x     = data, 
                    .f     = tk_ts, 
                    freq   = 12,
                    start  = 2016,
                    silent = TRUE)
      ) %>%
    mutate(fit = map(.x = data_ts,
                     .f = auto.arima)
           ) %>%
    mutate(pronostico = map(.x = fit,
                            .f = forecast,
                            h = 12))
  
  # aplicar sweep
  osp.sweep <- osp.mensual %>%
    mutate(sweep = map(.x           = pronostico,
                       .f           = sw_sweep,
                       timetk_idx  = TRUE,
                       rename_index = 'date')
           ) %>%
    select(material, sweep) %>%
    unnest()
  
  
#   ____________________________________________________________________________
#   GRAFICAR PRONOSTICOS                                                    ####

  osp.sweep %>%
    ggplot(aes(x = date, y = total, color = key)) +
    # Prediction intervals
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
                fill = "#D5DBFF", color = NA, size = 0) +
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
                fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
    # Actual & Forecast
    geom_point() +
    geom_line(size = 0.8) +
    facet_wrap(~ material, ncol = 2, scale = "free") +
    theme_tq() +
    scale_color_tq(theme = 'dark') +
    ggtitle("Outside Plant (OSP) Materials, 12-Months Forecast") +
    xlab("") +
    ylab("Cantidad")

 
  # imprimir la precision del modelo
  osp.mensual.2 %>% 
    mutate(glance = map(.x = fit,
                        .f = sw_glance)
    ) %>% unnest(glance, .drop = TRUE)
  
  
  
#   ____________________________________________________________________________
#   IMPRIMIR                                                                ####

  
  wb <- createWorkbook('William Chavarria')
  
  addWorksheet(wb = wb, sheetName = 'atipicos_fibra')
  writeData(wb = wb, sheet = 'atipicos_fibra', x = outlier_fiber)
  saveWorkbook(wb = wb, file = 'forecast_2018.xlsx', overwrite = TRUE)  
  
  
  guardar_excel <- function(..., bookname = 'mybook.xlsx'){
   
    df        <- list(...)
    
    # create the workbook
    wb        <- createWorkbook(creator = 'William Chavarria')
    
    # give name to each worksheet
    names(df) <- names(sapply(1:length(df), function(x) setNames(df[x], letters[x])))
    
    # create worksheet for each tibble
    sapply(seq_along(df), function(i) addWorksheet(wb = wb, sheetName = names(df[i])))
    
    # write the tibble in the correspondent worksheet
    sapply(seq_along(df), function(j) writeData(wb = wb, sheet = names(df[j]), x = df[[j]]))
    
    # save all the workbook
    saveWorkbook(wb = wb, file = bookname, overwrite = TRUE)
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  