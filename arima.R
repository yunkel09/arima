#   ____________________________________________________________________________
#   FORCAST MATERIALES: ARIMA                                               ####

  # opcion de visualizacion de estructura
  options(str = strOptions(vec.len = 2))

  # definir local
  Sys.setlocale("LC_TIME", "C")

  # cargar funciones
  source('instalar_paquetes.R')

  # definir paquetes a utilizar
  pkgs <- c(
    'tidyquant', 'dplyr', 'broom', 'ggplot2', 'purr',
    'stringr', 'data.table', 'timetk', 'cranlogs',
    'timekit', 'sweep', 'forecast', 'geofacet', 'magrittr'
    )

  # instalar paquetes necesarios y cargar librerias
  ipak(pkgs)

#   ____________________________________________________________________________
#   LEER DATASET                                                            ####

  # read dataset
  osp <- fread(
          input      = 'osp_materials.csv',
          data.table = FALSE
         ) %>% as.tibble()
  
  
  # borrar objetos innecesarios
  rm(list = setdiff(ls(), 'osp'))

##  ............................................................................
##  AGREGACION MENSUAL                                                      ####

  # agregacion mensual
  osp.mensual <- osp %>%
    mutate_at('fecha', dmy) %>%
    group_by(material) %>%
    tq_transmute(
      select     = cantidad,
      mutate_fun = apply.monthly,
      FUN        = sum,
      na.rm      = TRUE,
      col_rename = 'total'
      
    ) %>% select(fecha, material, total)
  
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
         subtitle = "Enero-2015 a Agosto-2017", y = 'Media de Consumo Mensual',
         caption = "Departamento de Tx Support") +
    expand_limits(y = 0) +
    scale_color_tq() +
    theme_tq() +
    theme(legend.position = "none")
  
  
  
#   ____________________________________________________________________________
#   MATERIALS FORECASTING                                                   ####

  
  # ajuste y pronostico
  osp.mensual %<>% 
    nest() %>%                             
    mutate(
      data_ts = map(.x     = data, 
                    .f     = tk_ts, 
                    freq   = 12,
                    start  = 2015,
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
  osp.mensual %>% 
    mutate(glance = map(.x = fit,
                        .f = sw_glance)
    ) %>% unnest(glance, .drop = TRUE)
  
  
  
  