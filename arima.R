#   ____________________________________________________________________________
#   FORCAST MATERIALES: ARIMA                                               ####

  # opcion de visualizacion de estructura
  options(str = strOptions(vec.len = 2))

  # cargar funciones
  source('instalar_paquetes.R')

  # definir paquetes a utilizar
  pkgs <- c(
    'tidyquant', 'dplyr', 'broom', 'ggplot2', 'purr',
    'stringr', 'data.table', 'timetk'
    )

  ipak(pkgs)
  
  
#   ____________________________________________________________________________
#   LEER DATASET                                                            ####

  # definir columnas a conservar
  colsToKeep <- c('fecha_uso_material',
                  'tipo_ticket',
                  'codigo_ebs',
                  'detalle',
                  'cantidad')

  # leer dataset
  ds <- fread(
          input      = 'materiales.csv',
          data.table = FALSE,
          select     = colsToKeep, 
          col.names  = c('fecha',
                         'area',
                         'codigo',
                         'descripcion',
                         'cantidad')
        )

#   ____________________________________________________________________________
#   DATA CARPENTRY                                                          ####

  
##  ............................................................................
##  CODIGOS VALIDOS PARA FILTRAR                                            ####

  
  # codigos validos
  codigos <- c('OSP0000111',
               'TE0762',
               'OSP0000193',
               'TE0993',
               'OSP0000185',
               'TE0991',
               'OSP0000167')
  
  categor <- c('FIBRA OPTICA DE 48 HILOS',
               'FIBRA OPTICA DE 48 HILOS',
               'MUFAS PUP',
               'MUFAS PUP',
               'BANDEJAS',
               'BANDEJAS',
               'CRUCETAS')
  
  # crear tibble
  mat <- tibble(codigo   = codigos,
                material = categor)
  
  # pre-definir orden de columnas
  ordenCols <- c('fecha', 'material', 'cantidad')
  
##  ............................................................................
##  TRANSFORMAR DATASET                                                     ####

  ds.1 <- ds %>%
          mutate_at('fecha',ymd, tz = 'America/Guatemala') %>%
          filter(codigo %in% mat$codigo) %>%
          left_join(mat, by = 'codigo') %>%
          mutate_if(is.character, toupper) %>%
          as.tibble %>%
          select(ordenCols) %>%
          arrange(fecha) 
  
  
##  ............................................................................
##  VISUALIZACION POR DIA                                                   ####

  # visualizar el uso de materiales
  ds.1 %>%
    ggplot(aes(x = fecha, y = cantidad, color = material)) +
    # Data
    geom_point(alpha = 0.5) +
    facet_wrap(~ material, ncol = 2, scale = "free_y") +
    # Aesthetics
    labs(title = "Consumo de materiales: Uso Mensual", x = "",
         subtitle = "2017-01-01 through 2017-12-31",
         caption = "Departamento de Tx Support") +
    scale_color_tq() +
    theme_tq() +
    theme(legend.position = "none")


##  ............................................................................
##  VISUALIZACION DEL CONSUMO POR MES                                       ####

  
  
  
