

  # ==========================================================================================
  # LOAD PACKAGES
  # ==========================================================================================


  pacman::p_load(forecast,   # Most popular forecasting pkg
                 sweep,      # Broom tidiers for forecast pkg
                 timetk,     # Working with time series in R
                 tidyquant,  # Get's data from FRED, loads tidyverse behind the scenes
                 geofacet,   # facet_geo() for visualizing facets organized as states
                 data.table, # function fread
                 ggpubr,     # graphic already formatted
                 outliers,   # detect outliers
                 openxlsx)   # print excel


  # ==========================================================================================
  # READ DATASET
  # ==========================================================================================

  rda <- list.files(pattern = '*.rda')
  myf <- lapply(rda, load, .GlobalEnv)
  rm(list=setdiff(ls(), c('osp', 'anomalies')))	

  # ==========================================================================================
  # CODING FOR SUBSETTING
  # ==========================================================================================
  
  codigo <- c('OSP0000111',   # FIBER OPTIC CABLE ADSS FOR 150 METER
              'OSP0000193',   # MUFA_48
              'OSP0000185',   # BANDEJAS
              'OSP0000186',   # MUFA_24
              'OSP0001420',   # MUFA COYLCC-F008 12 THREADS
              'OSP0001142'    # HERRAJE VKOM
  )
  
  # herraje vkon, fibra de 8 y mufas de 12 OSP0001420
  orden  <- c('fecha', 'cantidad', 'material')
  
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
  
  # create function to replace codes
  conv.material <- . %>% str_replace_all(codigo_ebs)
  
  # ==========================================================================================
  # TRANSFORMING DATA
  # ==========================================================================================
  
  fiber <-
    osp %>%
    mutate_at('fecha_uso', dmy) %>%
    rename(material = nombre,
           fecha    = fecha_uso,
           item     = codigo_actual) %>%
    filter(tipo  == 'PLEX',
           item  %in% codigo,
           fecha > '2014-12-31', fecha < '2018-01-01') %>%
    mutate_at('material', conv.material) %>%
    filter(material == 'FIBRA_48') %>%
    mutate_at('ticket', as.integer) %>%
    anti_join(anomalies, by = 'ticket') %>%                   # delete errors
    arrange(fecha) %>%
    select(orden)
  
  # by month
  monthly.fiber <-
    fiber %>%
    group_by(material) %>%
    tq_transmute(
      select     = cantidad,
      mutate_fun = apply.monthly,
      FUN        = sum,
      na.rm      = TRUE,
      col_rename = 'total'
    )
  
  # ==========================================================================================
  # EDA
  # ==========================================================================================

  dia <- fiber$cantidad
  mes <- monthly.fiber$total
  
  par(mfrow = c(2, 2), cex.lab = 0.8, cex.axis = 0.6, mar = c(4, 3.5, 3, 1.5), mgp = c(2.5, 1, 0)) 
 
  
  boxplot(dia, horizontal = T, outcol = 'red', xlab = 'boxplot diario')
  boxplot(mes, horizontal = T, outcol = 'red', xlab = 'boxplot mensual')
  hist(dia, xlab = 'hist diario', main = NULL)
  
  bw <- 2 * IQR(mes) * length(mes)^(-1/3)
  hist(mes, xlab = 'hist mensual', main = NULL)
  
  xvals <- seq(0, max(mes), by = 1)
  fx <- dnorm(x = xvals, mean = mean(mes), sd = sd(mes))
  curve(dnorm(x = xvals, mean = mean(mes), sd = sd(mes)), add = T)
  
  abline(v = mean(mes), lty = 2, col = 'red')
  abline(v = median(mes), lty = 2, col = 'blue')
       
  # ==========================================================================================
  # SUMMARIZING BY MONTH
  # ==========================================================================================


    
  
  
  

  test <- osp.2 %>%
  filter(material == 'FIBRA_48') %>%
  mutate(ano = year(fecha)) %>%
  mutate_at('ano', as.factor) %>%
  select(ano, cantidad) %T>%
  boxplot(cantidad ~ ano, data = .)
 
  
  # modificar df con spread para poder pasarle funcion de alto orden
  wide <- test %>%
    mutate(i = row_number()) %>%
    spread(key = ano, value = cantidad) %>%
    select(-i)
 
  

  # generate outlier list and rename the output list
  out <- map(wide, function(x) sort(boxplot.stats(x)$out))
  names(out) <- c('año_2015', 'año_2016', 'año_2017')

  
  # objective: get an index vector for all the outliers
  ind.out <- sort(unlist(sapply(seq_along(wide), function(x) which(wide[[x]] %in% out[[x]]))))
  
  # remove all outliers (just to test)
  wide.2 <- wide[-ind.out, ]
  
  long.1 <- wide.2 %>%
    gather(key = ano, value = cantidad) 
  
  wide.3 %$% boxplot(cantidad ~ ano)
  
  long.1 %>%
  ggplot(aes(x = ano, y = cantidad)) +
    geom_boxplot(outlier.colour = 'red', outlier.shape = 1) +
    theme_bw()
  
  

    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  