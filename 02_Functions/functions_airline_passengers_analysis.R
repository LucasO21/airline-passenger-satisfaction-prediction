func_plot_missing_data <-
function(data){
  
  nrow <- nrow(data)
  
  missing_tbl <- data %>% 
    sapply(function(x) sum(is.na(x))) %>% 
    tidy() %>% 
    mutate(pct = x/nrow) %>% 
    mutate(pct_txt = pct %>% scales::percent(accuracy = .3)) %>% 
    mutate(names = names %>% fct_rev()) %>% 
    rename(missing = x)
  
  missing_tbl %>% 
    ggplot(aes(missing, names))+
    geom_col(fill = "#ce295e")+
    geom_label(aes(label = pct_txt, fill = "#ce295e"))+
    theme_bw()+
    theme(legend.position = "none")
    
  
}
func_mosaic_plot <-
function(data, var_name){
  
  var_name_expr <- rlang::enquo(var_name)
  
  data %>% 
    select(!!var_name_expr, satisfaction) %>% 
    group_by(!!var_name_expr, satisfaction) %>% 
    summarise(count = n()) %>%
    mutate(cut.count = sum(count),
           prop = count/sum(count)) %>%
    ungroup() %>% 
    ggplot(aes(!!var_name_expr, prop, width = cut.count, fill = satisfaction))+
    geom_bar(stat = "identity", position = "fill")+
    scale_y_continuous(labels = scales::percent_format())+
    theme_bw()+
    geom_text(aes(label = scales::percent(prop, accuracy = .1)), position = position_stack(vjust = 0.5), size = 3)+
    scale_fill_manual(values = scale_manual_2)+
    theme(axis.text.x = element_blank())+
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.key.size = unit(0.35, "cm"))
}
func_plot_axis_text_format <-
function(facet = FALSE, mosaic = TRUE){
  
  # format x & y axis
  theme <- theme(axis.text.x = element_text(color = "black", size = 9.5),
                 axis.text.y = element_text(colour = "black", size = 8.5))
  
  # edit strip text if facet is TRUE
  if (facet){
    theme <- theme +
      theme(strip.background = element_rect(fill = "#2c3e50"),
            strip.text = element_text(color = "white"))
    
  } else {
    
    theme <- theme
    
  }
  
  # mosaic plots
  if (mosaic){
    
    theme <- theme +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
  } else {
    
    theme <- theme
    
  }
  
  return(theme)
  
}
func_get_model_performance_auc_plot <-
function(model_h2o){
  
  perf_h2o <- h2o.performance(model_h2o, newdata = test_h2o)
  
  perf_h2o %>% 
    h2o.metric() %>% 
    as_tibble() %>% 
    mutate(auc = h2o.auc(perf_h2o)) %>% 
    select(tpr, fpr, auc, precision, recall)
}
