library(dplyr)
library(magrittr)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)

separator <- function (x, sep = " ", ...) format(x, ..., big.mark = sep, scientific = FALSE, trim = TRUE)

ExcludeEmpty <- function(df) {
  naCols <- apply(df, 2, function(c) all(is.na(c)))
  df[, !naCols]
}

colScheme <- function(colscheme = 'Blues') {
  colorRampPalette(RColorBrewer::brewer.pal(9, colscheme))
}


#' Prepare data
#' @export
PrepareData <- function(df_raw, df_extra, df_filter){
  
  ## Restrict dataframe to variables of interest
  df_selection <- 
    df_raw %>% 
    select(matches('Recipient country|Donor$|Type of appeal issued|Amount in US\\$')) %>% 
    setNames(c('country', 'donor', 'appeal', 'amount_received'))
  
  
  # if (df_extra$Status %in% 'Contribution' %>% grep(TRUE, .) %>% length) {
  if (df_extra %>% filter(Status %in% 'Contribution') %>% nrow) {
    
    df_extra %<>% 
      filter(Status %in% 'Contribution') %>% 
      select(matches('Recipient country|Donor$|Type of appeal issued|Amount in US\\$')) %>% 
      setNames(c('country', 'donor', 'appeal', 'amount_received'))
    
    df_selection %<>% bind_rows(df_extra)
    
  }
  
  names(df_filter) <- c('appeal', 'status', 'amount_requested')
  
  ## Merge datasets to restrict to appeals of interest
  df <- left_join(df_filter, df_selection)
  
  list_status <- c('L3', 'L2', 'Priority', 'Other') %>% factor
  list_country <- df$country %>% unique %>% sort
  list_donor <- df$donor %>% unique %>% sort
  list_appeal <- df$appeal %>% unique %>% sort
  
  df_donor <-
    df %>%
    group_by(appeal, status, donor) %>%
    summarise(total_requested = min(amount_requested, na.rm = T),
              total_received = sum(amount_received, na.rm = T),
              prop_funded = round(sum(amount_received, na.rm = T)/min(amount_requested, na.rm = T)*100, digits = 1)) %>%
    arrange(status, appeal, desc(total_received))
  
  df_donor$prop_funded <- ifelse(df_donor$prop_funded %in% Inf, 100, df_donor$prop_funded)
  df_donor$prop_funded[is.na(df_donor$prop_funded)] <- 0
  
  df_total <-
    df %>%
    group_by(appeal, status) %>%
    summarise(total_requested = min(amount_requested, na.rm = T),
              total_received = sum(amount_received, na.rm = T),
              prop_funded = round(sum(amount_received, na.rm = T)/min(amount_requested, na.rm = T)*100, digits = 1)) %>%
    arrange(desc(prop_funded), status, appeal)
  
  df_total$prop_funded <- ifelse(df_total$prop_funded %in% Inf, 100, df_total$prop_funded)
  df_total$prop_funded[is.na(df_total$prop_funded)] <- 0
  
  df_total$status %<>% factor(levels = c('L3', 'L2', 'Priority', 'Other'))
  df_total %<>% arrange(desc(prop_funded))
  df_total %<>% filter(!is.na(status))
  
  list_appeals_L3 <- subset(df_total$appeal, df_total$status %in% 'L3') 
  list_appeals_priority <- subset(df_total$appeal, df_total$status %in% 'Priority')
  list_appeals_other <- subset(df_total$appeal, df_total$status %in% 'Other')
  
  df_total$appeal %<>% factor
  
  df_total$status %<>% factor(levels = c('L3', 'L2', 'Priority', 'Other'))
  df_total %<>% 
    ungroup %>% 
    arrange(status, desc(prop_funded)) %>% 
    setNames(c('appeal', 'Crisis type', 'Amount requested', 'Amount received', 'Funded (%)'))
  
  # return list of prepared dataframes
  list(df_total, df_donor)
  
}


#' Horizontal bar plots 
#' @export
PlotBar <- function (data, xvar = "key", yvar = "value", colscheme = "Blues",  yaxis = "", legend = "") {
  
  tmp <- 
    substitute(
      data %>% 
        group_by_(xvar) %>% 
        summarise(yvar_sum = round(sum(colname, na.rm = T))), 
      list(colname = as.symbol(yvar))) %>% 
    eval
  
  tmp[[xvar]] <- factor(tmp[[xvar]], levels = rev(unique(tmp[[xvar]])))
  # tmp[[xvar]] <- factor(tmp[[xvar]], levels = unique(tmp[[xvar]]))
  
  ggplot(tmp, aes_string(x = xvar, y = "yvar_sum", fill = xvar)) + 
    geom_bar(stat = "identity", colour = "black", size = .25) + 
    scale_fill_manual(
      values = colScheme(colscheme)(nrow(tmp)),
      guide = guide_legend(reverse = TRUE) # not working!
    ) +
    # scale_y_continuous(labels = scales::percent) +
    coord_flip() + 
    theme_bw() +
    theme(
      panel.border = element_blank(), 
      plot.title = element_text(size = 12, face = "bold", color = "darkblue"), 
      legend.key = element_blank(),
      # legend.position = "",
      # legend.position = as.character(legend),
      axis.text = element_text(size = 7, angle = 0, hjust = 1, colour = "black"), 
      axis.title = element_text(size = 8, face = 'bold'),
      axis.ticks = element_blank()
    ) + 
    labs(title = "", x = "", y = yaxis)
  
}  


#' Pie plot
#' @export
PlotPie <- function(data, key = 'key', value = 'value', colscheme = 'Blues', title = NULL, retain.order = F){
  
  tmp <-
    eval(substitute(
      data %>%
        group_by_(key) %>%
        summarise(value_sum = round(sum(colname, na.rm = T))),
      list(colname = as.symbol(value)))) %>%
    mutate(prop = round(value_sum/sum(value_sum) * 100, 1))
  
  tmp <- if (retain.order == T) tmp else arrange(tmp, desc(prop))
  
  if (nrow(tmp) > 10) {
    
    tmp1 <- tmp[1:7, ]
    
    tmp2 <-
      tmp[8:nrow(tmp), ] %>%
      summarise(value_sum = sum(value_sum)) %>%
      mutate(prop = round(value_sum/sum(tmp$value_sum) * 100, 1))
    
    tmp1[8, 1] <- 'Other'
    tmp1[8, 2:3] <- tmp2
    
    tmp <- mutate(tmp1, pos = cumsum(prop) - 0.5 * prop)
    
    tmp[[key]] <-
      factor(tmp[[key]],
             levels = c(tmp[[key]][nrow(tmp)],
                        tmp[[key]][-nrow(tmp)][order(tmp$prop[-nrow(tmp)])]))
    
  } else {
    
    tmp <- mutate(tmp, pos = cumsum(prop) - 0.5 * prop)
    tmp[[key]] <- factor(tmp[[key]], levels = tmp[[key]])
    
  }
  
  tmp$charLab <- as.character(tmp[[key]])
  
  TwoLines <- function(x) {
    if (nchar(x) > 10) {
      x <- gsub(' or ', '/\n', x)
      x <- gsub(' ', '\n', x)
    } else x
    return(x)
  }
  
  tmp$charLab <- sapply(as.list(tmp$charLab), TwoLines)
  
  for(i in 1:nrow(tmp)) tmp$label[i] <- paste0(tmp[i, 'charLab'], '\n', tmp[i, 'prop'], '%')
  
  tmp[[key]] <- factor(tmp[[key]], levels = tmp[[key]])
  
  ggplot(tmp, aes_string(x = 1, y = 'prop', fill = key)) +
    geom_bar(width = 1, stat = 'identity', colour = 'white') +
    #       scale_x_discrete(limits = c(0, 1)) +
    #       geom_text(aes(x = 1.75, y = tmp$pos, label = tmp$label), colour = 'black') +
    # scale_y_continuous(breaks = tmp$pos, labels = tmp$label) +
    scale_y_continuous(breaks = tmp$pos, labels = NULL) +
    scale_fill_manual(values = colScheme(colscheme)(nrow(tmp))) +
    coord_polar('y', start = 0) +
    theme_bw() +
    theme(panel.border = element_blank(),
          plot.title = element_text(size = 12, face = 'bold', color = 'darkblue'),
          legend.key = element_blank(),
          legend.position = '',
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid  = element_blank()) +
    labs(x = '', y = '', title = ifelse(title %in% NULL, NULL, paste0(title)))
  
}
