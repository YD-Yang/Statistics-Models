library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(ChannelAttribution)
library(markovchain)

# simulating the "real" data
set.seed(354)
df <- data.frame(client_id = sample(c(1:1000), 5000, replace = TRUE),
                  date = sample(c(1:32), 5000, replace = TRUE),
                  channel = sample(c(0:9), 5000, replace = TRUE,
                                   prob = c(0.1, 0.15, 0.05, 0.07, 0.11, 0.07, 0.13, 0.1, 0.06, 0.16)))
df$date <- as.Date(df$date, origin = "2015-01-01")
df$channel <- paste0('channel_', df$channel)


# aggregating channels to the paths for each customer
df1 <- df %>%
  arrange(client_id, date) %>%
  group_by(client_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            # assume that all paths were finished with conversion
            conv = 1,
            conv_null = 0) %>%
  ungroup()

# calculating the models (Markov and heuristics)
mod <- markov_model(df1,
                     var_path = 'path',
                     var_conv = 'conv',
                     var_null = 'conv_null',
                     out_more = TRUE)


# heuristic_models() 
#h_mod1 <- heuristic_models(df1, var_path = 'path', var_conv = 'conv')

df_hm <- df1 %>%
  mutate(channel_name_ft = sub('>.*', '', path),
         channel_name_ft = sub(' ', '', channel_name_ft),
         channel_name_lt = sub('.*>', '', path),
         channel_name_lt = sub(' ', '', channel_name_lt))

# first-touch conversions
df_ft <- df_hm %>%
  group_by(channel_name_ft) %>%
  summarise(first_touch_conversions = sum(conv)) %>%
  ungroup()
# last-touch conversions
df_lt <- df_hm %>%
  group_by(channel_name_lt) %>%
  summarise(last_touch_conversions = sum(conv)) %>%
  ungroup()

h_mod2 <- merge(df_ft, df_lt, by.x = 'channel_name_ft', by.y = 'channel_name_lt')

# merging all models
all_models <- merge(h_mod2, mod$result, by.x = 'channel_name_ft', by.y = 'channel_name')
colnames(all_models)[c(1, 4)] <- c('channel_name', 'attrib_model_conversions')


#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------


############## visualizations ##############
# transition matrix heatmap for "real" data
df_plot_trans <- mod2$transition_matrix

cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")
t <- max(df_plot_trans$transition_probability)

ggplot(df_plot_trans, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  scale_fill_gradientn(colours = cols, limits = c(0, t),
                       breaks = seq(0, t, by = t/4),
                       labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)),
                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Transition matrix heatmap")

# models comparison
all_mod_plot <- melt(all_models, id.vars = 'channel_name', variable.name = 'conv_type')
all_mod_plot$value <- round(all_mod_plot$value)
# slope chart
pal <- colorRampPalette(brewer.pal(10, "Set1"))
ggplot(all_mod_plot, aes(x = conv_type, y = value, group = channel_name)) +
  theme_solarized(base_size = 18, base_family = "", light = TRUE) +
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  geom_line(aes(color = channel_name), size = 2.5, alpha = 0.8) +
  geom_point(aes(color = channel_name), size = 5) +
  geom_label_repel(aes(label = paste0(channel_name, ': ', value), fill = factor(channel_name)),
                   alpha = 0.7,
                   fontface = 'bold', color = 'white', size = 5,
                   box.padding = unit(0.25, 'lines'), point.padding = unit(0.5, 'lines'),
                   max.iter = 100) +
  theme(legend.position = 'none',
        legend.title = element_text(size = 16, color = 'black'),
        legend.text = element_text(size = 16, vjust = 2, color = 'black'),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold", color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = "bold", color = 'black'),
        strip.background = element_rect(fill = "#f0b35f")) +
  labs(x = 'Model', y = 'Conversions') +
  ggtitle('Models comparison') +
  guides(colour = guide_legend(override.aes = list(size = 4)))




#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
#part II 

library(tidyverse)
library(reshape2)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(ChannelAttribution)
library(markovchain)
library(visNetwork)
library(expm)
library(stringr)


##### simulating the "real" data #####
set.seed(454)
df_raw <- data.frame(customer_id = paste0('id', sample(c(1:20000), replace = TRUE)),
                     date = as.Date(rbeta(80000, 0.7, 10) * 100, origin = "2016-01-01"), 
                     channel = paste0('channel_', sample(c(0:7), 80000, replace = TRUE,
                     prob = c(0.2, 0.12, 0.03, 0.07, 0.15, 0.25, 0.1, 0.08))) ) %>%
  group_by(customer_id) %>%
  mutate(conversion = sample(c(0, 1), n(), prob = c(0.975, 0.025), replace = TRUE)) %>%
  ungroup() %>%
  arrange(customer_id, date)

df_paths <- df_raw %>%
  group_by(customer_id) %>%
  mutate(path_no = ifelse(is.na(lag(cumsum(conversion))), 0, lag(cumsum(conversion))) + 1) %>%
  ungroup()


df_paths_1 <- df_paths %>%
  filter(path_no == 1) %>%
  select(-path_no)

df_paths_2 <- df_paths %>%
  filter(path_no == 2) %>%
  select(-path_no)

df_paths_3 <- df_paths %>%
  filter(path_no == 3) %>%
  select(-path_no)

df_paths_4 <- df_paths %>%
  filter(path_no == 4) %>%
  select(-path_no)


df_p1 <- df_paths_1 %>%
  arrange(customer_id, date) %>%
  group_by(customer_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            # assume that all paths were finished with conversion
            conv = sum(conversion)) %>%
  ungroup()

df_p2 <- df_paths_2 %>%
  arrange(customer_id, date) %>%
  group_by(customer_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            # assume that all paths were finished with conversion
            conv = sum(conversion)) %>%
  ungroup()

df_p3 <- df_paths_3 %>%
  arrange(customer_id, date) %>%
  group_by(customer_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            # assume that all paths were finished with conversion
            conv = sum(conversion)) %>%
  ungroup()


df_p4 <- df_paths_3 %>%
  arrange(customer_id, date) %>%
  group_by(customer_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            # assume that all paths were finished with conversion
            conv = sum(conversion)) %>%
  ungroup()

####################################################################################

df_path_1_clean <- df_paths_1 %>%
  # removing NAs
  filter(!is.na(channel)) %>%
  
  # adding order of channels in the path
  group_by(customer_id) %>%
  mutate(ord = c(1:n()),
         is_non_direct = ifelse(channel == 'channel_6', 0, 1),
         is_non_direct_cum = cumsum(is_non_direct)) %>%
  
  # removing Direct (channel_6) when it is the first in the path
  filter(is_non_direct_cum != 0) %>%
  
  # replacing Direct (channel_6) with the previous touch point
  mutate(channel = ifelse(channel == 'channel_6', channel[which(channel != 'channel_6')][is_non_direct_cum], channel)) %>%
  
  ungroup() %>%
  select(-ord, -is_non_direct, -is_non_direct_cum)


##### one- and multi-channel paths #####
df_path_1_clean <- df_path_1_clean %>%
  group_by(customer_id) %>%
  mutate(uniq_channel_tag = ifelse(length(unique(channel)) == 1, TRUE, FALSE)) %>%
  ungroup()

df_path_1_clean_uniq <- df_path_1_clean %>%
  filter(uniq_channel_tag == TRUE) %>%
  select(-uniq_channel_tag)

df_path_1_clean_multi <- df_path_1_clean %>%
  filter(uniq_channel_tag == FALSE) %>%
  select(-uniq_channel_tag)

### experiment ###
# attribution model for all paths
df_all_paths <- df_path_1_clean %>%
  group_by(customer_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            conversion = sum(conversion)) %>%
  ungroup() %>%
  filter(conversion == 1)

mod_attrib <- markov_model(df_all_paths,
                           var_path = 'path',
                           var_conv = 'conversion',
                           out_more = TRUE)
mod_attrib$removal_effects
mod_attrib$result
d_all <- data.frame(mod_attrib$result)

# attribution model for splitted multi and unique channel paths
df_multi_paths <- df_path_1_clean_multi %>%
  group_by(customer_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            conversion = sum(conversion)) %>%
  ungroup() %>%
  filter(conversion == 1)

mod_attrib_alt <- markov_model(df_multi_paths,
                               var_path = 'path',
                               var_conv = 'conversion',
                               out_more = TRUE)
mod_attrib_alt$removal_effects
mod_attrib_alt$result

# adding unique paths
df_uniq_paths <- df_path_1_clean_uniq %>%
  filter(conversion == 1) %>%
  group_by(channel) %>%
  summarise(conversions = sum(conversion)) %>%
  ungroup()

d_multi <- data.frame(mod_attrib_alt$result)

d_split <- full_join(d_multi, df_uniq_paths, by = c('channel_name' = 'channel')) %>%
  mutate(result = total_conversions + conversions)

sum(d_all$total_conversions)
sum(d_split$result)



##### Higher order of Markov chains and consequent duplicated channels in the path #####

# computing transition matrix - 'manual' way
df_multi_paths_m <- df_multi_paths %>%
  mutate(path = paste0('(start) > ', path, ' > (conversion)'))
m <- max(str_count(df_multi_paths_m$path, '>')) + 1 # maximum path length

df_multi_paths_cols <- colsplit(string = df_multi_paths_m$path, pattern = ' > ', names = c(1:m))
colnames(df_multi_paths_cols) <- paste0('ord_', c(1:m))
df_multi_paths_cols[df_multi_paths_cols == ''] <- NA

df_res <- vector('list', ncol(df_multi_paths_cols) - 1)

for (i in c(1:(ncol(df_multi_paths_cols) - 1))) {
  
  df_cache <- df_multi_paths_cols %>%
    select(num_range("ord_", c(i, i+1))) %>%
    na.omit() %>%
    group_by_(.dots = c(paste0("ord_", c(i, i+1)))) %>%
    summarise(n = n()) %>%
    ungroup()
  
  colnames(df_cache)[c(1, 2)] <- c('channel_from', 'channel_to')
  df_res[[i]] <- df_cache
}

df_res <- do.call('rbind', df_res)

df_res_tot <- df_res %>%
  group_by(channel_from, channel_to) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  group_by(channel_from) %>%
  mutate(tot_n = sum(n),
         perc = n / tot_n) %>%
  ungroup()

df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       n = c(0, 0, 0),
                       tot_n = c(0, 0, 0),
                       perc = c(0, 1, 1))

df_res_tot <- rbind(df_res_tot, df_dummy)

# comparing transition matrices
trans_matrix_prob_m <- dcast(df_res_tot, channel_from ~ channel_to, value.var = 'perc', fun.aggregate = sum)
trans_matrix_prob <- data.frame(mod_attrib_alt$transition_matrix)
trans_matrix_prob <- dcast(trans_matrix_prob, channel_from ~ channel_to, value.var = 'transition_probability')

# computing attribution - 'manual' way
channels_list <- df_path_1_clean_multi %>%
  filter(conversion == 1) %>%
  distinct(channel)
channels_list <- c(channels_list$channel)

df_res_ini <- df_res_tot %>% select(channel_from, channel_to)
df_attrib <- vector('list', length(channels_list))

for (i in c(1:length(channels_list))) {
  
  channel <- channels_list[i]
  
  df_res1 <- df_res %>%
    mutate(channel_from = ifelse(channel_from == channel, NA, channel_from),
           channel_to = ifelse(channel_to == channel, '(null)', channel_to)) %>%
    na.omit()
  
  df_res_tot1 <- df_res1 %>%
    group_by(channel_from, channel_to) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    
    group_by(channel_from) %>%
    mutate(tot_n = sum(n),
           perc = n / tot_n) %>%
    ungroup()
  
  df_res_tot1 <- rbind(df_res_tot1, df_dummy) # adding (start), (conversion) and (null) states
  
  df_res_tot1 <- left_join(df_res_ini, df_res_tot1, by = c('channel_from', 'channel_to'))
  df_res_tot1[is.na(df_res_tot1)] <- 0
  
  df_trans1 <- dcast(df_res_tot1, channel_from ~ channel_to, value.var = 'perc', fun.aggregate = sum)
  
  trans_matrix_1 <- df_trans1
  rownames(trans_matrix_1) <- trans_matrix_1$channel_from
  trans_matrix_1 <- as.matrix(trans_matrix_1[, -1])
  
  inist_n1 <- dcast(df_res_tot1, channel_from ~ channel_to, value.var = 'n', fun.aggregate = sum)
  rownames(inist_n1) <- inist_n1$channel_from
  inist_n1 <- as.matrix(inist_n1[, -1])
  inist_n1[is.na(inist_n1)] <- 0
  inist_n1 <- inist_n1['(start)', ]
  
  res_num1 <- inist_n1 %*% (trans_matrix_1 %^% 100000)
  
  df_cache <- data.frame(channel_name = channel,
                         conversions = as.numeric(res_num1[1, 1]))
  
  df_attrib[[i]] <- df_cache
}

df_attrib <- do.call('rbind', df_attrib)

# computing removal effect and results
tot_conv <- sum(df_multi_paths_m$conversion)

df_attrib <- df_attrib %>%
  mutate(tot_conversions = sum(df_multi_paths_m$conversion),
         impact = (tot_conversions - conversions) / tot_conversions,
         tot_impact = sum(impact),
         weighted_impact = impact / tot_impact,
         attrib_model_conversions = round(tot_conversions * weighted_impact)
  ) %>%
  select(channel_name, attrib_model_conversions)


##### Generic Probabilistic Model #####
df_all_paths_compl <- df_path_1_clean %>%
  group_by(customer_id) %>%
  summarise(path = paste(channel, collapse = ' > '),
            conversion = sum(conversion)) %>%
  ungroup() %>%
  mutate(null_conversion = ifelse(conversion == 1, 0, 1))

mod_attrib_complete <- markov_model(
  df_all_paths_compl,
  var_path = 'path',
  var_conv = 'conversion',
  var_null = 'null_conversion',
  out_more = TRUE
)

trans_matrix_prob <- mod_attrib_complete$transition_matrix %>%
  dmap_at(c(1, 2), as.character)

##### viz #####
edges <-
  data.frame(
    from = trans_matrix_prob$channel_from,
    to = trans_matrix_prob$channel_to,
    label = round(trans_matrix_prob$transition_probability, 2),
    font.size = trans_matrix_prob$transition_probability * 100,
    width = trans_matrix_prob$transition_probability * 15,
    shadow = TRUE,
    arrows = "to",
    color = list(color = "#95cbee", highlight = "red")
  )

nodes <- data_frame(id = c( c(trans_matrix_prob$channel_from), c(trans_matrix_prob$channel_to) )) %>%
  distinct(id) %>%
  arrange(id) %>%
  mutate(
    label = id,
    color = ifelse(
      label %in% c('(start)', '(conversion)'),
      '#4ab04a',
      ifelse(label == '(null)', '#ce472e', '#ffd73e')
    ),
    shadow = TRUE,
    shape = "box"
  )

visNetwork(nodes,
           edges,
           height = "2000px",
           width = "100%",
           main = "Generic Probabilistic model's Transition Matrix") %>%
  visIgraphLayout(randomSeed = 123) %>%
  visNodes(size = 5) %>%
  visOptions(highlightNearest = TRUE)



#-------------------------------------------------------
#-------------------------------------------------------
#-------------------------------------------------------
#part III 



