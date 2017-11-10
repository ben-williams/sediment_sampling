# metal figures for Johnny Zutz
# created by ben.williams@alaska.gov
# Nov 2017

# load ----
# this loads the necessary libraries - you may have to run the 
# "font_import()" if you haven't before - the rest of this 
# is setting the "themes" to use that will establish the plot appearance

library(tidyverse)
library(scales)
library(extrafont)
# font_import() only do this one time - it takes a while
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12, base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  strip.placement = 'outside'))

# data ----
limits <- read_csv('data/tecpec.csv')
metals <- read_csv('data/kgm_metals.csv')

# figures ----
# this code does the following:
# a) joins the metal data with the limit data
# b) tweaks some value for future plotting purposes
# c) cycles through each group, creates a faceted plot
#       for each metal
# d) saves the output in a list format, the first part of this list are the
#       location names, the second index is the figures

metals %>% 
  left_join(limits) %>% 
  mutate(metal = paste(metal, "(mg/kg)"),
         detected = case_when(detected == 'n' ~ 'ND',
                              detected == 'y' ~ ''),
         value = ifelse(detected == 'ND', 0.0, value)) %>% 
  group_by(metal) %>% 
  mutate(max_y = ifelse(is.na(PEC), max(value) + max(value) * 0.1, 
                        ifelse(!is.na(PEC) & max(value)>PEC, 
                               max(value) + max(value) * 0.1, PEC + PEC * 0.1)),
         max_y = ifelse(max_y<1, 1.2, max_y)) %>% 
  group_by(location) %>% 
  do(plots = ggplot(data = ., aes(year, value)) + 
       geom_point(data = . %>% filter(detected != 'ND')) +
       xlab('') + ylab('') +
       expand_limits(y = 0) +
       scale_y_continuous(label=comma, breaks = scales::pretty_breaks(n=3)) +
       geom_line(aes(year, TEC), lty = 4) +
       geom_line(aes(year, PEC)) +
       geom_blank(aes(year, max_y)) +
       facet_wrap(~metal, scales = 'free_y', 
                  strip.position  = "left", 
                  ncol = 2,
                  dir="v") +
       geom_text(aes(label = detected), vjust = -0.25, size = 3)) -> out 

# save each plot into the "figs" folder, 
# naming each figure along the way

for(i in 1:length(out[[1]])){
  ggsave(plot = out[[2]][[i]], 
         filename = paste0('figs/', make.names(out[[1]][[i]]), '.png'),
         height = 8, width = 6.5, units = 'in')
} 


