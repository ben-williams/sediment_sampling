# metal figures for Johnny 
# created by ben.williams@alaska.gov
# Nov 2017

# load ----
# this loads the necessary libraries - you may have to run the 
# "font_import()" if you haven't before - the rest of this 
# is setting the "themes" to use 
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
metals <- read_csv('data/metals.csv')

# join the two files and change add the units to the metals
metals %>% 
  left_join(limits) %>% 
  mutate(metal = paste(metal, "(mg/kg)")) -> metals

# figure ----
# create a list of figures by location

metals %>% 
  group_by(location) %>% 
  do(plots = ggplot(data = ., aes(year, value)) + 
       geom_point() +
       xlab('') + ylab('') +
       expand_limits(y = 0) +
       scale_y_continuous(label=comma) +
       geom_line(aes(year, TEC), lty = 4) +
       geom_line(aes(year, PEC)) +
       facet_wrap(~metal, scales = 'free_y', 
                  strip.position  = "left", 
                  ncol = 2,
                  dir="v")) -> out

# plot each figure (into the "figs" folder), 
# naming each figure along the way

for(i in 1:length(out[[1]])){
  ggsave(plot = out[[2]][[i]], 
         filename = paste0('figs/', make.names(out[[1]][[i]]), '.png'),
         height = 8, width = 6.5, units = 'in')
} 


