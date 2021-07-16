library(extrafont)
library(tidyverse)
library(tidytext)
library(ggtext)
library(gganimate)
options(gganimate.dev_args = list(width = 900, height = 600)) # Change this line to the desired output size

# Colour palette, made with 
colour_pal = str_c("#", c("5b507a","d64933","4f000b","FFF8F0", "8D918B","90aa86","002a22","028090"))

# Plotting function
# Big thanks to @isabellabenabaye, this is her fantastic function, modified slightly by me
source("theme_mark.R")

# This is a vector for the levels of the year variable, modified to fit in the sentence in the subtitle
year_order = c("before 1900", paste("in", c("1900-1919", "1920-1939", "1940-1959", "1960-1979", "1980-1999", "2000-2020")))



# Data input --------------------------------------------------------------

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')


# Data processing ---------------------------------------------------------

threats_by_continent = threats %>% 
  # Convert year last seen to factor, wrap threat type to fit as facet label
  mutate(year_last_seen = case_when(str_detect(year_last_seen, "Before") ~ "before 1900",
                                    T ~ paste("in", year_last_seen)),
         year_last_seen = factor(year_last_seen, levels = year_order, ordered=T),
         threat_type = str_wrap(threat_type, 24)) %>% 
  # Count the number of threats by year last seen, continent, and threat type
  count(year_last_seen, continent, threat_type, wt = threatened, name = "threats") %>% 
  # Remove obs which have no year
  filter(!is.na(year_last_seen))

# Plotting ----------------------------------------------------------------


threats_by_continent %>% 
  ggplot(., aes(x = continent, y = threats, fill = continent))+
  geom_col()+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name = "No. of species affected", 
                     expand = expansion())+     # This brings the bars flush with the axis
  scale_fill_manual(name = NULL, values = viz_colours[1:6], guide = guide_legend(nrow = 1))+
  theme_mark(md=T, base_size = 16)+ # Use md=T to use ggtext's element_markdown, this allows us to use HTML too
  theme(axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.text = ggtext::element_markdown(size = 18, margin = margin(l = 5, r = 25)),
        axis.title.y = ggtext::element_markdown(margin = margin(r=20)),
        strip.text = ggtext::element_markdown(face = "bold"))+
  facet_wrap(~threat_type, strip.position = "top", scales = "free_x")+
  labs(title = "What Threatens Plant Biodiversity?",
       subtitle = "Threats to <strong><span style='color:#DA2A1C;'>IUCN Red List</span></strong> plant species last seen <strong><span style='color:#DA2A1C;'>{closest_state}</span></strong>",
       caption = str_c("<strong>Note</strong>: The data covers threats to plant species which are extinct or in danger of being extinct. This does not capture threats to all other plant life.<br><br>", 
                       "Visualized by @MokeEire"))+
  # I don't think this is doing exactly as intended, I was going for a long pause at the end
  transition_states(year_last_seen, transition_length = 4, state_length = c(rep(12, 6), 20))

# anim_save("your_animation.gif")
