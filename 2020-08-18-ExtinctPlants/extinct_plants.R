library(extrafont)
loadfonts(device = "win")
library(tidyverse)
library(tidytext)
library(ggtext)
library(gganimate)
options(gganimate.dev_args = list(width = 900, height = 600)) # Change this line to the desired output size

# Colour palette, made with 
colour_pal = str_c("#", c("5b507a","d64933","4f000b","FFF8F0", "8D918B","90aa86","002a22","028090"))

# Plotting function
# Big thanks to @isabellabenabaye, this is her fantastic function, modified slightly by me
theme_plants = function(title_family = "Inter",
                        text_family = "Inter",
                        base_size = 13, text_color = "gray20",
                        bg_color = colour_pal[4], line_color = colour_pal[5],
                        plot_margin = margin(20,20,20,20),
                        plots_pane = FALSE,
                        md = FALSE) {
  
  if (plots_pane == FALSE & md == FALSE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        text = element_text(family = text_family,
                            size = base_size,
                            color = text_color),
        title = element_text(family = title_family,
                             color = text_color),
        line = element_line(color = line_color),
        
        plot.title = element_text(face = "bold",
                                  size = base_size * 2,
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = base_size * 1.7,
                                     lineheight = 1.2),
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0, 
                                    size = base_size * 1.25),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.text = element_text(size = base_size * 1.2),
        axis.title = element_text(size = base_size * 1.6,
                                  hjust = 1,
                                  face = "italic",
                                  margin = margin(b=15)),
        axis.line = element_line(color = line_color),
        
        legend.title = element_text(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.1)
      )
  } else if (plots_pane == FALSE & md == TRUE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        text = element_text(family = text_family,
                            size = base_size,
                            color = text_color),
        title = ggtext::element_markdown(family = title_family,
                                         color = text_color),
        line = element_line(color = line_color),
        
        plot.title = ggtext::element_markdown(face = "bold",
                                              size = base_size * 2,
                                              lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(size = base_size * 1.7,
                                                 lineheight = 1.2),
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0, 
                                    size = base_size * 1.25),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.text = element_text(size = base_size * 1.2),
        axis.title = ggtext::element_markdown(size = base_size * 1.6,
                                              hjust = 1,
                                              face = "italic",
                                              margin = margin(b=15)),
        axis.line = element_line(color = line_color),
        
        legend.title = ggtext::element_markdown(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.1)
      )
  } else if (plots_pane == TRUE & md == TRUE) {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        text = element_text(family = text_family,
                            color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = line_color),
        
        plot.title = ggtext::element_markdown(face = "bold",
                                              lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(lineheight = 1.2),
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = ggtext::element_markdown(hjust = 0),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.title = ggtext::element_markdown(hjust = 1,
                                              face = "italic",
                                              margin = margin(b=15)),
        axis.line = element_line(color = line_color)
      )
  } else {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        text = element_text(family = text_family,
                            color = text_color),
        title = element_text(family = title_family),
        line = element_line(color = line_color),
        
        plot.title = element_text(face = "bold",
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(lineheight = 1.2),
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0),
        plot.background = element_rect(fill = bg_color,
                                       color = bg_color),
        
        axis.title = element_text(hjust = 1,
                                  face = "italic",
                                  margin = margin(b=15)),
        axis.line = element_line(color = line_color)
      )
  }
}

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
  scale_fill_manual(name = NULL, values = colour_pal[-4:-5], guide = guide_legend(nrow = 1))+
  theme_plants(plots_pane=T, md=T, base_size = 16)+ # Use md=T to use ggtext's element_markdown, this allows us to use HTML too
  theme(axis.text.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 18, margin = margin(l = 5, r = 25)),
        axis.title.y = element_text(margin = margin(r=20)),
        strip.text = element_text(face = "bold"))+
  facet_wrap(~threat_type, strip.position = "bottom", scales = "free_x")+
  labs(title = "What Threatens Plant Biodiversity?",
       subtitle = "Threats to <strong><span style='color:#DA2A1C;'>IUCN Red List</span></strong> plant species last seen <strong><span style='color:#DA2A1C;'>{closest_state}</span></strong>",
       caption = str_c("<strong>Note</strong>: The data covers threats to plant species which are extinct or in danger of being extinct. This does not capture threats to all other plant life.<br><br><span style='color:", 
                       colour_pal[5], 
                       "'>Visualized by @MokeEire</span>"))+
  # I don't think this is doing exactly as intended, I was going for a long pause at the end
  transition_states(year_last_seen, transition_length = 4, state_length = c(rep(12, 6), 20)) 

# anim_save("your_animation.gif")
