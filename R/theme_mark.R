library(extrafont)
library(tidyverse)

my_col_pal = str_c("#", 
                   c("f0fafa", # background
                     "051414", # text
                     "474d4d", # Axis line colour
                     "C2D6D4", # Grid line colour
                     "236c6c", # skobeloff
                     "e9724c", # burnt sienna
                     "6ca8a8", # cadet blue
                     "69b44b", # green
                     "941c2f", # crimson
                     "b35757", # lighter crimson
                     "072e25", # dark green
                     "6d435a"  # eggplant
                   )) 

viz_colours = my_col_pal[5:12]
library(reactable)
options(reactable.theme = reactableTheme(
  color = my_col_pal[2],
  backgroundColor = my_col_pal[1],
  borderColor = my_col_pal[3],
  stripedColor = my_col_pal[4],
  highlightColor = my_col_pal[7],
  style = list(
    fontFamily = "Lato, sans-serif",
    fontSize = "0.75rem"
  )
)
)

source_caption = function(sources, md=F){
  if(missing(sources)){
    stop("Need to provide a vector of sources to the sources argument")
  }
  
  str_c(
    "Source", if(length(sources) > 1){"s"}, ": ",
    str_c(sources, collapse = "; "),
    if_else(md, "<br><br>", "\n\n"),
    "Visualized by @MokeEire"
  )
}

coord_no_clip = function(data, x, y, ...){
  coord_cartesian(xlim = range(data[[x]], na.rm=T), 
                  ylim = range(data[[y]], na.rm=T), ...)
}

fonts = list(
  title = "Antic",
  subtitle = "Fira Sans Extra Condensed",
  body = "Lato"
)


theme_mark = function(title_family = fonts$title,
                      subtitle_family = fonts$subtitle,
                        text_family = fonts$body,
                        base_size = 13, 
                        plot_margin = margin(20,20,20,20),
                        plots_pane = FALSE,
                        md = FALSE,
                        colour_pal = my_col_pal) {
  
  bg_colour = colour_pal[1]
  text_colour = colour_pal[2]
  line_colour = colour_pal[3]
  grid_colour = colour_pal[4]
  
  # plots_pane == True & md == False
  theme_obj = ggplot2::theme_minimal(base_size = if_else(plots_pane, base_size, 11)) +
    ggplot2::theme(
      # Main elements
      text = element_text(family = text_family,
                          colour = text_colour),
      title = element_text(family = title_family,
                           colour = text_colour),
      
      plot.title = element_text(family = title_family,
                                face = "bold",
                                lineheight = 1.2,
                                hjust = 0, 
                                margin = margin(b = 10)),
      plot.subtitle = element_text(lineheight = 1,
                                   family = subtitle_family,
                                   hjust = 0, 
                                   margin = margin(b = 10)),
      

      line = element_line(colour = line_colour),
      panel.grid.major = element_line(colour = grid_colour),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Plot titles
      plot.title.position = "plot",
      
      # Plot Margin & Caption
      plot.margin = plot_margin,
      plot.caption.position = "plot", 
      plot.caption = element_text(hjust = 0,
                                  colour = line_colour, 
                                  margin = margin(t = 15, b = 0, l = 0, r = 0)),
      
      
      # Background
      plot.background = element_rect(fill = bg_colour,
                                     colour = bg_colour),
      
      # Axes
      axis.text = element_text(colour = text_colour,
                               margin = margin(t = 25, b = 25, l = 25, r = 25)),
      
      axis.line = element_line(colour = line_colour),
      
      axis.title = element_text(hjust = 1,
                                family = subtitle_family,
                                margin = margin(t = 10, b = 10, l = 10, r = 10)),
      axis.title.x = element_text(margin = margin(t = 10, b = 5, l = 10, r = 10)),

      axis.title.y.left = element_text(hjust = 1,
                                       margin = margin(t = 20, r = 10, l = 0)),
      

      
      axis.title.y.right = element_text(hjust = 0, 
                                        margin = margin(t = 10, r = 0, l = 10)),
      
      
      # Facets
      strip.text.y.left = element_text(angle = 0, 
                                       vjust = 0.5, 
                                       hjust = 1),
      strip.placement = "outside",
      strip.text.y.right = element_text(angle = 0, 
                                        vjust = 0.5, 
                                        hjust = 0)
      
    )
  
  if (plots_pane == FALSE & md == FALSE) {
    theme_obj +
      ggplot2::theme(
        text = element_text(size = base_size),
        # Plot titles
        plot.title = element_text(size = base_size * 2.2),
        plot.subtitle = element_text(size = base_size * 1.4, margin = margin(b = 25)),
        plot.caption = element_text(size = base_size, margin = margin(t = 5)),
        # Axes
        axis.text = element_text(size = base_size * 1.2, margin = margin(t = 30, b = 5)),
        axis.title = element_text(size = base_size * 1.5, margin = margin(t = 15, b = 10)),
        # Legend
        legend.title = element_text(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.1)
        
      )
    
  } else if (plots_pane == FALSE & md == TRUE) {
    theme_obj +
      ggplot2::theme(
        text = element_text(size = base_size),

        # Plot titles
        plot.title = ggtext::element_markdown(family = title_family,
                                              face = "bold",
                                              size = base_size * 2,
                                              lineheight = 1.2,
                                              hjust = 0),
        plot.subtitle = ggtext::element_markdown(size = base_size * 1.4,
                                                 lineheight = 1,
                                                 family = subtitle_family,
                                                 hjust = 0), 
        # Plot margin & caption
        plot.caption = ggtext::element_markdown(hjust = 0, 
                                                colour = line_colour, 
                                                margin = margin(t = 10, b = 5, l = 0, r = 0)),

        # Axes
        axis.text = ggtext::element_markdown(size = base_size * 1.25, 
                                             colour = text_colour,
                                             margin = margin(t = 25, b = 25, l = 25, r = 25)),
        axis.title = ggtext::element_markdown(size = base_size * 1.6,
                                              family = subtitle_family,
                                              hjust = 1,
                                              margin = margin(t = 10, b = 10, l = 10, r = 10)),
        axis.title.x = ggtext::element_markdown(margin = margin(t = 10, b = 5, l = 10, r = 10)),
        
        axis.title.y.left = ggtext::element_markdown(size = base_size * 1.6,
                                                     hjust = 1,
                                                     margin = margin(t = 10, r = 10, l = 0)),
        axis.title.y.right = ggtext::element_markdown(size = base_size * 1.6,
                                                      hjust = 0,
                                                      margin = margin(t = 10, r = 0, l = 10)),

        # Legend
        legend.title = ggtext::element_markdown(size = base_size * 1.3),
        legend.text = ggtext::element_markdown(size = base_size * 1.1),
        
        # Facets
        strip.text.y.left = element_text(size = base_size * 1.1),
        strip.text.y.right = element_text(size = base_size * 1.1)
        
      )
  } else if (plots_pane == TRUE & md == TRUE) {
    theme_obj +
      ggplot2::theme(

        # Plot titles
        plot.title = ggtext::element_markdown(face = "bold",
                                              lineheight = 1.2,
                                              hjust = 0),
        plot.subtitle = ggtext::element_markdown(lineheight = 1,
                                                 family = subtitle_family,
                                                 hjust = 0),
        
        # Plot margin and caption
        plot.caption = ggtext::element_markdown(hjust = 0, 
                                                colour = line_colour,
                                                margin = margin(10,5,0,0)),
        

        # Axes
        axis.title = ggtext::element_markdown(hjust = 1, 
                                              family = subtitle_family,
                                              margin = margin(t = 10, b = 10, l = 10, r = 10)),
        axis.title.x = ggtext::element_markdown(margin = margin(t = 10, b = 5, l = 10, r = 10)),
        
        axis.title.y.left = ggtext::element_markdown(margin = margin(t = 10, r = 10, l = 0)),
        axis.title.y.right = ggtext::element_markdown(hjust = 0, 
                                                      margin = margin(t = 10, r = 0, l = 10)),
        axis.text = ggtext::element_markdown(colour = text_colour,
                                             margin = margin(5,5,5,5)),

        # Legend
        legend.title = ggtext::element_markdown(family = title_family),
        legend.text = ggtext::element_markdown(family = text_family)
        
        
      )
  } else {
    # plots_pane == True & md == False
    theme_obj

  }
}

