theme_black_presentation <- function (base_size = 16){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_text(size = base_size, colour = 'white', vjust = 1), 
    axis.text.y = theme_text(size = base_size, colour = 'white', hjust = 1), 
    axis.ticks = theme_segment(colour = "white", size = 0.2), 
    axis.title.x = theme_text(size = base_size*1.2, colour = 'white', vjust = .25), 
    axis.title.y = theme_text(size = base_size*1.2, colour = 'white', angle = 90, vjust = .25), 
    axis.ticks.length = unit(0.3, "lines"), 
    axis.ticks.margin = unit(0.5, "lines"), 
    legend.background = theme_rect(colour = NA), 
    legend.key = theme_rect(colour = "white", fill = 'black'),
    legend.key.height = unit(2,"lines"),
    legend.text = theme_text(size = base_size, colour = 'white'), 
    legend.title = theme_text(size = base_size, face = "bold", hjust = 0, colour = 'white'),  
    panel.background = theme_rect(fill = "black", colour = NA), 
    panel.border = theme_rect(fill = NA, colour = "white"), 
    panel.grid.major = theme_line(colour = "grey20", size = 0.4), 
    panel.grid.minor = theme_line(colour = "grey5", size = 0.5), 
    panel.margin = unit(3, "lines"), 
    strip.background = theme_rect(fill = "grey30", colour = "grey10"), 
    strip.text.x = theme_text(size = base_size * 0.8, colour = 'white'), 
    strip.text.y = theme_text(size = base_size * 0.8, colour = 'white', angle = -90), 
    plot.background = theme_rect(colour = 'black', fill = 'black'), 
    plot.title = theme_text(size = base_size * 1.2),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
    ), class = "options")
}


theme_black <- function (base_size = 12){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_text(size = base_size * 0.8, colour = 'white', lineheight = 0.9, vjust = 1), 
    axis.text.y = theme_text(size = base_size * 0.8, colour = 'white', lineheight = 0.9, hjust = 1), 
    axis.ticks = theme_segment(colour = "white", size = 0.2), 
    axis.title.x = theme_text(size = base_size, colour = 'white', vjust = 1), 
    axis.title.y = theme_text(size = base_size, colour = 'white', angle = 90, vjust = 0.5), 
    #axis.ticks.length = unit(0.3, "lines"), 
    #axis.ticks.margin = unit(0.5, "lines"), 
    legend.background = theme_rect(colour = NA), 
    legend.key = theme_rect(colour = "white", fill = 'black'), 
    #legend.key.size = unit(1.2, "lines"), 
    legend.key.height = NA, 
    legend.key.width = NA,     
    legend.text = theme_text(size = base_size * 0.8, colour = 'white'), 
    legend.title = theme_text(size = base_size * 0.8, face = "bold", hjust = 0, colour = 'white'), 
    legend.position = "right", 
    legend.text.align = NA, 
    legend.title.align = NA, 
    legend.direction = "vertical", 
    legend.box = NA,    
    panel.background = theme_rect(fill = "black", colour = NA), 
    panel.border = theme_rect(fill = NA, colour = "white"), 
    panel.grid.major = theme_line(colour = "grey20", size = 0.2), 
    panel.grid.minor = theme_line(colour = "grey5", size = 0.5), 
    #panel.margin = unit(0.25, "lines"), 
    strip.background = theme_rect(fill = "grey30", colour = "grey10"), 
    strip.text.x = theme_text(size = base_size * 0.8, colour = 'white'), 
    strip.text.y = theme_text(size = base_size * 0.8, colour = 'white', angle = -90), 
    plot.background = theme_rect(colour = 'black', fill = 'black'), 
    plot.title = theme_text(size = base_size * 1.2) 
    #plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")
    ), class = "options")
}

theme_fullframe <- function (base_size = 12){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_blank(), 
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.title.x = theme_blank(), 
    axis.title.y = theme_blank(), 
    axis.ticks.length = unit(0, "lines"), 
    axis.ticks.margin = unit(0, "lines"), 
    legend.position = "none", 
    panel.background = theme_blank(), 
    panel.border = theme_blank(), 
    panel.grid.major = theme_blank(), 
    panel.grid.minor = theme_blank(), 
    panel.margin = unit(0, "lines"), 
    plot.background = theme_blank(), 
    plot.margin = unit(0*c(-1.5, -1.5, -1.5, -1.5), "lines")
    ), class = "options")
}

theme_minimal <- function (base_size = 12, base_family = "") 
{
  structure(list(axis.line = theme_blank(), axis.text.x = theme_text(family = base_family, 
                                                                     size = base_size * 0.8, lineheight = 0.9, vjust = 1), 
                 axis.text.y = theme_text(family = base_family, size = base_size * 
                   0.8, lineheight = 0.9, hjust = 1), axis.ticks = theme_segment(colour = "black", 
                                                                                 size = 0.2), axis.title.x = theme_text(family = base_family, 
                                                                                                                        size = base_size, vjust = 1), axis.title.y = theme_text(family = base_family, 
                                                                                                                                                                                size = base_size, angle = 90, vjust = 0.5), axis.ticks.length = unit(0.3, 
                                                                                                                                                                                                                                                     "lines"), axis.ticks.margin = unit(0.5, "lines"), legend.background = theme_rect(colour = NA), 
                 legend.margin = unit(0.2, "cm"), legend.key = theme_rect(colour = NA), 
                 legend.key.size = unit(1.2, "lines"), legend.key.height = NULL, 
                 legend.key.width = NULL, legend.text = theme_text(family = base_family, 
                                                                   size = base_size * 0.8), legend.text.align = NULL, 
                 legend.title = theme_text(family = base_family, size = base_size * 
                   0.8, face = "bold", hjust = 0), legend.title.align = NULL, 
                 legend.position = "right", legend.direction = "vertical", legend.justification = "center", 
                 legend.box = NULL, panel.background = theme_rect(fill = "white", 
                                                                  colour = NA), panel.border = theme_rect(fill = NA, 
                                                                                                          colour = "grey90"), panel.grid.major = theme_line(colour = "grey90", 
                                                                                                                                                            size = 0.2), panel.grid.minor = theme_line(colour = "grey98", 
                                                                                                                                                                                                       size = 0.5), panel.margin = unit(0.25, "lines"), 
                 strip.background = theme_rect(fill = NA, colour = NA), 
                 strip.text.x = theme_text(family = base_family, size = base_size * 
                   0.8), strip.text.y = theme_text(family = base_family, 
                                                   size = base_size * 0.8, angle = -90), plot.background = theme_rect(colour = NA), 
                 plot.title = theme_text(family = base_family, size = base_size * 
                   1.2), plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")), 
            class = "options")
}