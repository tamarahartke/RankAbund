# Functions to calculate rank abundances and plot rank abundance curves

# rank the abundances
spec.ranks <- function(df,gv){
  ra.all <- by(df, gv,
     function(x){
       t_data <- t(x)
       sum_data <- rowSums(t_data)
       ranks <- rank(-sum_data, ties.method = "random")
     as.data.frame(cbind(sum_data, ranks))
     } )
  }


# plotting: x = species rank, y = abundance
ra.plot <- function(ra.df, gv, mycolours){
  library(ggplot2) # requires ggplot2 for plotting
  plot.df <- dplyr::bind_rows(ra.df, .id="Groups")
  plot.df$Groups <- factor(plot.df$Groups, labels = c(levels(gv)))
  ggplot(plot.df, aes(x= ranks, y = sum_data, colour = Groups)) +
    geom_line(size = 1) +
    scale_color_manual(values = mycolours) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.line = element_line(color = "black"),
          panel.border = element_rect(fill = NA),
          plot.margin = unit(c(4,4,4,4), "pt")
      ) +
      labs(y = "total abundance", x = "species rank") +
      scale_y_continuous(trans = scales::pseudo_log_trans(),
                       limits = c(0,(max(plot.df$sum_data))),
                       breaks = c(0, 10, 100, 1000)
                       ) +
    geom_line(size = 1, y = 0, linetype = 3, colour = "grey") +
    theme(legend.title = element_blank(),
          legend.justification=c(1,1),
          legend.position=c(0.95, 0.95),
          legend.background = element_blank(),
          legend.key = element_blank())
  }
