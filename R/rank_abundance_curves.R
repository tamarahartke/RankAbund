# Functions to calculate rank abundances and plot rank abundance curves

# rank the abundances
spec.ranks <- function(df,gv){ # takes community matrix and grouping variable
  ra.all <- by(df, gv, function(x) { #group data by grouping variable, then
    t_data <- t(x) # transpose data (sites now columns, species rows)
    sum_data <- rowSums(t_data) # total abundance for each species
    ranks <- rank(-sum_data, ties.method = "random") # most abund = rank 1
    as.data.frame(cbind(sum_data, ranks)) # data frame for each level of gv
  })
  ra.all.done <- unclass(ra.all)  # remove the "S3: by" attribute, which made the list unreadable to dplyr::bind_rows in R v4.X
}


# plotting: x = species rank, y = abundance
ra.plot <- function(ra.df, mycolours = viridis::viridis(length(ra.df))){
  library(ggplot2) # requires ggplot2 for plotting
  plot.df <- dplyr::bind_rows(ra.df, .id="Groups")
  plot.df$Groups <- factor(plot.df$Groups, labels = c(names(ra.df[])))
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
      labs(y = "Total abundance", x = "Species rank") +
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
