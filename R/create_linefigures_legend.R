#' This function creates a legend to be used in a multi-grap panel
#'
#'
#'
#'



#########################################
# CREATING LEGEND FOR PANEL GRAPHS
#########################################

create_linefigures_legend <- function(DF){

 nameLine <- as.character(DF[1,"analyte"])


  DF$protocol <- as.factor(DF$protocol)
  DF$time <- as.factor(DF$time)
  DF$matrix <- as.factor(DF$matrix)

  lines <- ggplot(data = DF, aes(x = time, y = mean_conc,
                                 color = protocol))

  y_lim_max <- max(DF$mean_conc) + 0.1*(max(DF$mean_conc))


    plot <- lines +
      geom_line(aes(group = protocol), size = 1.2, position=position_dodge(width=0.2)) +
      scale_colour_grey(start = 0, end = .85) +
     # facet_wrap(matrix ~ analyte, scales = "fixed", ncol = ncol) +

      geom_point(aes(shape=protocol), size = 3.5, position=position_dodge(width=0.2)) +

      scale_shape_manual(values=c(5,6, 15:17)) +


      scale_y_continuous(limits = c(0, y_lim_max)) +
       # ggtitle(nameLine)  +

  theme_individual() +

   xlab("Time (hours)") +

      ylab("Concentration (U)") +

      ggtitle(nameLine)



    plot
    ##assigning a name to the file

    ## YourFileName <- paste(nameLine,"line.png", sep = "")

    ## saving the file to the imageDir

    ### saveInImageDirectory(YourFileName)
    return(plot)


     }

#line_graph_indiv(DF, im_dir = im_dir, width = 5, dpi = 300, height = 5)

# line_graph(DF = subset_1, ncol = 4)

#    labs(title = paste(nameLine, "\n", sep=""),
#         line = +4,
#         units = "pt") +
# geom_errorbar(aes(ymin = mean_conc - sem,
#                  ymax = mean_conc + sem),
#               width = .1, size = 0.5) +
#        scale_shape_manual(values=c(5,1,2,7,6)) +

# apply self-made theme
# Change shapes
#    scale_linetype_manual(values=c("solid",
#           "dotted", "longdash", "dotdash", "dashed"))
