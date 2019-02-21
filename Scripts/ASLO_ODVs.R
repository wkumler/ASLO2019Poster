# ASLO_ODVs.R

# Input: Clean_Complete.csv
# Output: ODV_SQDG.png , ODV_PG.png

library(dplyr)
library(ggplot2)
library(MBA)
library(reshape2)

LOBdata <- read.csv("Data/Clean_Complete.csv", stringsAsFactors = F)

IPDAGs <- filter(LOBdata, lipid_class=="IP_DAG")

ODV <- function(data, title, x.axis="n") {
  #Group and sum all samples with the same station and depth
  long_samples_i <- data %>% 
    group_by(Station, Depth) %>%
    summarize("total" = sum(intensity))
  
  #Interpolating via MBA
  surf_i <- mba.surf(long_samples_i, no.X = 300, no.Y = 300, extend = T)
  dimnames(surf_i$xyz.est$z) <- list(surf_i$xyz.est$x, surf_i$xyz.est$y)
  surf_i <- melt(surf_i$xyz.est$z, varnames = c('Station', 'Depth'), 
                 value.name = 'total')
  if(x.axis=="y"){
    x.axis
  }
  
  #Drawing
  gp <- ggplot(data = surf_i, aes(x = Station, y = Depth)) +
    geom_raster(aes(fill = total)) +
    scale_fill_gradientn(colours = rev(rainbow(5))) +
    geom_point(data = long_samples_i, 
               alpha = 0.2, 
               aes(x=Station, y=Depth),
               cex = 2) +
    geom_contour(aes(z = total), 
                 binwidth = max(surf_i$total)/8, 
                 colour = "black", 
                 alpha = 0.2, 
                 lwd = 1) +
    scale_y_reverse() +
    scale_x_continuous(breaks=long_samples_i$Station,
                       labels=paste("St. ", long_samples_i$Station)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 315, hjust = 0), 
          axis.title.x = element_blank(), 
          legend.title=element_blank(), 
          axis.text = element_text(size = 30, color="black"),
          axis.title = element_text(size = 36, face = "bold"),
          legend.text = element_text(size = 36, face = "bold"),
          legend.position = "none", 
          plot.title = element_text(size = 32, hjust = 0.5),
          plot.margin = unit(c(0.5,0.5,0,0), "in")) +
    ggtitle(title)
  
  
  if(x.axis=="y"){
    return(gp)
  } else {
    gp <- gp +theme(axis.ticks = element_blank(), axis.text.x = element_blank())
    return(gp)
  }
}

SQDG_df <- filter(IPDAGs, species=="SQDG")
ODV(SQDG_df, title = "SQDG abundance", x.axis = "n")
ggsave(filename = "ODV_SQDG.png", plot = last_plot(), device = "png",
       path = "Images", width = 10, height = 3.5, units = "in")

PG_df <- filter(IPDAGs, species=="PG")
ODV(PG_df, title = "PG abundance", x.axis = "n")
ggsave(filename = "ODV_PG.png", plot = last_plot(), device = "png",
       path = "Images", width = 10, height = 3.5, units = "in")

