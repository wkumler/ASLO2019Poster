# Stacked_IPDAGs.R

# Input: Clean_Complete.csv
# Output: Stacked_IPDAGs.png

# Startup things ----
library(dplyr)
library(ggplot2)

LOBdata <- read.csv("Data/Clean_Complete.csv", stringsAsFactors = F)

IPDAGs <- filter(LOBdata, lipid_class=="IP_DAG")

# Choose only relevant IP-DAGs ----
all.IPDAGs <- unique(IPDAGs$species)
big.9.names <- c("MGDG", "DGDG", "SQDG", "PG", "PE", "PC", "DGTS_DGTA")
#BLL represents less than 1% of all stations
#WaxEsters and DAGs not proper membrane lipids

big.9.data <- IPDAGs %>%
  filter(species%in%big.9.names) %>%
  group_by(species, Station) %>%
  summarize(total_intensity=sum(intensity))

# Handle DGCC/S_DGCC separately
true.DGCC <- IPDAGs %>%
  filter(species=="DGCC"|species=="S_DGCC") %>%
  group_by(Station) %>%
  summarize(total_intensity=sum(intensity)) %>%
  cbind(species="DGCC")

#Put together
rel.IPDAGs <- bind_rows(big.9.data, true.DGCC)




# Begin graphing ----
stations <- unique(LOBdata$Station)

rel.IPDAGs %>% group_by(Station) %>% 
  ggplot(aes(x=Station, y=total_intensity)) + 
  geom_bar(aes(fill=species), stat = "identity")

# Okay, but normalize to 100% ----
norm.rel.IPDAGs <- rel.IPDAGs %>%
  group_by(Station) %>%
  mutate(proportion=(total_intensity/sum(total_intensity))*100)

#Aaaaand replot.
norm.rel.IPDAGs %>% group_by(Station) %>%
  ggplot(aes(x=Station, y=proportion)) + 
  geom_bar(aes(fill=species), stat = "identity")

#Make it pretty
norm.rel.IPDAGs$Station <- factor(norm.rel.IPDAGs$Station,
                                  levels = c(1,2,4,6,11,7,8,9,10)
                                  #Insert paste("Station", c(...)) here later
                                  )
classify.MvP <- function(station.number) {
  if(station.number %in% c(1,2,4,6,11)){
    return("Monterey")
  } else {
    return("Point Reyes")
  }
}
norm.rel.IPDAGs <- suppressWarnings(mutate(norm.rel.IPDAGs, "Location"=classify.MvP(Station)))
ordered.IPDAGs <- norm.rel.IPDAGs
specs <- c("DGTS_DGTA", "DGCC", "MGDG", "DGDG", "SQDG", 
           "PC", "PE", "PG")
ordered.IPDAGs$species <- factor(ordered.IPDAGs$species, levels = specs)

proper.colors <- c("darkgoldenrod1", "darkgoldenrod3", "#41ae76", "#238b45", 
                    "#006d2c", "#3690c0", "#0570b0", "#045a8d")
# proper.colors <- c("#238b45", "#006d2c", "#3690c0", "#0570b0", "#045a8d",
#                    "mediumpurple3", "#6E56AA", "mediumpurple4")

stacked_gp <- ordered.IPDAGs %>% 
  group_by(Station) %>%
  ggplot(aes(x=Station, y=proportion, fill=species)) + 
  geom_bar(stat = "identity") +
  geom_bar(color="black", stat = "identity", show.legend = F) + #second call to
    #geom_bar to draw outlines without outlining legend boxes
  facet_wrap(~Location, scales = "free_x") + #hella clever, thanks internet
  #guides(fill = guide_legend(nrow = 1)) +
  ylab("% of total IP-DAG species") +
  xlab("Station Number") + 
  theme_bw() +
  theme(legend.title=element_blank(), legend.position = "bottom",
      axis.text = element_text(size = 18, face = "bold", color="black"),
      axis.title = element_text(size = 18, face = "bold"),
      #strip.background =element_rect(fill=c("#F8766D", "#00BFC4")),
      legend.text = element_text(size = 16)) +
  scale_fill_manual(breaks = specs,
                      labels = paste0(" ", specs, "   "),
                      values = proper.colors)

#Save progress
stacked_gp
ggsave(filename = "Stacked_IPDAGs.png", plot = stacked_gp, device = "png", 
       path = "Images", width = 8, height = 8, units = "in")
