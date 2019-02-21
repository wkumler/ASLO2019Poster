# Stacked_IPDAGs.R

# Code designed to make Figure 3 of the poster and do stats

# Input: Clean_Complete.csv
# Output: SQDPG barplot, 

# Startup things ----
library(dplyr)
library(ggplot2)

LOBdata <- read.csv("Data/Clean_Complete.csv", stringsAsFactors = F)

classify.MvP <- function(station.number) {
  if(station.number %in% c(1,2,4,6,11)){
    return("Monterey")
  } else {
    return("Point Reyes")
  }
}
locs <- sapply(LOBdata$Station, classify.MvP)
LOBdata$Location <- locs


# Grab SQDG and PG values ----
SQDG_df_samples <- LOBdata %>% 
  filter(species=="SQDG") %>% 
  group_by(Orbi_num, Location) %>%
  summarize("Total SQDG"=sum(intensity))
SQDG_Mont_samp <- filter(SQDG_df_samples, Location=="Monterey") %>% pull(`Total SQDG`)
SQDG_PtR_samp <- filter(SQDG_df_samples, Location=="Point Reyes") %>% pull(`Total SQDG`)

PG_df_samples <- LOBdata %>% 
  filter(species=="PG") %>% 
  group_by(Orbi_num, Location) %>%
  summarize("Total PG"=sum(intensity))
PG_Mont_samp <- filter(PG_df_samples, Location=="Monterey") %>% pull(`Total PG`)
PG_PtR_samp <- filter(PG_df_samples, Location=="Point Reyes") %>% pull(`Total PG`)

SQDPG_Mont_samp <- SQDG_Mont_samp/PG_Mont_samp
SQDPG_PtR_samp <- SQDG_PtR_samp/PG_PtR_samp

wilcox.test(SQDPG_Mont_samp, SQDG_PtR_samp)
boxplot(log(SQDPG_Mont_samp), log(SQDG_PtR_samp))




SQDG_Mont <- LOBdata %>% 
  filter(species=="SQDG") %>% 
  group_by(Station, Depth) %>%
  summarize("Total SQDG"=sum(intensity)) %>%
  filter(Station %in% c(1,2,4,6,11)) %>%
  pull("Total SQDG")

SQDG_PtR <- LOBdata %>% 
  filter(species=="SQDG") %>% 
  group_by(Station, Depth) %>%
  summarize("Total SQDG"=sum(intensity)) %>%
  filter(Station %in% c(7,8,9,10)) %>%
  pull("Total SQDG")


PG_Mont <- LOBdata %>%
  filter(species=="PG") %>% 
  group_by(Station, Depth) %>%
  summarize("Total PG"=sum(intensity)) %>%
  filter(Station %in% c(1,2,4,6,11)) %>%
  pull("Total PG")

PG_PtR <- LOBdata %>% 
  filter(species=="PG") %>% 
  group_by(Station, Depth) %>%
  summarize("Total PG"=sum(intensity)) %>%
  filter(Station %in% c(7,8,9,10)) %>%
  pull("Total PG")


Mont <- SQDG_Mont/PG_Mont
PtR <- SQDG_PtR/PG_PtR

wilcox.test(Mont, PtR)

gdata <- data.frame("SQDPG"=c(Mont, PtR), "Location"=c(rep("Monterey", length(Mont)), rep("Pt. Reyes", length(PtR))))

ggplot(data = gdata) + geom_boxplot(aes(x=Location, y=SQDPG, fill=Location), lwd=2) +
  theme_bw() + ylab("SQDG:PG Ratio\n") + xlab("") +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text = element_text(size = 28, face = "bold", color="black"),
        axis.title = element_text(size = 28, face = "bold"),
        axis.text.x = element_text(angle = 335))

ggsave(filename = "SQDPG_Boxplot.png", plot = last_plot(), device = "png",
       path = "Images", width = 3.5, height = 6.5, units = "in")
