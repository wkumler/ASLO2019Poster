#Nutrient plots

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


# Grab SQDG and PG values alongside Phosphate ----
SQDG_df_samples <- LOBdata %>% 
  filter(species=="SQDG") %>% 
  group_by(Orbi_num, Location, Station, Phos) %>%
  summarize("Total SQDG"=sum(intensity))
SQDG_Mont_samp <- filter(SQDG_df_samples, Location=="Monterey") %>% pull(`Total SQDG`)
SQDG_PtR_samp <- filter(SQDG_df_samples, Location=="Point Reyes") %>% pull(`Total SQDG`)

PG_df_samples <- LOBdata %>% 
  filter(species=="PG") %>% 
  group_by(Orbi_num, Location, Station, Phos) %>%
  summarize("PG"=sum(intensity))
PG_Mont_samp <- filter(PG_df_samples, Location=="Monterey") %>% pull(PG)
PG_PtR_samp <- filter(PG_df_samples, Location=="Point Reyes") %>% pull(PG)

SQDPG_df <- cbind(PG_df_samples, SQDG=SQDG_df_samples$`Total SQDG`)
SQDPG_df <- mutate(SQDPG_df, SQDGxPG=SQDG/PG)

my_x_title <- expression(paste("Phosphate (", mu, "mol L"^"-1",")"))

ggplot(SQDPG_df, aes(x=Phos, y=SQDGxPG)) +
  geom_point(aes(color=Location), size=4) +
  ylab("SQDG:PG") + xlab(my_x_title) + xlim(c(0, 1.55)) +
  geom_smooth(method = "lm", color="#000000", lwd=1.5) +
  coord_cartesian(ylim=c(0,8.1)) + 
  theme_bw() +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text = element_text(size = 24, face = "bold", color="black"),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 24))

ggsave(filename = "SQDPGxPhos_Scatter.png", plot = last_plot(), device = png(),
       path = "Images", width = 5.5, height = 3.5, units = "in")

cor(SQDPG_df$SQDGxPG, SQDPG_df$Phos)







# Grab SQDG and PG values alongside Chlorophyll and Pheo ----
SQDG_df_samples <- LOBdata %>% 
  filter(species=="SQDG") %>% 
  group_by(Orbi_num, Location, Station, Chla, Phaeo) %>%
  summarize("Total SQDG"=sum(intensity))
SQDG_Mont_samp <- filter(SQDG_df_samples, Location=="Monterey") %>% pull(`Total SQDG`)
SQDG_PtR_samp <- filter(SQDG_df_samples, Location=="Point Reyes") %>% pull(`Total SQDG`)

PG_df_samples <- LOBdata %>% 
  filter(species=="PG") %>% 
  group_by(Orbi_num, Location, Station, Chla, Phaeo) %>%
  summarize("PG"=sum(intensity))
PG_Mont_samp <- filter(PG_df_samples, Location=="Monterey") %>% pull(PG)
PG_PtR_samp <- filter(PG_df_samples, Location=="Point Reyes") %>% pull(PG)

SQDPG_df <- cbind(PG_df_samples, SQDG=SQDG_df_samples$`Total SQDG`)
SQDPG_df <- mutate(SQDPG_df, SQDGxPG=SQDG/PG)
SQDPG_df <- mutate(SQDPG_df, ChlxPheo=Phaeo/Chla)
SQDPG_df <- filter(SQDPG_df, ChlxPheo>0)

my_x_title <- expression(paste(" "^"0                                   ", "Pheophytin : Chlorophyll ", italic("a"), " "^"                                   0"))

ggplot(SQDPG_df, aes(x=ChlxPheo, y=SQDGxPG)) +
  geom_point(aes(color=Location), size=4) +
  ylab("") + xlab(my_x_title) + 
  coord_cartesian(ylim=c(0,8.1)) + 
  geom_smooth(method = "lm", color="#000000", lwd=1.5) +
  theme_bw() +
  theme(legend.title=element_blank(), legend.position = "none",
        axis.text = element_text(size = 24, face = "bold", color="black"),
        axis.title = element_text(size = 24, face = "bold", family="sans"),
        legend.text = element_text(size = 24))

ggsave(filename = "SQDPGxPheoChl_Scatter.png", plot = last_plot(), device = png(),
       path = "Images", width = 5, height = 3.5, units = "in")

cor(SQDPG_df$SQDGxPG, SQDPG_df$ChlxPheo)
