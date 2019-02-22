#Dendrogram plotter

library(xlsx)
library(dplyr)
library(ggplot2)
library(clustsig)
library(gridExtra)
library(ggplotify)
library(reshape2)
library(dendextend)

IPDAG_samples <- read.csv("Data/IPDAG_samples.csv", stringsAsFactors = F)
stations <- read.xlsx("Data/Stations.xlsx", sheetName = "CleanStations", stringsAsFactors=T)
stations <- stations %>% arrange(Orbi.Number) %>% filter(DNPPE_type=="High") %>%
  mutate("SD"=paste0("S",Station," D",Depths))
names(IPDAG_samples)[grep("^Orbi", names(IPDAG_samples))] <- stations$SD

#Normalize each compound by its mean
IPDAG_samples$row_mean <- rowMeans(IPDAG_samples[grep("^S[0-9]", names(IPDAG_samples))])

tonorm_df <- IPDAG_samples[,grep("^S[0-9]", names(IPDAG_samples))]
level_df <- sweep(x=tonorm_df, MARGIN = 1, STATS = IPDAG_samples$row_mean, FUN = `/`)
sample_df <- cbind(species = IPDAG_samples$species, 
                   compound_name=IPDAG_samples$compound_name,
                   level_df)

#set DGCC and S_DGCC to be combined
sample_df$species <- gsub("S_DGCC", "DGCC", sample_df$species)

#group by species
specs <- unique(sample_df$species)

spec_df <- data.frame()
for(i in 1:length(specs)) {
  df <- filter(sample_df, species==specs[i]) %>% 
    select(grep("^S[0-9]", names(sample_df)))
  df <- as.data.frame(lapply(df, mean, na.rm=T))
  df <- cbind("species"=specs[i], df)
  spec_df <- rbind(spec_df, df)
}

spec2df <- spec_df

spec_df[grep("^S[0-9]", names(spec_df))] <- 
  log2(spec_df[grep("^S[0-9]", names(spec_df))])
spec_df <- do.call(data.frame, lapply(spec_df, function(x) replace(x, is.infinite(x),NA)))


###Significant cluster analysis
sig_matrix <- t(as.matrix(spec_df[-1]))

#dendro_all <- simprof(just_stations_clean, method.cluster="complete")
sig.clus <- simprof(sig_matrix, method.cluster="complete")

v <- simprof.plot(sig.clus, leafcolors=NA , plot=TRUE, fill=TRUE,
                  leaflab="none", siglinetype=1)


dend_final <- v %>% set("branches_lwd", 8) %>% set("labels", " ")
leafcols <- leaf_colors(dend_final)

png("Images/DendrogramPlot.png", width = 12.5, height = 10, units = "in", res = 300)
plot(dend_final)
dev.off()



