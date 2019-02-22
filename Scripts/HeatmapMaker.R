#Heatmap code

library(xlsx)
library(dplyr)
library(ggplot2)
library(clustsig)
library(gridExtra)
library(ggplotify)
library(reshape2)

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
sig.clus <- simprof(sig_matrix, method.cluster="complete")


hpdata <- melt(spec_df, id="species", variable.name = "sample", value.name = "Fold Change")
hp_order <- names(spec_df[-1])[sig.clus$hclust$order]
hp_labels <- paste0(sub("\\.D", " ", hp_order), "m")
hp_labels <- gsub(pattern = "S", "St.", hp_labels)

nspaces <- abs(8-as.numeric(lapply(strsplit(hp_labels, ""), length)))*2+5
nspaces[36] <- nspaces[36]-3
nspaces[33] <- nspaces[33]-4
nspaces[26] <- nspaces[26]-4
nspaces[9] <- nspaces[9]-7
nspaces[3] <- nspaces[3]-4

v <- list()
for(i in 1:length(nspaces)) {
  v[[i]] <- rep(" ", nspaces[i])
}
spx <- sapply(v, paste0, collapse = "")

hp_labels <- strsplit(hp_labels, " ")
x_ax <- character(0)
for(i in 1:length(nspaces)) {
  x_ax[i] <- paste0(hp_labels[[i]][1], spx[i], hp_labels[[i]][2])
}

hp_order <- rev(hp_order)
hp_labels <- rev(hp_labels)

spnames <- as.character(unique(hpdata$species))
y_ax <- c( "DNPPE", spnames[-grep("DNPPE", spnames)])
y_ax_lab <- c("Int. Std.", spnames[-grep("DNPPE", spnames)])
y_ax_lab[grep("DGTS_DGTA", y_ax_lab)] <- "DGTS/A"

ggplot(data = hpdata, aes(x=species, y=sample)) + 
  geom_tile(aes(fill=`Fold Change`), color="grey", lwd=1) +
  scale_fill_gradient2(low = "orchid4", high = "mediumseagreen", mid = "white", name = "Fold change") +
  scale_y_discrete(limits=hp_order, labels=rev(x_ax)) + 
  scale_x_discrete(limits=y_ax, labels=y_ax_lab)+
  ylab("") + xlab("") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 24, color="black"), 
        axis.text.y = element_text(size = 18, color="black"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 24),
        legend.position = "right", 
        plot.margin = unit(c(0.5,0.5,0,0), "in")) 

ggsave(filename = "Heatmap.png", plot = last_plot(), device = "png",
       path = "Images", width = 12.5, height = 10, units = "in")
