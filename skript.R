library(RcmdrMisc)
library(dplyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms
library(ggplot2) # for plotting
library(ggdendro)

# Příprava dat
## Načtení tabulky
data <-  openxlsx::read.xlsx("C:\\Users\\marek\\Desktop\\PodobnostBio\\2024.xlsx", sheet = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE)

data <- read.csv(".\\2024.csv", header=TRUE, stringsAsFactors=TRUE)

## Zobrazení prvních pěti řádků tabulky
head(data)

# Uprav data
drops <- c("Časová.značka")
data <- data[, !(names(data) %in% drops)]
names(data)[names(data) == "Jméno.a.příjmení."] <- "Jméno"

## Vyřešit duplikáty
data <- distinct(data,Jméno, .keep_all = TRUE)

## Zlepšit čitelnost
data2 <- data[, -1]
rownames(data2) <- data[, 1]

## Faktory na čísla
for (i in seq_len(ncol(data2))){
  data2[, i] <- as.numeric(data2[, i])
}

# Phylostrom
## Calculate pairwise distances using Euclidean distance
distances <- dist(data2)  # Remove the 'Jméno' column

## Create a heatmap to visualize distances
heatmap(as.matrix(distances),
        Rowv = NA, Colv = NA,
        col = colorRampPalette(c("white", "blue"))(100),
        xlab = "Participants", ylab = "Participants",
        main = "Participant Distances",
        labRow = data$Jméno, labCol = data$Jméno)

## Heatmapa s čísly
library(pheatmap)
ds <- as.matrix(distances)

pheatmap(ds, display_numbers = TRUE, clustering_method = "ward.D2")

# K klustry
set.seed(round(as.numeric(Sys.time())))

## function to compute total within-cluster sum of square
fviz_nbclust(data2, kmeans, method = "wss")

fviz_nbclust(data2, kmeans, method = "silhouette")

## compute gap statistic
gap_stat <- clusGap(data2, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

## Klustering
final <- kmeans(data2, 3, nstart = 25)
fviz_cluster(final, data = data2)

# Hierachical clustering
## Dissimilarity matrix
d <- dist(data2, method = "euclidean")
## Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

## Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

## AGNES
### methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

### function to compute coefficient
ac <- function(x) {
  agnes(data2, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(data2, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

## DIANA
### compute divisive hierarchical clustering
hc4 <- diana(data2)

### Divise coefficient; amount of clustering structure found
hc4$dc

### plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

## Pěknej grafík
### Ward's method
hc5 <- hclust(d, method = "ward.D2" )

### Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 3)

### Number of members in each cluster
table(sub_grp)

data2 %>%
  mutate(cluster = sub_grp) %>%
  head

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 5, border = 2:5)

fviz_cluster(list(data = data2, cluster = sub_grp))

# Bootstrap
library(shipunov)
bb <- Bclust(data2, FUN=function(.x)
 hclust(dist(.x, method="euclidean"), method="ward.D2"),iter=1000,
 mc.cores=1, monitor=TRUE, bootstrap=TRUE, relative=FALSE, hclist=NULL)
Bclabels(bb$hclust, bb$values, threshold=0.5, col="grey", pos=1)


# Force graph
library(qgraph)
distances <- as.matrix(distances)
set.seed(round(as.numeric(Sys.time())))
dist_mi <- 1/distances
qgraph(dist_mi, layout='spring', vsize=3)

#PDF
pdf('strom.pdf', paper = "a4r", width = 11.69, height = 8.26, encoding = "CP1250.enc")

par(mar=c(0,0,0,0) + 0.1)
plot(hc5, cex = 0.7,  main=NULL, xlab='', ylab=NULL, sub= '', axes = FALSE,)
# rect.hclust(hc5, k = 5, border = 2:5)

# fviz_cluster(list(data = data2, cluster = sub_grp))

pheatmap(ds, display_numbers = T, clustering_method = "ward.D2", fontsize = 5, fontsize_number = 3, number_format = "%.1f", legend = FALSE)

qgraph(dist_mi, layout='spring', vsize = 3, mar=c(1,1,1,1))

dev.off()

pdf('strom.pdf', width = 60, height = 60)
pheatmap(ds, display_numbers = T, clustering_method = "ward.D2")
dev.off()
