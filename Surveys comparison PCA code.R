### Saskatchewan survey PCA
### Fig. S15

### Packages
library(ggfortify)
library(plotly)
library(cluster)

### Surveys and experiment data
dugoutdata <- read.csv(file.choose(), header=TRUE)
#### select correct columns
dugdata <-dugoutdata[2:30]
### Make the PCA
pca_dug <-prcomp(dugdata, center=TRUE, scale=TRUE)
pca_dug
### Plot
pca.p <- autoplot(pca_dug, data = dugoutdata, colour='Group', fill='Group',
                  size=2, shape = 21,
        loadings=TRUE, loadings.colour='black',
         loadings.label=TRUE, loadings.label.colour='black', loadings.label.size=4)+
  theme_bw()+
  theme(legend.position = c(.1,.1))
### add colours
pca.p <- pca.p + scale_color_manual(values=c("orangered4", "goldenrod1", "navy", 'black'))+
  scale_fill_manual(values=c("orangered4", "goldenrod1", "navy", "white"))
pca.p
## save
ggsave("reservoir_survey_pca.pdf", pca.p, dpi=300, scale=1)
### Fig. S15
