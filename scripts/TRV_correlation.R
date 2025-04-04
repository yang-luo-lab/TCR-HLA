#### This performs correlation between the TRV genes and plots a heatmap of the correlation

### It takes as input plink phenotype files with each phenotype in one column

> pheno_alpha[1:5,1:5]
  FID  IID    TRAV13.2      TRAV5    TRAV12.1
1  74 4_C1  1.12639113  0.8927333  0.21726735
2  76 5_C1  0.69988360  0.2585273  0.41792767
3  78 6_C1  1.03643339 -0.3745435  2.32634787
4  79 1_C2 -1.27023762 -2.3263479 -0.09539637
5  80 7_C1 -0.08532879 -0.9384757 -0.49585035

#### This performs the correlation 
tcrCorrelationsAlpha<-cor.tables(t(pheno_alpha[,c(3:45)]),t(pheno_alpha[,c(3:45)]))
colnames(tcrCorrelationsAlpha$estimate)<-row.names(tcrCorrelationsAlpha$estimate)
tcrCorrelations.E<-data.frame(tcrCorrelationsAlpha$estimate) 
tcrCorrelations.E$ID<-row.names(tcrCorrelations.E)
tcrCorrelations.Emelt <- melt(tcrCorrelations.E,id.vars=c('ID'))
tcrCorrelations.Emelt <- ddply(tcrCorrelations.Emelt, .(variable), transform,rescale = rescale(value))

colnames(tcrCorrelationsAlpha$p.value)<-row.names(tcrCorrelationsAlpha$p.value)
tcrCorrelations.P<-data.frame(tcrCorrelationsAlpha$p.value)
tcrCorrelations.P$ID<-row.names(tcrCorrelations.P)
tcrCorrelations.Pmelt <- melt(tcrCorrelations.P,id.vars=c('ID'))
tcrCorrelations.Pmelt <- ddply(tcrCorrelations.Pmelt, .(variable), transform,rescale = rescale(value))

## This does the heatmap

cormat <- round(cor(pheno_alpha[,c(3:45)],method = 'spearman'),2)
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)
melted_cormat$pair<-paste(melted_cormat$Var1,melted_cormat$Var2,sep="_")
tcrCorrelations.Pmelt$pair<-paste(tcrCorrelations.Pmelt$ID,tcrCorrelations.Pmelt$variable,sep="_")
melted_cormat$Pval<- tcrCorrelations.Pmelt$value[match(melted_cormat$pair,tcrCorrelations.Pmelt$pair)]

# Create a ggheatmap
ggheatmapPanAlpha <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "#0072B2", high = "#D55E00", mid ="snow", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Spearman\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 7, hjust = 1),axis.text.y=element_text(size=7))+
  ggtitle('pan alpha/delta correlation')+
    coord_fixed()+
    geom_text(aes(label=ifelse(Pval<3e-16,"",
                             ifelse(Pval<1e-6,"***",
                                    ifelse(Pval<1e-3,"**",
                                           ifelse(Pval<5e-2,"*","")
                                           )
                                    )
                             )
                  )
              ,colour="grey45")
