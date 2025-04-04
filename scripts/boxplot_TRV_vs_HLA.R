## This does the box plot (TRV gene usage vs HLA dosage) for Figure 4b

x<-c('scales','stringi','reshape2','plyr','ggpubr','wesanderson','ggrepel','ggbeeswarm')
lapply(x, require, character.only = TRUE)
library(stringi)

#### this takes as input a dataframe that has TRBV19 gene usage and HLA_B44 dosage and does a boxplot with overlying points

> head(df_TRBV19_HLA_B44)
      TRBV19 HLA_B44
1  0.1560419       0
2  0.6249559       1
3  0.8927333       1
4  0.1155616       0
5 -0.7926187       0
6  1.2055268       0

plot4<-ggplot(df_TRBV19_HLA_B44,aes(HLA_B44,TRBV19))+geom_boxplot(colour='grey60',fill='snow',alpha=0.2)+geom_beeswarm(pch=21,aes(fill=HLA_B44),size=2.5,alpha=0.75,colour='grey45',cex = 1.5)+ scale_fill_manual(values = wes_palette("FantasticFox1"))+theme_bw()+
     ylab("Normalised TRBV19 gene usage")+
    xlab("Copies of HLA-B*44 allele")+theme_classic()+
theme(axis.text.x = element_text(face = 'bold',angle = 45, vjust = 1, 
                                     size = 12, hjust = 1),axis.text=element_text(size=20),
        axis.title=element_text(size=20), 
        plot.title=element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20))
  ##ggtitle('TRBV19 versus HLA B44 allele carriage')
    ggsave(
  "TRBV19 vs HLA B44.png",
  plot=plot4,
  width = 10,
  height = 10,
  dpi = 400
)
