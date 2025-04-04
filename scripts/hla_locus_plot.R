### this does individual hla plots eg figure 4a

library(ggplot2)
library(ggpubr)
library(grid)

getplot<-function(allmanh){
colnames(allmanh)<-c("SNP","CHR","BP","P");
allmanh[which(allmanh$CHR!="0"),]->tmp2
tmp2->tmp3
tmp2[,3]/1000000->tmp3[,3]
na.omit(tmp3)->data_no_na

#Define startswith function
startswith <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

#create a column to use for labelling based on GRCh37
data_no_na$colour_label <- ifelse(data_no_na$BP >= 29.909037 & data_no_na$BP <= 29.917349, "HLA-A",
                               ifelse(data_no_na$BP >= 31.321649 & data_no_na$BP <= 31.334844, "HLA-B",
                                      ifelse(data_no_na$BP >= 31.236526 & data_no_na$BP <= 31.239907, "HLA-C",
                                             ifelse(data_no_na$BP >= 32.595956 & data_no_na$BP <= 32.614839, "HLA-DQA1",
                                                    ifelse(data_no_na$BP >= 32.627244 & data_no_na$BP <= 32.636160, "HLA-DQB1",
                                                           ifelse(data_no_na$BP >= 33.032346 & data_no_na$BP <= 33.048552, "HLA-DPA1",
                                                                  ifelse(data_no_na$BP >= 33.043767 & data_no_na$BP <= 33.057473, "HLA-DPB1",
                                                                         ifelse(data_no_na$BP >= 32.545679 & data_no_na$BP <= 32.557625, "HLA-DRB1", "others"))))))))
 
 
data_no_na[which(data_no_na$colour_label=="others"),]->grey
data_no_na[which(data_no_na$colour_label!="others"),]->notgrey
rbind(grey,notgrey)->data_no_na2


legend_colour<-""

out1<- ggplot(data_no_na2) +
  geom_point(aes(x=BP,y=-log10(P), colour=colour_label)) +
  theme_classic() + 
  scale_color_manual(legend_colour,values = c("#FC4E07", "#008000", "black" ,"#386CB0", "#E7B800", "blue","#CC00FFFF", "#00AFBB","#808080"))+
  labs(x = "Chr 6 coordinates in Mb", y="-log(10)p") + scale_x_continuous(labels = scales::comma) + theme(
        axis.text=element_text(size=20), #change font size of axis text
        axis.title=element_text(size=20), #change font size of axis titles
        plot.title=element_text(size=20), #change font size of plot title
        legend.text=element_text(size=20), #change font size of legend text
        legend.title=element_text(size=20)) #change font size of legend title  
return(out1)
}

##### this starts from the plink output of glm


library(ggplot2)
read.table("./data/c1b_inrt.TRBV19*00.glm.linear",header=T,comment.char="")->lin;  
lin[which(lin$TEST=="ADD"),]->lin
data.frame(lin$ID,lin$X.CHROM,lin$POS,lin$P)->df
colnames(df)<-c("SNP","CHR","BP","P");
df[which(df$CHR!="0"),]->tmp3;
na.omit(tmp3)->tmp3;

getplot(tmp3)->out2
ggsave(
  "TRBV19_HLA.png",
  plot=out2,
  width = 10,
  height = 10,
  dpi = 400
)

