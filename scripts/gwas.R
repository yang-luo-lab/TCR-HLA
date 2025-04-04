### this does GWAS manhattan plot. Figure 3a shows all beta chains plotted together but this just does TRBV28

library(qqman)
### this is a modified version of the manhattan function in the qqman package
source("manhattan.R")

#### plink file has to be read in and a dataframe (df) with SNP, CHR, POS and P value has to be created
read.table("c1b_inrt.TRBV28*00.glm.linear",comment.char="",header=T)->lin
data.frame(lin$ID,lin$CHROM,lin$POS,lin$P)->df

#### this does the plot highlighting SNPs in the cis region of the TRBV gene
colnames(df)<-c("SNP","CHR","BP","P");
df[which(df$CHR!="0"),]->tmp3;
na.omit(tmp3)->tmp3;
tmp3[which(tmp3$CHR==7),]->tmp4
tmp4[which(tmp4$BP>141299011),]->tmp5
tmp5[which(tmp5$BP<143813287),"SNP"]->snpsOfInterest

png("c1b_inrt.TRBV28*00.glm.linear.Rd.png",type="cairo")
manhattan(tmp3, ylim = c(0, 50), cex = 0.6, cex.axis = 0.9, 
    col = c("blue2", "grey3"), highlight=snpsOfInterest, suggestiveline = F, genomewideline = F, chrlabs = c(1:22))
dev.off()
