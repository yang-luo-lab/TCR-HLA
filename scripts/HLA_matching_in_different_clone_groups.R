### This does Figure 5A
### This calculates proportion of HLA matched clones per individual in 3 different groups of clones - unstable (seen pre-treatment only), novel (seen post-treatment only) and persistent (seen both pre and post treatment)
### It takes as input 3 vectors - which are proportion of HLA matched clones for each individual across the 3 different clone groups

load("/data/prop_C1sd.Rd")
prop->propC1
load("/data/prop_C2sd.Rd")
prop->propC2
load("/data/prop_inter.Rd")
prop->propinter

data.frame(propC1,propC2,propinter)->df
df->df2

c(df2[,1],df2[,2],df2[,3])->allprop
c(rep("unstable",179),rep("novel",179),rep("persistent",179))->all
data.frame(as.factor(all),allprop)->df
colnames(df)<-c("group","proportion")
df[which(df[,2]!="Inf"),]->df

level_order <- c('unstable', 'persistent', 'novel') 

plot5<-ggplot(df,aes(x=factor(group,level=level_order),y=proportion))+geom_boxplot(colour='black',fill='white',alpha=0.2,width=.15)+geom_violin(aes(fill=group),alpha=0.75)+ scale_fill_manual(values = wes_palette("GrandBudapest1"))+theme_bw()+
    ylab("Proportion of HLA matched clones")+
    xlab("Clone Group")+theme_classic()+
theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20), 
        plot.title=element_text(size=20), 
        legend.text=element_text(size=20), 
        legend.title=element_text(size=20)) 
    ggsave(
  "clone_group_beta_chain.png",
  plot=plot5,
  width = 10,
  height = 10,
  dpi = 400)
