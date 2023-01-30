

lgram<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_gram_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_gram_mean - PctC_gram_se, 
                    ymax = PctC_gram_mean + PctC_gram_se,col=TRT),size=1.1) +
  scale_linetype_manual(values = c(2,1))+
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='Grass cover')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.7,label="*",size=9)+
  annotate("text",x=1998,y=0.7,label="*",size=9)+
  annotate("text",x=2008,y=0.7,label="*",size=9)+
  annotate("text",x=2013,y=0.7,label="*",size=9)+
  annotate("text",x=2016,y=0.7,label="*",size=9)+
  #annotate("text",x=2019,y=0.25,label="*",size=9)+
  ylim(0,0.75)+
  theme_bw()+
  theme(legend.position = "none")

lforb<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_forb_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_forb_mean - PctC_forb_se, 
                    ymax = PctC_forb_mean + PctC_forb_se,col=TRT),size=1.1) +
  scale_linetype_manual(values = c(2,1))+
  scale_alpha_manual(values = c(0.3,1))+
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='Forb cover')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.3,label="*",size=9)+
  annotate("text",x=1998,y=0.3,label="*",size=9)+
  ylim(0,.35)+
  theme_bw()+
  theme(legend.position = "none")


lexotic<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_exot_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_exot_mean - PctC_exot_se, 
                    ymax = PctC_exot_mean + PctC_exot_se,col=TRT),size=1.1) +
  scale_linetype_manual(values = c(2,1))+
  scale_alpha_manual(values = c(1))+
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='Non-native cover')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.55,label="*",size=9)+
  annotate("text",x=1998,y=0.55,label="*",size=9)+
  annotate("text",x=2008,y=0.55,label="*",size=9)+
  annotate("text",x=2013,y=0.55,label="*",size=9)+
  annotate("text",x=2016,y=0.55,label="*",size=9)+
  annotate("text",x=2019,y=0.55,label="*",size=9)+
  ylim(0,0.6)+
  theme_bw()+
  theme(legend.position = "none")


lnative<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_natv_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_natv_mean - PctC_natv_se, 
                    ymax = PctC_natv_mean + PctC_natv_se,col=TRT),size=1.1)+
  scale_linetype_manual(values = c(2,1))+
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='Non-native cover')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.55,label="*",size=9)+
  annotate("text",x=1998,y=0.55,label="*",size=9)+
  annotate("text",x=2008,y=0.55,label="*",size=9)+
  annotate("text",x=2013,y=0.55,label="*",size=9)+
  annotate("text",x=2016,y=0.55,label="*",size=9)+
  annotate("text",x=2019,y=0.55,label="*",size=9)+
  #  ylim(0,0.6)+
  theme_bw()+
  theme(legend.position = "none")




lnative

lbare|lgram|lforb|lexotic|lnative





natgra<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_ngra_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_ngra_mean - PctC_ngra_se, 
                    ymax = PctC_ngra_mean + PctC_ngra_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% cover",title='Native Grass Cover')+
  ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.9,label="*",size=9)+
  annotate("text",x=1998,y=0.9,label="*",size=9)+
  annotate("text",x=2019,y=0.9,label="*",size=9)+
  
  theme_bw()+
  theme(legend.position = "none")

natfor<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_nfor_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_nfor_mean - PctC_nfor_se, 
                    ymax = PctC_nfor_mean + PctC_nfor_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% cover",title='Native Forb Cover')+
  ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.9,label="*",size=9)+
  annotate("text",x=1998,y=0.9,label="*",size=9)+
  theme_bw()+
  theme(legend.position = "none")


exofor<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_efor_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_efor_mean - PctC_efor_se, 
                    ymax = PctC_efor_mean + PctC_efor_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% cover",title='Non-native Forb Cover')+
  ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  theme_bw()+
  theme(legend.position = "none")

exogra<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_egra_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_egra_mean - PctC_egra_se, 
                    ymax = PctC_egra_mean + PctC_egra_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% cover",title='Non-native Grass Cover')+
  ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.9,label="*",size=9)+
  annotate("text",x=1998,y=0.9,label="*",size=9)+
  annotate("text",x=2008,y=0.9,label="*",size=9)+
  annotate("text",x=2013,y=0.9,label="*",size=9)+
  annotate("text",x=2019,y=0.9,label="*",size=9)+
  theme_bw()+
  theme(legend.position = "none")




lbare<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_bare_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_bare_mean - PctC_bare_se, 
                    ymax = PctC_bare_mean + PctC_bare_se,col=TRT),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  scale_linetype_manual(values = c(2,1))+
  labs(x="Year",y="% Cover",title='Bare ground cover')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

lbare


totcov<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_plnt_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_plnt_mean - PctC_plnt_se, 
                    ymax = PctC_plnt_mean + PctC_plnt_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% cover",title='Total plant Cover')+
  ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.9,label="*",size=9)+
  annotate("text",x=2008,y=0.9,label="*",size=9)+
  theme_bw()+
  theme(legend.position = "none")





covrat<-ggplot(full_d)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=exotic_coverratio,fill=TRT),outlier.alpha=0.25,position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="Cover",title='Non:Native Cover Ratio')+
  geom_hline(yintercept = 1)+
  theme_bw()+
  annotate("text",x=1,y=8.75,label="*",size=9)+
  annotate("text",x=2,y=8.75,label="*",size=9)+
  annotate("text",x=3,y=8.75,label="*",size=9)+
  annotate("text",x=4,y=8.75,label="*",size=9)+
  annotate("text",x=5,y=8.75,label="*",size=9)+
  theme(legend.position = "none")




(covrat|totcov/lbare|natgra/exogra|natfor/exofor)








spprat<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_exoticsppratio_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_exoticsppratio_mean - PctC_exoticsppratio_se, 
                    ymax = PctC_exoticsppratio_mean + PctC_exoticsppratio_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="ratio of exotic:native species",title='Exotic:Native Species Ratio')+
  geom_hline(aes(yintercept = 1), lty = 3) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  theme_bw()+
  theme(legend.position = "none")


covrat<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_exoticcovratio_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_exoticcovratio_mean - PctC_exoticcovratio_se, 
                    ymax = PctC_exoticcovratio_mean + PctC_exoticcovratio_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="Ratio of exotic:native cover",title='Exotic:Native Cover Ratio')+
  geom_hline(aes(yintercept = 1), lty = 3) +
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  theme_bw()+
  theme(legend.position = "none")

spprat|covrat




















pbrin<-ggplot(subset(dft,variable=="prop_brin"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),alpha=0.75,stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurrence",title='BRIN')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")
pbrin
pbrte<-ggplot(subset(dft,variable=="prop_brte"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),alpha=0.75,stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurence",title='BRTE')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

pbrca<-ggplot(subset(dft,variable=="prop_brca"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),alpha=0.75,stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurrence",title='BRCA')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

plomu<-ggplot(subset(dft,variable=="prop_lomu"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),alpha=0.75,stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurence",title='LOMU')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

plope<-ggplot(subset(dft,variable=="prop_lope"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),alpha=0.75,stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurrence",title='LOPE')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

peltr<-ggplot(subset(dft,variable=="prop_eltr"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),alpha=0.75,stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurence",title='ELTR')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

pbocu<-ggplot(subset(dft,variable=="prop_bocu"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),alpha=0.75,stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurrence",title='BOCU')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

pscsc<-ggplot(subset(dft,variable=="prop_scsc"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),alpha=0.75,stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurence",title='SCSC')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")



presence<-(pbrca/plomu/plope/peltr|pbocu/pscsc/pbrin/pbrte)+
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

presence














alomu<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_lomu,fill=TRT),alpha=0.75,outlier.alpha=0.25,position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOMU')+
  ylim(0,1)+
  theme_bw()+
  annotate("text",x=1,y=0.9,label="*",size=9)+
  annotate("text",x=2,y=0.9,label="*",size=9)+
  theme(legend.position = "none")


abrca<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_brca,fill=TRT),alpha=0.75,outlier.alpha=0.25,position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRCA')+
  ylim(0,1)+
  theme_bw()+
  annotate("text",x=1,y=0.9,label="*",size=9)+
  annotate("text",x=2,y=0.9,label="*",size=9)+
  theme(legend.position = "none")

abrin<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_brin,fill=TRT),alpha=0.75,outlier.alpha=0.25,position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRIN')+
  ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1,y=0.9,label="*",size=9)+
  annotate("text",x=2,y=0.9,label="*",size=9)+
  annotate("text",x=3,y=0.9,label="*",size=9)+
  annotate("text",x=4,y=0.9,label="*",size=9)+
  annotate("text",x=5,y=0.9,label="*",size=9)+
  theme_bw()+
  theme(legend.position = "none")


abrte<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_brte,fill=TRT),outlier.alpha=0.25,alpha=0.75,position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRTE')+
  ylim(0,1)+
  theme_bw()+
  annotate("text",x=5,y=0.9,label="*",size=9)+
  theme(legend.position = "none")

aeltr<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_eltr,fill=TRT),outlier.alpha=0.25,alpha=0.75,position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='ELTR')+
  ylim(0,0.3)+
  theme_bw()+
  annotate("text",x=1,y=0.25,label="*",size=9)+
  annotate("text",x=2,y=0.25,label="*",size=9)+
  annotate("text",x=3,y=0.25,label="*",size=9)+
  theme(legend.position = "none")

abocu<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_bocu,fill=TRT),outlier.alpha=0.25,alpha=0.75,position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BOCU')+
  ylim(0,0.3)+
  annotate("text",x=2,y=0.25,label="*",size=9)+
  annotate("text",x=3,y=0.25,label="*",size=9)+
  annotate("text",x=4,y=0.25,label="*",size=9)+
  annotate("text",x=5,y=0.25,label="*",size=9)+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

ascsc<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_scsc,fill=TRT),outlier.alpha=0.25,alpha=0.75,position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='SCSC')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")



abundance<-(abrca/alomu/alope/aeltr|abocu/ascsc/abrin/abrte)+
  plot_layout(guides = "collect") & theme(legend.position = 'bottom') 

abundance





















png("./Figures/EDA/covrat.png",height = 10, width = 10, units = "cm", res=300, bg='white')
(covrat)

dev.off()




png("./Figures/EDA/groundcover.png",height = 15, width = 30, units = "cm", res=300, bg='white')
(covrat|totcov/lbare|natgra/exogra|natfor/exofor)

dev.off()

png("./Figures/EDA/groundcover2.png",height = 20, width = 30, units = "cm", res=300, bg='white')
(totcov/lbare|natgra/exogra|natfor/exofor)

dev.off()




png("./Figures/EDA/mainspp.png",height = 15, width = 30, units = "cm", res=300, bg='white')

(pbrca/abrca)|(plomu/alomu)|(pbrin/abrin)|(pbrte/abrte) 

dev.off()


png("./Figures/EDA/fewspp.png",height = 15, width = 22, units = "cm", res=300, bg='white')

(pbocu/abocu)|(peltr/aeltr)|(pscsc/ascsc)
dev.off()










