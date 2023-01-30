


lbare<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_bare_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_bare_mean - PctC_bare_se, 
                    ymax = PctC_bare_mean + PctC_bare_se,col=TRT),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  scale_linetype_manual(values = c(2,1))+
  labs(x="Year",y="% Cover",title='Bare ground cover')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.5,label="*",size=9)+
  annotate("text",x=2008,y=0.5,label="*",size=9)+
  ylim(0,0.55)+
  theme_bw()+
  theme(legend.position = "none")

lbare

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














llope<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_lope_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_lope_mean - PctC_lope_se, 
                    ymax = PctC_lope_mean + PctC_lope_se,col=TRT )) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOPE')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

lbrte<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_brte_mean,col=TRT,size=0.5))+
  geom_errorbar(aes(x=year, ymin = PctC_brte_mean - PctC_brte_se, 
                    ymax = PctC_brte_mean + PctC_brte_se,col=TRT )) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='brte')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")



peltr

dft<-subset(dft,variable!="nplot")
dft<-subset(dft,year!=2002)
dft$variable<-as.factor(dft$variable)
str(dft)

pbrin<-ggplot(subset(dft,variable=="prop_brin"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurrence",title='BRIN')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")
pbrin
pbrte<-ggplot(subset(dft,variable=="prop_brte"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurence",title='BRTE')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

pbrca<-ggplot(subset(dft,variable=="prop_brca"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurrence",title='BRCA')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

plomu<-ggplot(subset(dft,variable=="prop_lomu"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Occurence",title='LOMU')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

plope<-ggplot(subset(dft,variable=="prop_lope"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurrence",title='LOPE')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

peltr<-ggplot(subset(dft,variable=="prop_eltr"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurence",title='ELTR')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

pbocu<-ggplot(subset(dft,variable=="prop_bocu"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurrence",title='BOCU')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")

pscsc<-ggplot(subset(dft,variable=="prop_scsc"))+
  geom_bar(mapping=aes(x=as.factor(year),y=value*100,fill=TRT),stat="identity",position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% occurence",title='SCSC')+
  ylim(0,100)+
  theme_bw()+
  theme(legend.position = "none")



presence<-(pbrca/plomu/plope/peltr|pbocu/pscsc/pbrin/pbrte)+
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

presence


llomu<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_lomu_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_lomu_mean - PctC_lomu_se, 
                    ymax = PctC_lomu_mean + PctC_lomu_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOMU')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+  
  ylim(0,0.3)+
  annotate("text",x=1997,y=0.275,label="*",size=9)+
  annotate("text",x=1998,y=0.275,label="*",size=9)+
  theme(legend.position = "none")
llomu



llope<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_lope_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_lope_mean - PctC_lope_se, 
                    ymax = PctC_lope_mean + PctC_lope_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOPE')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  theme_bw()+
  ylim(0,0.02)+
  annotate("text",x=1997,y=0.015,label="*",size=9)+
  theme(legend.position = "none")

lbrte<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_brte_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_brte_mean - PctC_brte_se, 
                    ymax = PctC_brte_mean + PctC_brte_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRTE')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.175,label="*",size=9)+
  annotate("text",x=1998,y=0.175,label="*",size=9)+
  annotate("text",x=2019,y=0.175,label="*",size=9)+
  ylim(0,0.2)+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

lbrin<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_brin_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_brin_mean - PctC_brin_se, 
                    ymax = PctC_brin_mean + PctC_brin_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRIN')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.5,label="*",size=9)+
  annotate("text",x=1998,y=0.5,label="*",size=9)+
  annotate("text",x=2019,y=0.5,label="*",size=9)+
  annotate("text",x=2013,y=0.5,label="*",size=9)+
  annotate("text",x=2016,y=0.5,label="*",size=9)+
  annotate("text",x=2008,y=0.5,label="*",size=9)+
  ylim(0,0.55)+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

  
leltr<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_eltr_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_eltr_mean - PctC_eltr_se, 
                    ymax = PctC_eltr_mean + PctC_eltr_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='ELTR')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  #ylim(0,1)+
  annotate("text",x=1997,y=0.063,label="*",size=9)+
  annotate("text",x=1998,y=0.063,label="*",size=9)+
  annotate("text",x=2008,y=0.063,label="*",size=9)+
  ylim(0,0.075)+
  theme_bw()+
  theme(legend.position = "none")


lbocu<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_bocu_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_bocu_mean - PctC_bocu_se, 
                    ymax = PctC_bocu_mean + PctC_bocu_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BOCU')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.05,label="*",size=9)+
  annotate("text",x=1998,y=0.05,label="*",size=9)+
  annotate("text",x=2019,y=0.05,label="*",size=9)+
  annotate("text",x=2013,y=0.05,label="*",size=9)+
  annotate("text",x=2008,y=0.05,label="*",size=9)+
  ylim(0,0.06)+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

lscsc<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_scsc_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_scsc_mean - PctC_scsc_se, 
                    ymax = PctC_scsc_mean + PctC_scsc_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='SCSC')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  ylim(0,0.1)+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


lbrca<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_brca_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_brca_mean - PctC_brca_se, 
                    ymax = PctC_brca_mean + PctC_brca_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRCA')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.275,label="*",size=9)+
  annotate("text",x=1998,y=0.275,label="*",size=9)+
  ylim(0,0.3)+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


lcoca<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_coca_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_coca_mean - PctC_coca_se, 
                    ymax = PctC_coca_mean + PctC_coca_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='COCA')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.275,label="*",size=9)+
  annotate("text",x=1998,y=0.275,label="*",size=9)+
  ylim(0,0.3)+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")



abundance_lines<-(lbrca/llomu/llope/leltr|lbocu/lscsc/lbrin/lbrte)+
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

abundance_lines

alope<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_lope,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOPE')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

alomu<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_lomu,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='LOMU')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


abrca<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_brca,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRCA')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

abrin<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_brin,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRIN')+
  #ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1,y=1.1,label="*",size=9)+
  annotate("text",x=2,y=1.1,label="*",size=9)+
  annotate("text",x=3,y=1.1,label="*",size=9)+
  annotate("text",x=4,y=1.1,label="*",size=9)+
  annotate("text",x=5,y=1.1,label="*",size=9)+
  theme_bw()+
  theme(legend.position = "none")


abrte<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_brte,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BRTE')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

aeltr<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_eltr,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='ELTR')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

abocu<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_bocu,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='BOCU')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

ascsc<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_scsc,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='SCSC')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")



abundance<-(abrca/alomu/alope/aeltr|abocu/ascsc/abrin/abrte)+
  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

abundance

pbrca|abrca






spprat<-ggplot(full_d)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=exotic_sppratio,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="Richness",title='Non-native spp richness')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


natfor<-ggplot(full_d)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_nfor,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="Richness",title='Non-native spp richness')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


natgra<-ggplot(full_d)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_ngra,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="Richness",title='Non-native spp richness')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


exofor<-ggplot(full_d)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_efor,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="Richness",title='Non-native spp richness')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


exogra<-ggplot(full_d)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_egra,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="Richness",title='Non-native spp richness')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")






ggplot(subset(d,Full_name %in% reffnames))+
  geom_bar(mapping=aes(x=Transect_id,y=Canopy_cm,fill=Code),stat="identity",position=position_dodge())+
  scale_fill_manual(values=redblu)+
  facet_wrap(~year)+
  theme_classic()
  



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




covrat<-ggplot(full_d)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=exotic_coverratio,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="Richness",title='Non-native spp richness')+
  geom_hline(yintercept = 1)+
  theme_bw()+
  theme(legend.position = "none")


natgra<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_ngra_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_ngra_mean - PctC_ngra_se, 
                    ymax = PctC_ngra_mean + PctC_ngra_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% cover",title='Native Grass Cover')+
  #ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  theme_bw()+
  theme(legend.position = "none")

natfor<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_nfor_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_nfor_mean - PctC_nfor_se, 
                    ymax = PctC_nfor_mean + PctC_nfor_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% cover",title='Native Forb Cover')+
  #ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  theme_bw()+
  theme(legend.position = "none")


exofor<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_efor_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_efor_mean - PctC_efor_se, 
                    ymax = PctC_efor_mean + PctC_efor_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% cover",title='Non-native Forb Cover')+
  #ylim(0,1)+
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
  #ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  theme_bw()+
  theme(legend.position = "none")




totcov<-ggplot(d_mean)+
  geom_line(mapping=aes(x=(year),y=PctC_plnt_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_plnt_mean - PctC_plnt_se, 
                    ymax = PctC_plnt_mean + PctC_plnt_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% cover",title='Total plant Cover')+
  #ylim(0,1)+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  theme_bw()+
  theme(legend.position = "none")



totcov

lcoca<-ggplot(d_mean)+
  geom_line(mapping=aes(x=year,y=PctC_coca_mean,col=TRT),size=1.1)+
  geom_errorbar(aes(x=year, ymin = PctC_coca_mean - PctC_coca_se, 
                    ymax = PctC_coca_mean + PctC_coca_se,col=TRT ),size=1.1) +
  scale_color_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='COCA')+
  geom_vline(aes(xintercept = 1996.5), lty = 2) +
  geom_vline(aes(xintercept = 2011), lty = 2) +
  annotate("text",x=1997,y=0.275,label="*",size=9)+
  annotate("text",x=1998,y=0.275,label="*",size=9)+
  ylim(0,0.3)+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")



lgram


lbare|lgram|lforb|lexotic|lnative

(totcov/lbare|natgra/exogra|natfor/exofor)
(spprat|covrat)


full_d
mod<-glmmTMB(PctC_egra~TRT*yearfac+(1|Transect_id),data=full_d,family="gaussian")
summary(mod)
plot(simulateResiduals(mod))
em<-emmeans(mod,~year)
em
contrast(em)

visreg(mod,"TRT",by="dome_dnbr")
?visreg

mod<-glmmTMB(PctC_plnt~TRT+PctC_shrub+(1|Transect_id),data=full_d)
summary(mod)             
plot(simulateResiduals(mod))             
performance::r2(mod)


nonrich<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=Richness_exotic,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="Richness",title='Non-native spp richness')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")


nativerich<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=Richness_native,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="Richness",title='Native spp richness')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

cover<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=PctC_gram+PctC_forb,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='Non-native Cover')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

cover


nativecover<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=1-(PctC_bare+PctC_exot),fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='Non-native Cover')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

cover_prop<-ggplot(d_annual)+
  geom_boxplot(mapping=aes(x=as.factor(year),y=Prop_exotic,fill=TRT),position=position_dodge())+
  scale_fill_manual(values=pal2,"Treatment")+
  labs(x="Year",y="% Cover",title='Proportion of Non-Native Cover')+
  #ylim(0,1)+
  theme_bw()+
  theme(legend.position = "none")

cover

nonrich|nativerich

ascsc

dev.off()

lbare|lgram|lforb|lexotic











png("./Figures/EDA/presence.png",height = 20, width = 15, units = "cm", res=300, bg='white')
presence
#native_plot/brin_plot


dev.off()

png("./Figures/EDA/abundance.png",height = 20, width = 15, units = "cm", res=300, bg='white')
abundance_lines
#native_plot/brin_plot


dev.off()

png("./Figures/EDA/groundcover.png",height = 20, width = 20, units = "cm", res=300, bg='white')
lbare/lgram|lforb/lexotic

dev.off()


png("./Figures/EDA/mainspp.png",height = 15, width = 30, units = "cm", res=300, bg='white')

(pbrca/abrca)|(plomu/alomu)|(pbrin/abrin)|(pbrte/abrte) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

dev.off()
png("./Figures/EDA/fewspp.png",height = 15, width = 22, units = "cm", res=300, bg='white')

(pbocu/lbocu)|(peltr/leltr)|(pscsc/lscsc) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
dev.off()

png("./Figures/EDA/groundcover.png",height = 20, width = 20, units = "cm", res=300, bg='white')
(lbare/lgram|lforb/lexotic)
plot_layout(guides = "collect") & theme(legend.position = 'bottom')
dev.off()



png("./Figures/EDA/richness.png",height = 10, width = 25, units = "cm", res=300, bg='white')
nonrich|nativerich+plot_layout(guides = "collect") & theme(legend.position = 'right')
dev.off()


























####MAPS



########MAPS############

plot.df<-as.data.frame(pts,xy=T)

plot(plot.df)
names(plot.df)[2]<-"Transect_id"

red<-merge.data.frame(plot.df,d_annual)
red<-subset(red,year!=2002)

ext<-extent(buffer(pts,1500))

ext

plot(band)

red$brin_abs<-ifelse(red$PctC_brin>0,1,0)

red$brin_abs<-as.factor(red$brin_abs)

red_brin_prs<-subset(red,PctC_brin>0)
red_brin_abs<-subset(red,PctC_brin==0)

red_brte_prs<-subset(red,PctC_brte>0)
red_brte_abs<-subset(red,PctC_brte==0)

red_lolium_prs<-subset(red,PctC_lolium>0)
red_lolium_abs<-subset(red,PctC_lolium==0)

red_brca_prs<-subset(red,PctC_brca>0)
red_brca_abs<-subset(red,PctC_brca==0)

red_eltr_prs<-subset(red,PctC_eltr>0)
red_eltr_abs<-subset(red,PctC_eltr==0)

red_scsc_prs<-subset(red,PctC_scsc>0)
red_scsc_abs<-subset(red,PctC_scsc==0)

red_bocu_prs<-subset(red,PctC_bocu>0)
red_bocu_abs<-subset(red,PctC_bocu==0)



library(ggspatial)

year_facet=data.frame(year=2008)
red_brin_pre<-subset(red_brin_prs,year<2009)
red_brin_pst<-subset(red_brin_prs,year>2009)

red_brin_pre_ab<-subset(red_brin_abs,year<2009)
red_brin_pst_ab<-subset(red_brin_abs,year>2009)

red_brin_prs$fact<-cut(red_brin_prs$PctC_brin, breaks = c (0,.01,.05,.10,.25,.50, 1), labels = c("< 1","1 - 5", "5 - 10", "10 - 25", "25 - 50", "> 50"))
red_brin_prs$fact
red_brin_prs$fact<-ifelse(red_brin_prs$fact >= 0.50001 & red_brin_prs$fact < 1,"5",red_brin_prs$fact)
red_brin_prs$fact<-ifelse(red_brin_prs$fact <= 0.05,"1",red_brin_prs$fact)

red_brin_prs$fact<-ifelse(red_brin_prs$fact >= 0.049 & red_brin_prs$fact <= 0.15,"2",red_brin_prs$fact)
red_brin_prs$fact<-ifelse(red_brin_prs$fact >= 0.15 & red_brin_prs$fact <= 0.33,"3",red_brin_prs$fact)
red_brin_prs$fact<-ifelse(red_brin_prs$fact >= 0.33 & red_brin_prs$fact <= 0.50,"4",red_brin_prs$fact)
str(red_brin_prs)
as.factor(red_brin_prs$fact)

?scale_color_scico_d

red_brin_prs$fact


pal6<-pnw_palette("Sunset2",n=6)
pal1<-pnw_palette("Sunset2",n=7)
pal1<-pal1[1]

edf<-as.data.frame(elev_crop,xy=T)
names(edf)<-c("x","y","e")

countourLines(elev_crop,levels = 100)

ggplot()+
  geom_path(cont.line,mapping=aes(x=long,y=lat,group=group))


new_brin_pre_97<-ggplot()+
  geom_path(cont.line,mapping=aes(x=long,y=lat,group=group),alpha=0.15)+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  scale_shape_manual(values=c(17,16),"")+
  geom_point(subset(red_brin_prs,year==1997),mapping=aes(x=POINT_X,y=POINT_Y,col=fact,pch=TRT),alpha=0.9,size=4,show.legend = F)+
  annotate('text',label="1997",x=375740.1,y=3964043,size=7)+
  geom_jitter()+
  scale_color_manual(values=pal6)+
  new_scale(new_aes = "color")+
  new_scale(new_aes = "shape")+
  scale_shape_manual(values=c(2,1),"Treatment")+
  geom_point(subset(red_brin_abs,year==1997),mapping=aes(x=POINT_X,y=POINT_Y,col=as.factor(PctC_brin),pch=TRT),alpha=0.75,size=3,show.legend = F)+
  scale_color_manual(values=pal1)+
  coord_cartesian(xlim=c(371070.1      ,380410.1      ), ylim=c(3952433      ,3963853     ))+
  theme_bw()+ 
  annotation_north_arrow(location = "tr",style=north_arrow_minimal())+
  annotation_scale()+
  geom_polygon(unseed,mapping=aes(x=long,y=lat), fill="NA",col="black",lty=1,size=0.5,alpha=0.15)+
  labs(x="",y="")+
  theme(strip.text = element_text(size=12),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())


new_brin_pre_97

new_brin_pre_98<-ggplot()+
  geom_path(cont.line,mapping=aes(x=long,y=lat,group=group),alpha=0.15)+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  scale_color_manual(values=pal6)+  scale_shape_manual(values=c(17,16),"")+
  geom_point(subset(red_brin_prs,year==1998),mapping=aes(x=POINT_X,y=POINT_Y,col=fact,pch=TRT),alpha=0.9,size=4,show.legend = F)+
  annotate('text',label="1998",x=375740.1,y=3964043,size=7)+
  geom_jitter()+
  new_scale(new_aes = "color")+
  new_scale(new_aes = "shape")+
  scale_shape_manual(values=c(2,1),"Treatment")+
  geom_point(subset(red_brin_pre_ab,year==1998),mapping=aes(x=POINT_X,y=POINT_Y,col=as.factor(PctC_brin),pch=TRT),alpha=0.75,size=3,show.legend = F)+
  scale_color_manual(values=pal1)+
  coord_cartesian(xlim=c(371070.1      ,380410.1      ), ylim=c(3952433      ,3963853     ))+
  theme_bw()+ 
  labs(x="",y="")+
  geom_polygon(unseed,mapping=aes(x=long,y=lat), fill="NA",col="black",lty=1,size=0.5,alpha=0.15)+
  theme(strip.text = element_text(size=12),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())


new_brin_pre_98

new_brin_pre_08<-ggplot()+
  geom_path(cont.line,mapping=aes(x=long,y=lat,group=group),alpha=0.15)+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  scale_color_manual(values=pal6,"% cover BRIN")+
  scale_shape_manual(values=c(17,16),"Treatment")+
  geom_point(subset(red_brin_prs,year==2008),mapping=aes(x=POINT_X,y=POINT_Y,col=fact,pch=TRT),alpha=0.9,size=4)+
  annotate('text',label="2008",x=375740.1,y=3964043,size=7)+
  geom_jitter()+
  new_scale(new_aes = "color")+
  new_scale(new_aes = "shape")+
  scale_shape_manual(values=c(2,1),"Treatment")+
  geom_point(subset(red_brin_pre_ab,year==2008),mapping=aes(x=POINT_X,y=POINT_Y,col=as.factor(PctC_brin),pch=TRT),alpha=0.75,size=3,show.legend = F)+
  scale_color_manual(values=pal1)+
  coord_cartesian(xlim=c(371070.1      ,380410.1      ), ylim=c(3952433      ,3963853     ))+
  theme_bw()+ 
  labs(x="",y="")+
  geom_polygon(unseed,mapping=aes(x=long,y=lat), fill="NA",col="black",lty=1,size=0.5,alpha=0.15)+
  
  theme(strip.text = element_text(size=12),axis.text.x=element_blank(),axis.text.y=element_blank(),legend.position="none",axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(size=FALSE,alpha=FALSE,shape = guide_legend(override.aes = list(size = 5,pch=c(17,16))))
  

new_brin_pre_08


new_brin_pst_13<-ggplot()+
  geom_path(cont.line,mapping=aes(x=long,y=lat,group=group),alpha=0.15)+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray",lty=2,size=0.75,pattern_alpha=0.25)+
  scale_color_manual(values=pal6)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.1)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+  scale_shape_manual(values=c(17,16), "")+
  geom_point(subset(red_brin_prs,year==2013),mapping=aes(x=POINT_X,y=POINT_Y,col=fact,pch=TRT),size=4,alpha=0.9)+
  annotate('text',label="2013",x=375740.1,y=3964043,size=7)+
  geom_jitter()+
  scale_shape_manual(values=c(17,16),"Treatment")+
  new_scale(new_aes = "shape")+
  new_scale(new_aes = "color")+
  scale_shape_manual(values=c(2,1),"Treatment")+
  geom_point(subset(red_brin_abs,year==2013),mapping=aes(x=POINT_X,y=POINT_Y,col=as.factor(PctC_brin),pch=TRT),alpha=0.75,size=3,show.legend=FALSE)+
  scale_color_manual(values=pal1)+
  #coord_cartesian(xlim=c(370570.1    ,380110.1    ), ylim=c(3954033    ,3962253   ))+
  coord_cartesian(xlim=c(371070.1      ,380410.1      ), ylim=c(3952433      ,3963853     ))+
  theme_bw()+ 
  labs(x="",y="")+
  theme(strip.text = element_text(size=12),axis.text.x=element_blank(),axis.text.y=element_blank(),legend.position = "none",axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_polygon(unseed,mapping=aes(x=long,y=lat), fill="gray",col="black",lty=1,size=0.5,alpha=0.15)+
  guides(size=FALSE,alpha=FALSE,shape = guide_legend(override.aes = list(size = 5,pch=c(2,1))))


new_brin_pst_19<-ggplot()+
  geom_path(cont.line,mapping=aes(x=long,y=lat,group=group),alpha=0.15)+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray",lty=2,size=0.75,pattern_alpha=0.25)+
  scale_color_manual(values=pal6, "% Cover BRIN")+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.1)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+  scale_shape_manual(values=c(17,16), "")+
  geom_point(subset(red_brin_prs,year==2019),mapping=aes(x=POINT_X,y=POINT_Y,col=fact,pch=TRT),size=4,alpha=0.9)+
  annotate('text',label="2019",x=375740.1,y=3964043,size=7)+
  geom_jitter()+
  scale_shape_manual(values=c(17,16),"Treatment")+
  new_scale(new_aes = "shape")+
  new_scale(new_aes = "color")+
  scale_shape_manual(values=c(2,1),"Treatment")+
  geom_point(subset(red_brin_abs,year==2019),mapping=aes(x=POINT_X,y=POINT_Y,col=as.factor(PctC_brin),pch=TRT),alpha=0.75,size=3,show.legend=FALSE)+
  scale_color_manual(values=pal1)+
  #coord_cartesian(xlim=c(370570.1    ,380110.1    ), ylim=c(3954033    ,3962253   ))+
  coord_cartesian(xlim=c(371070.1      ,380410.1      ), ylim=c(3952433      ,3963853     ))+
  theme_bw()+ 
  geom_polygon(unseed,mapping=aes(x=long,y=lat), fill="gray",col="black",lty=1,size=0.5,alpha=0.15)+
  labs(x="",y="")+
  theme(strip.text = element_text(size=12),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())

new_brin_pst_19
plot(leg)
png("./Figures/EDA/brin_spatial.png",height = 27, width = 37.5, units = "cm", res=300, bg='white')
new_brin_pre_97/new_brin_pre_08|new_brin_pre_98/new_brin_pst_13|guide_area()/new_brin_pst_19 + 
  plot_layout(guides = 'collect') &  theme(legend.key.size = unit(1, 'cm'), #change legend key size
                                           legend.key.height = unit(1, 'cm'), #change legend key height
                                           legend.key.width = unit(1, 'cm'), #change legend key width
                                           legend.title = element_text(size=24), #change legend title font size
                                           legend.text = element_text(size=16)) 
dev.off()

png("./Figures/EDA/brin_spatial2.png",height = 20, width = 20, units = "cm", res=300, bg='white')
new_brin_pst_19
dev.off()




plot_layout(guides = "collect") & theme(legend.position = 'bottom')

brin_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon(unseed,mapping=aes(x=long,y=lat), fill="NA",col="gray",lty=1,size=2,alpha=0.25)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna",alpha=0.2)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna",alpha=0.2)+
  geom_point(red_brin_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=fact,pch=TRT),size=4,alpha=0.5,show.legend=FALSE)+
  #scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16), "")+
  scale_color_scico_d(palette="romaO",direction=1,begin=0.5,end=1,"% Cover")+
  
  #scale_size_continuous(range = c(1.5,4))+
  new_scale(new_aes = "shape")+
  geom_point(red_brin_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=brin_abs,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1),"Treatment")+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_brin,pch=TRT,col=brin_abs,size=PctC_brin),alpha=0.5)+
  #scale_alpha(range = c(0.4,0.75))+
  #scale_color_manual(values=c("NA","black"))+
  #scale_color_scico_d(palette="davos",direction=-1,begin=0,end=1,"% Cover")+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Smooth Brome (BRIN)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(370570.1 ,380110.1 ), ylim=c(3954033 ,3962253 ))+
  theme_void()+
  theme(legend.position = c(0.95,0.67))+
  annotation_scale(data=year_facet)+
  guides(size=FALSE,alpha=FALSE,shape = guide_legend(override.aes = list(size = 5,pch=c(17,16))))+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())


brin_spatial


lol_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_lolium_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  new_scale(new_aes = "shape")+
  geom_point(red_lolium_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_lolium,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_lolium,pch=TRT,col=lolium_abs,size=PctC_lolium),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="oslo",direction=1,begin=1,end=0)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Ryegrass (Lolium sp.)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())


brte_spatial<-ggplot()+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_brte_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  new_scale(new_aes = "shape")+
  geom_point(red_brte_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brte,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_brte,pch=TRT,col=brte_abs,size=PctC_brte),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="roma",direction=-1,begin=1,end=0.5)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Cheatgrass (BRTE)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())

brca_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_brca_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  new_scale(new_aes = "shape")+
  geom_point(red_brca_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brca,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_brca,pch=TRT,col=brca_abs,size=PctC_brca),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="roma",direction=-1,begin=1,end=0.5)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Mountain Brome (BRCA)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())

eltr_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_eltr_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  new_scale(new_aes = "shape")+
  geom_point(red_eltr_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_eltr,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_eltr,pch=TRT,col=eltr_abs,size=PctC_eltr),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="roma",direction=-1,begin=1,end=0.5)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Smooth Wheatgrass (ELTR)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())

bocu_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_bocu_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  new_scale(new_aes = "shape")+
  geom_point(red_bocu_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_bocu,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_bocu,pch=TRT,col=bocu_abs,size=PctC_bocu),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="roma",direction=-1,begin=1,end=0.5)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Sideoats Gramma (BOCU)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())



scsc_spatial<-ggplot()+
  geom_polygon_pattern(band.df,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="gray90",lty=5,size=0.5,pattern_alpha=0.25)+
  geom_polygon_pattern(unseed,mapping=aes(x=long,y=lat),pattern = "stripe", fill="NA",col="NA",lty=1,size=0.5,pattern_alpha=0.25,pattern_density=0.9)+
  geom_polygon(dome.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.4)+
  geom_polygon(conchas.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_polygon(juan.df,mapping=aes(x=long,y=lat),fill="sienna4",alpha=0.2)+
  geom_point(red_scsc_abs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_scsc,pch=TRT),alpha=0.75)+
  scale_shape_manual(values=c(2,1))+
  new_scale(new_aes = "shape")+
  geom_point(red_scsc_prs,mapping=aes(x=POINT_X,y=POINT_Y,col=PctC_brin,pch=TRT,alpha=PctC_brin),size=2)+
  scale_alpha(range=c(0.3,0.75))+
  scale_shape_manual(values=c(17,16))+
  scale_size_continuous(range = c(2,6))+
  #geom_point(red,mapping=aes(x=POINT_X,y=POINT_Y,fill=PctC_scsc,pch=TRT,col=scsc_abs,size=PctC_scsc),alpha=0.5)+
  #scale_alpha(range = c(0.5,1))+
  #scale_color_manual(values=c("NA","black"))+
  scale_color_scico(palette="roma",direction=-1,begin=1,end=0.5)+
  #scale_color_scico(palette = "lapaz",direction=-1)+
  #scale_color_scico(palette = "grayC",direction=-1)+
  facet_wrap(~year)+
  ggtitle("Little Bluestem (scsc)")+
  #coord_cartesian(xlim=c(370018 ,382032.5 ), ylim=c(3951525 ,3965654 ))+
  coord_cartesian(xlim=c(371070.1,379610.1), ylim=c(3954533,3961753))+
  theme_void()+
  theme(legend.position = 'none')+
  annotation_scale(data=year_facet)+
  annotation_north_arrow(data=year_facet, location = "tr",style=north_arrow_minimal())

scsc_spatial


png("./Figures/EDA/brin_spatial.png",height = 20, width = 32, units = "cm", res=300, bg='white')
brin_spatial
dev.off()

png("./Figures/EDA/lol_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
lol_spatial
dev.off()


png("./Figures/EDA/brte_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
brte_spatial
dev.off()

png("./Figures/EDA/brca_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
brca_spatial
dev.off()

png("./Figures/EDA/bocu_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
bocu_spatial
dev.off()

png("./Figures/EDA/eltr_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
eltr_spatial
dev.off()

png("./Figures/EDA/scsc_spatial.png",height = 15, width = 25, units = "cm", res=300, bg='white')
scsc_spatial
dev.off()

