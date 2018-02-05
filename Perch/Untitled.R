library(tidyverse)

lter_fish <- read_csv("https://lter.limnology.wisc.edu/file/12848/download?token=-0G3eTcJCnat50PvBQYK2hTp9d8BWIA447_iz0NcH94")

CR_Fish = lter_fish %>% filter(lakeid=="CR") %>% filter(spname =="YELLOWPERCH" | spname == "RAINBOWSMELT")
CR_Fish$CPUE = round(CR_Fish$total_caught/CR_Fish$effort,digits=3)
CR_Fish$spname = as.factor(CR_Fish$spname)

CR_VGN = CR_Fish %>% filter(grepl("VGN",gearid)) %>% group_by(year4,spname) %>% summarise(total_CPUE = sum(CPUE)) %>% complete(year4 ,spname) %>% replace(., is.na(.), 0)

myplot = ggplot(data=CR_VGN, aes(x=year4, y=total_CPUE, group=spname)) +
  geom_rect(aes(xmin = 2011.5, xmax = 2013.5, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.03) +
  geom_line(aes(color=spname)) +
  geom_point(aes(color=spname))

myplot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = c(0.15, 0.85)) + labs(x="Year",y="VGN CPUE", colour="Species")


CR_FYK = CR_Fish %>% filter(grepl("FYK",gearid)) %>% group_by(year4,spname) %>% summarise(total_CPUE = sum(CPUE)) %>% complete(year4 ,spname=c("YELLOWPERCH" ,"RAINBOWSMELT")) %>% replace(., is.na(.), 0)

myplot = ggplot(data=CR_FYK, aes(x=year4, y=total_CPUE, group=spname)) +
  geom_line(aes(color=spname)) +
  geom_point(aes(color=spname)) 

myplot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = c(0.5, 0.85)) + labs(x="Year",y="FYKE CPUE", colour="Species")

CR_BS = CR_Fish %>% filter(grepl("BS",gearid)) %>% group_by(year4,spname) %>% summarise(total_CPUE = sum(CPUE)) %>% expand(year4=full_seq(year4,1),spname) 

myplot = ggplot(data=CR_BS, aes(x=year4, y=total_CPUE, group=spname)) +
  geom_rect(aes(xmin = 2011.5, xmax = 2013.5, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.03) +
  geom_line(aes(color=spname)) +
  geom_point(aes(color=spname)) 

myplot + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),legend.position = c(0.15, 0.85)) + labs(x="Year",y="Beach Seine CPUE", colour="Species")

                                                                                                                                                                                                                                                                                    