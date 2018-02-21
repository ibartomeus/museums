#######################################
#plot number of records by year for flies
#######################################
library(ggplot2)

d2$count <- rep(1,nrow(d2)) # make new value column 
date.record.fly <- d2 %>% 
    group_by(year, Gen_sp) %>% 
    summarise(Freq = sum(count))

no.ind.fly <- as.data.frame(sort(table(d2$Gen_sp))) #14 species with more than 30 datapoints
species.remove.fly <- filter(no.ind.fly, Freq < 30)#create df with species that have less than 30 records
colnames(species.remove.fly)[1] <- "Gen_sp"
date.record.fly <- date.record.fly[!(date.record.fly$Gen_sp %in% species.remove.fly$Gen_sp),]

#plot fly records
p <- ggplot(date.record.fly, aes(year, Freq, colour=Gen_sp))
p <- p + xlab("Year") + ylab("Bin estimate")
p <- p + theme(text = element_text(size=18))
p <- p + geom_point(size = 1)
p <- p + facet_wrap(~Gen_sp, scales="free")
p <- p + theme(panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")) +
    theme(panel.border=element_rect(colour = "black", fill = "NA", size = 1)) +
    theme(axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =8),
          axis.title.y=element_text(size=30, vjust = 1),
          axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =8),
          axis.title.x=element_text(size=30, vjust = 1),
          axis.text=element_text(colour = "black"))+
    theme(strip.background = element_rect(colour="NA", fill=NA),
          strip.text = element_text(size=10))
p <- p + theme(legend.position="none")
p <- p + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p

#######################################
#plot number of records by year for bees
#######################################

d2$count <- rep(1,nrow(d2)) # make new value column 
date.record.bee <- exotic %>% 
    group_by(year) %>% 
    summarise(Freq = sum(count))

no.ind.bee <- as.data.frame(sort(table(date.record.bee$Gen_sp))) #14 species with more than 30 datapoints
species.remove.bee <- filter(no.ind.bee, Freq < 30)#create df with species that have less than 30 records
colnames(species.remove.bee)[1] <- "Gen_sp"
date.record.bee <- date.record.bee[!(date.record.bee$Gen_sp %in% species.remove.bee$Gen_sp),]

#plot bee records
p <- ggplot(date.record.bee, aes(year, Freq))
p <- p + xlab("Year") + ylab("Number of records all bees")
p <- p + theme(text = element_text(size=18))
p <- p + geom_point(size = 3)
p <- p + facet_wrap(~Gen_sp, scales="free_y")
p <- p + theme(panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")) +
    theme(panel.border=element_rect(colour = "black", fill = "NA", size = 1)) +
    theme(axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =8),
          axis.title.y=element_text(size=30, vjust = 1),
          axis.text.x=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =8),
          axis.title.x=element_text(size=30, vjust = 1),
          axis.text=element_text(colour = "black"))+
    theme(strip.background = element_rect(colour="NA", fill=NA),
          strip.text = element_text(size=10))
p <- p + theme(legend.position="none")
p <- p + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p

#######################################
#######################################


library(reshape2)
xx2_melt <- melt(xx2)
colnames(xx2_melt)[2] <- "estimate"
se_melt <- melt(se)
colnames(se_melt)[2] <- "se"
xx2_join <- merge(xx2_melt, se_melt, by="variable")
time <- as.data.frame(time)
xx <- cbind(xx2_join,time)
m <- lm(estimate~time, data=xx)
summary(m)

predict(m)

#plot chnages in fly richness across 10 time periods
p <- ggplot(xx2_join, aes(time, estimate))
p <- p + xlab(NULL) + ylab("Number of fly species")
p <- p + theme(text = element_text(size=18))
p <- p + geom_point(size = 3)
p <- p + geom_abline(intercept = 24.90350, slope = -0.05035,linetype="dashed")#intercpt seems too high??
p <- p + geom_errorbar(aes(ymin=estimate-se, ymax=estimate+se), width = 0)
p <- p + theme(panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black")) +
    theme(panel.border=element_rect(colour = "black", fill = "NA", size = 1)) +
    theme(axis.text.y=element_text(angle= 360, hjust = 0.5, vjust = 0.5, size =12),
          axis.title.y=element_text(size=30, vjust = 1),
          axis.text.x=element_text(angle= 90, hjust = 0.5, vjust = 0.5, size =12),
          axis.title.x=element_text(size=30, vjust = 1),
          axis.text=element_text(colour = "black"))+
    theme(strip.background = element_rect(colour="NA", fill=NA),
          strip.text = element_text(size=10))
p <- p + theme(legend.position="none")
p <- p + theme(axis.title.y=element_text(margin=margin(0,20,0,0)))
p


