#load data

d <- read.csv("data/bee_data_PhilB.csv", header = TRUE, sep = ",")
str(d)

levels(d$Species) #need to correct space
#d$Gen_sp <- paste(d$NameGenus, d$NameSpecies, sep = "_")
d$Gen_sp <- d$species

sort(table(d$Gen_sp))
sum(table(d$Gen_sp))
head(d)

#date
date2 <- strptime(d$Date, "%d/%m/%Y") #several formats mixed in... will be painfull
d$jday <- date2$yday
d$year <- 1900 + date2$year
d$month <- date2$mon + 1
head(d$month)

#select fields to be independent
uni <- as.data.frame(cbind(d$Gen_sp, d$locality, d$region, d$year))
str(uni)
dup <- duplicated(uni)

# remove duplicates
d2 <- d[-which(dup == TRUE),]
str(d2)
sort(table(d2$Gen_sp)) #14 species with more than 30 datapoints

#visualize the data: #not with bees
#plot(d2$Long, d2$Lat)
#library(maps)
#map("world", col="red", add=TRUE)

#plot(d2$Long, d2$Lat, ylim = c(-50,-32), xlim = c(163,183))
#map("world", col="red", add=TRUE) #fix wrong lat/long


#create time periods
d4 <- d2
#for now
d4$year <- d4$year+1900#skip this?
d4$year <- d4$year
d4$time_period50  <- cut(d4$year, breaks = seq(1875,2025,50), labels = c("1875-1925","1925-1975","1975-2011"))

d4$time_period25  <- cut(d4$year, breaks = seq(1875,2025,25), labels = c("1875-1900","1900-1925","1925-1950","1950-1975","1975-2000","2000-2011"))
levels(d4$time_period25)

#d4$time_period_custom  <- cut(d4$year, breaks = c(1875,1945,1985,2025), labels = c("1875-1945","1950-1985","1985-2012"))

i = 3
break1 <- quantile(d4$year, probs = seq(0,1,1/i), na.rm = TRUE)
lab <- c(1:i)
for(j in 1:i){
    lab[j] <- paste(as.numeric(break1)[j],as.numeric(break1)[j+1], sep = "-")
}
d4$time_period_custom  <- cut(d4$year, breaks = break1, labels = lab)
levels(d4$time_period_custom) #fix year = 0!

#par(mfrow = c(1,3))
#temp <- subset(d4, time_period_custom == "0-1957")
#plot(temp$Long, temp$Lat, ylim = c(-50,-32), xlim = c(163,183), col = "blue")
#map("world", col="black", add=TRUE)
#temp <- subset(d4, time_period_custom == "1957-1976")
#plot(temp$Long, temp$Lat, ylim = c(-50,-32), xlim = c(163,183), col = "red")
#map("world", col="black", add=TRUE)
#temp <- subset(d4, time_period_custom == "1976-2007")
#plot(temp$Long, temp$Lat, ylim = c(-50,-32), xlim = c(163,183), col = "green")
#map("world", col="black", add=TRUE)


####species acumulation curve for years
library(vegan)

com <- table(d4$year,d4$Gen_sp)

x <- specaccum(com, method = "collector", permutations = 100, conditioned =TRUE, gamma = "jack1")
plot(x, add = FALSE, ci = 2, ci.type = "line", col = par("fg"), ci.col = col, ci.lty = 1, xlab = "Years",ylab = "Species", xaxt="n")
axis(side = 1, at= c(5,30,55,80,105,130), labels= c("1880","1905","1930","1955","1980","2010")) #check are the right ones.

temp <- as.numeric(row.names(com))
com_innverse <- com[order(-temp),]
x2 <- specaccum(com_innverse, method = "collector", permutations = 100, conditioned =TRUE, gamma = "jack1")
par(mfrow = c(1,3))
plot(x, add = FALSE, ci = 2, ci.type = "line", col = par("fg"), ci.col = col, ci.lty = 1, xlab = "Years",ylab = "Species", xaxt="n")
axis(side = 1, at= c(5,30,55,80,105,130), labels= c("1880","1905","1930","1955","1980","2010")) #check are the right ones.
plot(x2, add = FALSE, col = par("fg"), ci.col = col, ci.lty = 1, xlab = "Years",ylab = "Species", xaxt="n")
axis(side = 1, at= c(5,30,55,80,105,130), labels= c("2010","1980","1955","1930","1905","1880")) #check are the right ones.

x3 <- specaccum(com, method = "random", permutations = 100, conditioned =TRUE, gamma = "jack1")
plot(x3, add = FALSE, xlab = "Random number of years subsampled", ylab = "Species")


#######################################
Resampling Analysis
#######################################
par(mfrow = c(2,5), mar = c(4,3,1,1))

All <- d4
#fin year = 0 quick and dirty
length(which(All$year < 1900))
All <- subset(All, year > 1900)
for (i in 3:10){
    break1 <- quantile(All$year, probs = seq(0,1,1/i), na.rm = TRUE)
    lab <- c(1:i)
    for(j in 1:i){
        lab[j] <- paste(as.numeric(break1)[j],as.numeric(break1)[j+1], sep = "-")
    }
    All$time_period_quant  <- cut(All$year, breaks = break1, labels = lab)
    comm2 <- table(All$time_period_quant, factor(All$Gen_sp))
    (x2 <- rarefy(comm2,200, se = TRUE)) #same result as my resampling...
    #rar[[i-2+10]] <- x2
    #str(x2)
    time <- c(1:i)
    for(j in 1:i){
        time[j] <- (as.numeric(break1)[j] + as.numeric(break1)[j+1])/2
    }
    xx2 <- as.data.frame(x2)[1,]
    se <- as.data.frame(x2)[2,]
    if(i == 3){
        rarecurve(comm2, label = TRUE, step = 20, cex = 0.1, las = 2)
        abline(v = 200)}
    plot(time,t(xx2), xlab = "", las = 2, ylab = "species", xaxt = "n", ylim = c(min(xx2, na.rm = TRUE)- max(se), max(xx2, na.rm = TRUE)+ max(se)))
    axis(1, at = time ,labels = lab, las = 2, cex.axis = 0.7)
    arrows(time,as.numeric(xx2)+as.numeric(se), time, as.numeric(xx2)-as.numeric(se), angle=90, length = 0)
    m <- lm(t(xx2)~ time)
    summary(m)
    corr <- cor(t(xx2), time, use = "complete.obs")
    cor_dis <- c(1:1000)
    for (k in 1:1000){
        cor_dis[k] <- cor(sample(t(xx2),i), time, use = "complete.obs")
    }
    #hist(cor_dis)
    #lines(c(corr, corr), c(0,200), col = "red")
    (p <- pnorm(corr, mean = mean(cor_dis), sd = sd(cor_dis)))
    #with lm
    #if(summary(m)$coef[8]> 0.05 | is.na(summary(m)$coef[8]) == TRUE){
    #abline(m, lty = 2)}
    #else{
    #abline(m, lty = 1)}    
    #with corrs
    if(p > 0.05){
        abline(m, lty = 2)}
    else{
        abline(m, lty = 1)}    
    if(i == 10){
        rarecurve(comm2, label = TRUE, step = 20, cex = 0.1, las = 2)
        abline(v = 200)}    
    #rarecurve(comm2)
    #abline(v = 1000)
}


#############################
# species models
#############################
#collection history per year
B <- All
frq <- table(factor(B$Gen_sp),factor(B$year))
bin <- ifelse(frq > 0, 1, 0) 
str(bin)

i=1
last_year2 <- c(1:length(bin[,1]))
for (i in 1:length(bin[,1])){
    bin2 <- bin[i,-which(bin[i,] == 0)]
    last_year2[i] <- names(tail(bin2,1))
}
l_yBB <- data.frame(Gen_sp = rownames(bin), last_year = last_year2)
l_yBB #broken at i = 5 check.

#year vector
y <- as.numeric(colnames(bin))


#weights per year (specimens collected/max specimens collected)

effort <- c(1:length(y))
wei <- c(c(1:length(y)))
for (i in 1:length(y)){effort[i] <- sum(frq[,i])}
for (i in 1:length(y)){wei[i] <- sum(frq[,i])/max(effort)}

####check modles!###

sort(table(All$Gen_sp))
rownames(bin)
i=10
par(mfrow = c(1,2))

summary(m <- glm(bin[i,] ~ y, family = binomial(link=logit), na.action=na.omit, weight = wei)) #ok
p2 <- predict(m, list(y=y), type="response", se = TRUE)
plot(y, p2$fit, type = "l", ylim = c(0,1), xlab = "Year", ylab = "presence-absence in collection", main = as.character(rownames(frq)[i]))
lines(y, p2$fit+1.96*p2$se.fit, lty = 2)
lines(y, p2$fit-1.96*p2$se.fit , lty = 2)
points(y, bin[i,])
rug(jitter(B$year,10), ticksize = 0.02, col = rgb(0,0,0,alpha= 0.1))

#i=1
summary(g <- glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit)) #ok

#overdispersion if bigger than 1.5
deviance(g)/df.residual(g)


#plot
res <- rstandard(g)
plot(res ~ y, axes = F, ylab = "stand. dev. residuals",xlab = "year", pch = 16)
abline(h = 0)
axis(1, at = unique(y), labels = levels(y), cex.axis = 0.8)
axis(2)
title("standardized deviance residuals", cex.main = 0.9)
box()
#

m <- glm(cbind(frq[i,], effort-frq[i,]) ~y, family = binomial(link=logit), na.action=na.omit)
p2 <- predict(m, list(y=y), type="response", se = TRUE)
plot(y, p2$fit, type = "l", ylim = c(0,1), xlab = "Year", ylab = "Proportion in collection", main = as.character(rownames(frq)[i]))
lines(y, p2$fit+1.96*p2$se.fit, lty = 2)
lines(y, p2$fit-1.96*p2$se.fit , lty = 2)
points(y, (frq[i,]/effort))
rug(jitter(B$year,10), ticksize = 0.02, col = rgb(0,0,0,alpha= 0.1))

#######NOT UPDATED#########
#for overdispersed models
summary(g.over <- glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = quasibinomial(link=logit), na.action=na.omit)) 
#
summary(g.over)$dispersion

p.val <- pchisq(summary(g.over)$dispersion * g$df.residual, m$df.residual, lower = F)


#### estimates for all sp
#mB <- frq[,c(1:11)]
#colnames(mB) <- c("estimate_bin", "SE_bin", "p_bin","estimate_frq", "SE_frq", "p_frq", "pred1880", "pred 2010","pred1880SE", "pred 2010SE","Logit")
mB <- frq[,c(1:10)]
colnames(mB) <- c("estimate_bin", "SE_bin", "p_bin","estimate_frq", "SE_frq", "p_frq", "pred1880", "pred 2015","pred1880SE", "pred 2015SE")
try(for (i in 1:length(frq[,1])){
    mB[i,1] <- summary(glm(bin[i,] ~ y, family = binomial(link=logit), na.action=na.omit, weight = wei))$coefficients[2]
    mB[i,2] <- summary(glm(bin[i,] ~ y, family = binomial(link=logit), na.action=na.omit, weight = wei))$coefficients[4]
    mB[i,3] <- summary(glm(bin[i,] ~ y, family = binomial(link=logit), na.action=na.omit, weight = wei))$coefficients[8]
    if (deviance(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit))/df.residual(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit)) > 1.5) {
        mB[i,4] <- summary(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = quasibinomial(link=logit), na.action=na.omit))$coefficients[2]
        mB[i,5] <- summary(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = quasibinomial(link=logit), na.action=na.omit))$coefficients[4]
        mB[i,6] <- summary(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = quasibinomial(link=logit), na.action=na.omit))$coefficients[8]
        mB[i,7] <- predict(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = quasibinomial(link=logit), na.action=na.omit), newdata = data.frame(y = c(1880,2015)), type = "response", se.fit = TRUE)$fit[1]
        mB[i,8] <- predict(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = quasibinomial(link=logit), na.action=na.omit), newdata = data.frame(y = c(1880,2015)), type = "response", se.fit = TRUE)$fit[2]
        mB[i,9] <- predict(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = quasibinomial(link=logit), na.action=na.omit), newdata = data.frame(y = c(1880,2015)), type = "response", se.fit = TRUE)$se.fit[1]
        mB[i,10] <- predict(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = quasibinomial(link=logit), na.action=na.omit), newdata = data.frame(y = c(1880,2015)), type = "response", se.fit = TRUE)$se.fit[2]
    }
    #mB[i,11] <- plogis(summary(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = quasibinomial(link=logit), na.action=na.omit))$coefficients[2])-0.5
    else {
        mB[i,4] <- summary(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit))$coefficients[2]
        mB[i,5] <- summary(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit))$coefficients[4]
        mB[i,6] <- summary(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit))$coefficients[8]
        mB[i,7] <- predict(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit), newdata = data.frame(y = c(1880,2015)), type = "response", se.fit = TRUE)$fit[1]
        mB[i,8] <- predict(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit), newdata = data.frame(y = c(1880,2015)), type = "response", se.fit = TRUE)$fit[2]
        mB[i,9] <- predict(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit), newdata = data.frame(y = c(1880,2015)), type = "response", se.fit = TRUE)$se.fit[1]
        mB[i,10] <- predict(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit), newdata = data.frame(y = c(1880,2015)), type = "response", se.fit = TRUE)$se.fit[2]    	
    }
})


newdata = data.frame(y = rep(seq(from = 1901, to = 2010, length.out = 110)))


#Plot
pdf("Decline/Models_appendix/Bombus_models.pdf")
try(for (i in 1:length(frq[,1])){
    if (deviance(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit))/df.residual(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit)) > 1.5) {
        m <- glm(cbind(frq[i,], effort-frq[i,]) ~y, family = quasibinomial(link=logit), na.action=na.omit)
        p2 <- predict(m, list(y=y), type="response", se = TRUE)
        plot(y, p2$fit, type = "l", ylim = c(0,0.5), xlab = "Year", ylab = "Proportion in collection", main = as.character(rownames(frq)[i]))
        lines(y, p2$fit+1.96*p2$se.fit, lty = 2)
        lines(y, p2$fit-1.96*p2$se.fit , lty = 2)
        points(y, (frq[i,]/effort))
    }
    else{
        m <- glm(cbind(frq[i,], effort-frq[i,]) ~y, family = binomial(link=logit), na.action=na.omit)
        p2 <- predict(m, list(y=y), type="response", se = TRUE)
        plot(y, p2$fit, type = "l", ylim = c(0,0.5), xlab = "Year", ylab = "Proportion in collection", main = as.character(rownames(frq)[i]))
        lines(y, p2$fit+1.96*p2$se.fit, lty = 2)
        lines(y, p2$fit-1.96*p2$se.fit , lty = 2)
        points(y, (frq[i,]/effort))
    }
    rug(jitter(B$year,10), ticksize = 0.02, col = rgb(0,0,0,alpha= 0.1))
})
dev.off()

#######UNTIL HERE#######


try(for (i in 1:length(frq[,1])){
    if (deviance(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit))/df.residual(glm(cbind(frq[i,], effort-frq[i,]) ~ y, family = binomial(link=logit), na.action=na.omit)) > 1.5) {
        m <- glm(cbind(frq[i,], effort-frq[i,]) ~y, family = quasibinomial(link=logit), na.action=na.omit)
        p2 <- predict(m, list(y=y), type="response", se = TRUE)
    }
    else{
        m <- glm(cbind(frq[i,], effort-frq[i,]) ~y, family = binomial(link=logit), na.action=na.omit)
        p2 <- predict(m, list(y=y), type="response", se = TRUE)
    }
})

#plot with ggplot as facet 

library(dplyr)
library(reshape2)
library(ggplot2)

xx <- as.data.frame(mB)

levs <- c("pred1880",
          "pred 2015",
          "pred1880SE",
          "pred 2015SE")

decline <- filter(xx, Var2 %in% levs)

#add dompol/dompla status to metric d1 dataframe
decline <- decline %>%
    mutate(type = ifelse(Var2 %in% c("pred1880",  "pred 2015"),
                         "estimate", "SE"))

decline <- decline %>%
    mutate(year = ifelse(Var2 %in% c("pred1880",  "pred1880SE"),
                         "1880", "2015"))

decline.cast <- dcast(decline, Var1+year~ type, value.var="Freq")

p <- ggplot(decline.cast, aes(year, estimate, group=Var1))
p <- p + xlab(NULL) + ylab("Frequency")
p <- p + theme(text = element_text(size=18))
p <- p + geom_point(aes(colour=year), size = 3)
p <- p + geom_line(linetype="dashed", size = 0.75)
p <- p + geom_errorbar(aes(ymin=estimate-SE, ymax=estimate+SE, colour=year), width = 0)
p <- p + facet_wrap(~Var1, scale="free")
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
p <- p + scale_colour_brewer(palette = "Dark2")
p

