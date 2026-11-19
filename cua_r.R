library(dplyr)
library(ggplot2)
library(mgcv)
library(itsadug)
library(praatpicture)
library(hqmisc)
library(ggchangepoint)
library(tidyverse)
library(ggpmisc)
library(scales)
library(ggpubr)
library(Hmisc)
library(emmeans)
library(ggmap)
library(ggh4x)

#Convert F0 from hz to semitone relative to speaker's mean F0 
for(sp in levels(dfp$speaker)){
 # df.ST$ST[df.ST$speaker ==sp] <- f2st(df.ST$pitch[df.ST$speaker==sp], 100)
  #speaker avg 
  dfp$normF0[dfp$speaker==sp] <- 12 * (log2(dfp$pitch[dfp$speaker == sp]/mean(dfp$pitch[dfp$speaker == sp])))
 #print(sp)
# print(mean(df.ST$pitch[df.ST$speaker==sp]))
}

#helper functions 
#id the maxima and minima for a dataframe d, model m, and function f 
id <- function(d, m, f){
  if(f == "maxY"){
    return(max(m$fitted))
  }
  if(f == "minY"){ 
    return(min(m$fitted))
  }
  if(f == "maxX"){ 
    return(d$sample[which.max(m$fitted)])
  }
  if(f == "minX"){ 
    return(d$sample[which.min(m$fitted)])
  }
  if(f == "diff"){ 
    return(max(m$fitted) - min(m$fitted))
  }
  
  if(f == "change"){ 
      peaksX <- unique(d$sample[which(find_peaks(fitted(m)))])
      peaksY <- fitted(m)[peaksX]
      valleysX <- unique(d$sample[which(find_valleys(fitted(m)))])
      valleysY <- fitted(m)[valleysX]
      
      return(list("peaksX" = peaksX, "peaksY" = peaksY, "valleysX" = valleysX, "valleysY" = valleysY))
    }
}
#get f0 m at timepoint x
at <- function(m, x){ 
  return(predict(m, data.frame(sample=x)))
}
#get max velocity for a dataframe d and a model m 
vmax <- function(d, m){ 
  v <- (id(d, m, "minY")-id(d, m, "maxY"))/(id(d, m, "minX")-id(d, m, "maxX"))
  return(round(v, digits=2))  
}

#Figure 1
ggplot() +
  ## First layer: worldwide map
  geom_polygon(data = map_data("world"),
               aes(x=long, y=lat, group = group),
               color = '#FFFFFF00', fill = '#FFFFFF00') +
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region == "Botswana",],
               aes(x=long, y=lat, group = group),
               color = '#ababab', fill = '#dedede') +
  ## Second layer: Country map
  geom_polygon(data = map_data('world')[map_data('world')$region == "Botswana:Central District",],
               aes(x=long, y=lat, group = group),
               color = '#ababab', fill = 'black') +
  coord_map() +
  coord_fixed(1,
              xlim = c(19, 30),
              ylim = c(-27, -17)) +
  geom_point(mapping = aes(x = 25.906792, y = -24.653257), color = "black")+
  annotate("text", x = 26.977, y = -24.653257, label="Gaborone", size=3)+
  geom_point(mapping = aes(x = 25.3078083333, y = -23.6736638889), color = "black")+
  annotate("text", x = 27.15, y = -23.6736638889, label="Diphuduhudu", size=4.25)+
  theme(panel.background =element_rect(color = "#9c9c9c", fill = '#FFFFFF00'), panel.grid = element_line(color="#dedede"))+ 
  xlab("Longitude") + ylab("Latitude")

#Substudy 1 
#Figure 4 - Isolation
ggplot(NULL, mapping=aes(x=sample, y=pitch, color=tone, fill=tone, linetype=tone))+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  annotate("text", x=7, y=mean(df$pitch)+3, label=paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data=subset(df, subset = df$tone %in% c("HH", "HM", "HL", "MM", "LL", "LM")), aes(x=sample, y=pitch), alpha=0.1)+
  geom_smooth(data=df.dHM, aes(x=sample, y=pitch), alpha=0.1)+
  geom_smooth(data=df.dHL, aes(x=sample, y=pitch), alpha=0.1)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("Realization of Cua Tonal Categories (Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend(title="Category"), color= guide_legend(title="Category"), linetype=guide_legend(title="Category"))

#Figure 4 - Phrase
ggplot(NULL, mapping=aes(x=sample, y=pitch, color=tone, fill=tone, linetype=tone))+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  annotate("text", x=7, y=mean(dfp$pitch)-3, label=paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data=subset(dfp, subset = dfp$tone %in% c("HH", "HM", "HL", "MM", "LL", "LM")), aes(x=sample, y=pitch), alpha=0.1)+
  geom_smooth(data=dfp.dHM, aes(x=sample, y=pitch), alpha=0.1)+
  geom_smooth(data=dfp.dHL, aes(x=sample, y=pitch), alpha=0.1)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("Realization of Cua Tonal Categories (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend(title="Category"), color= guide_legend(title="Category"), linetype=guide_legend(title="Category"))

#Semitone Normalize Figure 4 - isolation (not included in paper)
ggplot(NULL, mapping=aes(x=sample, y=pitch, color=tone, fill=tone, linetype=tone))+
  geom_hline(yintercept = mean(df$normF0), linetype=2)+
  annotate("text", x=7, y=mean(df$normF0)+0.3, label=paste("Mean: ", round(mean(df$normF0), digits=2), "ST", sep=""), size=3)+
  geom_smooth(data=subset(df, subset = df$tone %in% c("HH", "HM", "HL", "MM", "LL", "LM")), aes(x=sample, y=normF0), alpha=0.1)+
  geom_smooth(data=df.dHM, aes(x=sample, y=normF0), alpha=0.1)+
  geom_smooth(data=df.dHL, aes(x=sample, y=normF0), alpha=0.1)+
  xlab("Time (Normalized)")+
  ylab("F0 (Semitone-Normalized)")+
  ggtitle("Cua Tonal Categories (Normalized, Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend(title="Category"), color= guide_legend(title="Category"), linetype=guide_legend(title="Category"))

#Semitone Normalize Figure 4 - phrase (not included in paper)
ggplot(NULL, mapping=aes(x=sample, y=pitch, color=tone, fill=tone, linetype=tone))+
  geom_hline(yintercept = mean(dfp$normF0), linetype=2)+
  annotate("text", x=7, y=mean(dfp$normF0)+0.3, label=paste("Mean: ", round(mean(dfp$normF0), digits=2), "ST", sep=""), size=3)+
  geom_smooth(data=subset(dfp, subset = dfp$tone %in% c("HH", "HM", "HL", "MM", "LL", "LM")), aes(x=sample, y=normF0), alpha=0.1)+
  geom_smooth(data=dfp.dHM, aes(x=sample, y=normF0), alpha=0.1)+
  geom_smooth(data=dfp.dHL, aes(x=sample, y=normF0), alpha=0.1)+
  xlab("Time (Normalized)")+
  ylab("F0 (Semitone-Normalized)")+
  ggtitle("Cua Tonal Categories (Normalized, Phrasal)")+   
  coord_cartesian(ylim=c(-4, 4))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend(title="Category"), color= guide_legend(title="Category"), linetype=guide_legend(title="Category"))

#Figure 5
ggplot(subset(df, df$tone %in% c("HH")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HH Category, Average Raw F0 (Isolation)")

#Figure 6 
ggplot(subset(dfp, dfp$tone %in% c("HH")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean_p, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean_p, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HH Category, Average Raw F0 (Phrasal)")

#Figure 7 - isolation 
HH.iso <- ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)-2, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(df, df$tone %in% c("HM","HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(df, df$tone=="HH"), mapping=aes(x=sample, y=pitch, color="HH", fill="HH"), alpha=0.1)+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+ggtitle("HH Category (Isolation)")+ 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(linetype=guide_legend(title="Other Categories"), fill = guide_legend(title="", order=1), color= guide_legend(title="", order=1))

HH.iso.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(df, df$tone=="HH"))

HH.iso+ 
geom_point(mapping = aes(x = id(df, HH.iso.model, "change")$peaksX, y = id(df, HH.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, HH.iso.model, "change")$valleysX, y = id(df, HH.iso.model, "change")$valleysY))+ 
  geom_text(mapping = aes(x = id(df, HH.iso.model, "change")$peaksX+5, y = (id(df, HH.iso.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(at(HH.iso.model, 1), digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, HH.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, HH.iso.model, "change")$valleysX-6, y = (id(df, HH.iso.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(at(HH.iso.model, 100), digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, HH.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, HH.iso.model, "maxX"), y = id(df, HH.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HH.iso.model, "minX"), y = id(df, HH.iso.model, "minY")), color="red", shape=21, size=3)+ 
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(df, HH.iso.model), "Hz/T", sep=""), size=3)


#Figure 7 - Phrase 
HH.p <- ggplot()+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)-2, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(dfp, dfp$tone %in% c("HM","HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(dfp, dfp$tone=="HH"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+
  xlab("Time (Normalized)")+ylab("F0 (Hz)")+
  ggtitle("HH Category (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(linetype=guide_legend("Other Categories"), fill = guide_legend("", order=1), color= guide_legend("", order=1))

HH.p.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(dfp, dfp$tone=="HH"))

HH.p + 
  geom_point(mapping = aes(x = id(dfp, HH.p.model, "change")$peaksX, y = id(dfp, HH.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(dfp, HH.p.model, "change")$valleysX, y = id(dfp, HH.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, HH.p.model, "change")$peaksX+5, y = (id(df, HH.p.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(at(HH.p.model, 1), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, HH.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(dfp, HH.p.model, "change")$valleysX-6, y = (id(df, HH.p.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(at(HH.p.model, 100), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, HH.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(dfp, HH.p.model, "maxX"), y = id(dfp, HH.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(dfp, HH.p.model, "minX"), y = id(dfp, HH.p.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(dfp, HH.p.model), "Hz/T", sep=""), size=3)


#Figure 35 - HH 
ggplot()+
  geom_hline(yintercept = mean(dfs$pitch, na.rm=T), linetype=2)+
  annotate("text", x=7, y=mean(dfs$pitch, na.rm=T)+3, label=paste("Mean: ", round(mean(dfs$pitch, na.rm=T), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","MM","LL","LM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","MM","LL","LM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = !(dfs.dHM$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = dfs.dHM$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset = !(dfs.dHL$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset =dfs.dHL$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & dfs$label %in% c("cie", "kwa", "qχ'ui")), mapping = aes(x=sample, y=pitch, color=label), alpha=0.1)+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  annotate("text", x=12, y=124, label="cíé")+
  annotate("text", x=30, y=124, label="kwà")+
  annotate("text", x=51, y=124, label="{exemplar}")+
  annotate("text", x=70, y=124, label="ka")+
  annotate("text", x=87, y=124, label="qχ'úī")+
  annotate("text", x=12, y=119, label="1SS")+
  annotate("text", x=30, y=119, label="PROG")+
  annotate("text", x=70, y=119, label="POST")+
  annotate("text", x=87, y=119, label="say")+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_cartesian(ylim=c(120, 200))+
  guides(color= guide_legend(title="", order=1), linetype=guide_legend(title="Other Categories"))+
  ggtitle("HH Category in Carrier Phrase")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values="red", limits=c("HH"))

#Figure 8
ggplot(subset(df, df$tone %in% c("HM")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HM Category, Average Raw F0 (Isolation)")

#Figure 9 
ggplot(subset(dfp, dfp$tone %in% c("HM")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean_p, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean_p, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HM Category, Average Raw F0 (Phrasal)")

#Figure 10 - Isolation
HM.iso <- ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)-2, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(df, df$tone %in% c("HH","HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(df, df$tone=="HM"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HM Category (Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

HM.iso.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(df, df$tone=="HM"))

HM.iso + 
  geom_point(mapping = aes(x = id(df, HM.iso.model, "change")$peaksX, y = id(df, HM.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, HM.iso.model, "change")$valleysX, y = id(df, HM.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, HM.iso.model, "change")$peaksX+6, y = (id(df, HM.iso.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(at(HM.iso.model, 1), digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, HM.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, HM.iso.model, "change")$valleysX-6, y = (id(df, HM.iso.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(at(HM.iso.model, 100), digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, HM.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, HM.iso.model, "maxX"), y = id(df, HM.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HM.iso.model, "minX"), y = id(df, HM.iso.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(df, HM.iso.model), "Hz/T", sep=""), size=3)

#Figure 10 - Phrase
HM.p <- ggplot()+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)-2, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(dfp, dfp$tone %in% c("HH","HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(dfp, dfp$tone=="HM"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HM Category (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

HM.p.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(dfp, dfp$tone=="HM"))

HM.p + 
  geom_point(mapping = aes(x = id(dfp, HM.p.model, "change")$peaksX, y = id(dfp, HM.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(dfp, HM.p.model, "change")$valleysX, y = id(dfp, HM.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(dfp, HM.p.model, "change")$peaksX+6, y = (id(dfp, HM.p.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(df, HM.p.model, "change")$peaksY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(df, HM.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(dfp, HM.p.model, "change")$valleysX-6, y = (id(dfp, HM.p.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(df, HM.p.model, "change")$valleysY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(df, HM.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(dfp, HM.p.model, "maxX"), y = id(dfp, HM.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(dfp, HM.p.model, "minX"), y = id(dfp, HM.p.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(df, HM.p.model), "Hz/T", sep=""), size=3)

#Figure 35 - HM 
ggplot()+
  geom_hline(yintercept = mean(dfs$pitch, na.rm=T), linetype=2)+
  annotate("text", x=7, y=mean(dfs$pitch, na.rm=T)+3, label=paste("Mean: ", round(mean(dfs$pitch, na.rm=T), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HL","MM","LL","LM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HL","MM","LL","LM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = !(dfs.dHM$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = dfs.dHM$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset = !(dfs.dHL$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset =dfs.dHL$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM") & dfs$label %in% c("cie", "kwa", "qχ'ui")), mapping = aes(x=sample, y=pitch, color=label), alpha=0.1)+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  annotate("text", x=12, y=124, label="cíé")+
  annotate("text", x=30, y=124, label="kwà")+
  annotate("text", x=51, y=124, label="{exemplar}")+
  annotate("text", x=70, y=124, label="ka")+
  annotate("text", x=87, y=124, label="qχ'úī")+
  annotate("text", x=12, y=119, label="1SS")+
  annotate("text", x=30, y=119, label="PROG")+
  annotate("text", x=70, y=119, label="POST")+
  annotate("text", x=87, y=119, label="say")+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_cartesian(ylim=c(120, 200))+
  guides(color= guide_legend(title="", order=1), linetype=guide_legend(title="Other Categories"))+
  ggtitle("HM Category in Carrier Phrase")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values="red", limits=c("HM"))


#Figure 11
ggplot(subset(df, df$tone %in% c("HL")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HL Category, Average Raw F0 (Isolation)")

#Figure 12 
ggplot(subset(dfp, dfp$tone %in% c("HL")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean_p, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean_p, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HL Category, Average Raw F0 (Phrasal)")

#Figure 13 - Isolation
HL.iso <- ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)-2, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(df, df$tone %in% c("HH","HM","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(df, df$tone=="HL"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HL Category (Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

HL.iso.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(df, df$tone=="HL"))

HL.iso + 
  geom_point(mapping = aes(x = id(df, HL.iso.model, "change")$peaksX, y = id(df, HL.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, HL.iso.model, "change")$valleysX, y = id(df, HL.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, HL.iso.model, "change")$peaksX+6, y = (id(df, HL.iso.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(df, HL.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, HL.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, HL.iso.model, "change")$valleysX-6, y = (id(df, HL.iso.model, "change")$valleysY)+5, label = paste(sprintf("%+00.2f", (round(id(df, HL.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, HL.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, HL.iso.model, "maxX"), y = id(df, HL.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HL.iso.model, "minX"), y = id(df, HL.iso.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(df, HL.iso.model), "Hz/T", sep=""), size=3)

#Figure 15 - Phrase 
HL.p <- ggplot()+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)-2, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(dfp, dfp$tone %in% c("HH","HM","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(dfp, dfp$tone=="HL"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HL Category (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

HL.p.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(dfp, dfp$tone=="HL"))

HL.p + 
  geom_point(mapping = aes(x = id(dfp, HL.p.model, "change")$peaksX, y = id(dfp, HL.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(dfp, HL.p.model, "change")$valleysX, y = id(dfp, HL.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(dfp, HL.p.model, "change")$peaksX+5, y = (id(dfp, HL.p.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(dfp, HL.p.model, "change")$peaksY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, HL.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(dfp, HL.p.model, "change")$valleysX-6, y = (id(dfp, HL.p.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(dfp, HL.p.model, "change")$valleysY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, HL.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(dfp, HL.p.model, "maxX"), y = id(dfp, HL.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(dfp, HL.p.model, "minX"), y = id(dfp, HL.p.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(dfp, HL.p.model), "Hz/T", sep=""), size=3)

#Figure 35 - HL 
ggplot()+
  geom_hline(yintercept = mean(dfs$pitch, na.rm=T), linetype=2)+
  annotate("text", x=7, y=mean(dfs$pitch, na.rm=T)+3, label=paste("Mean: ", round(mean(dfs$pitch, na.rm=T), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","MM","LL","LM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","MM","LL","LM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = !(dfs.dHM$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = dfs.dHM$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset = !(dfs.dHL$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset =dfs.dHL$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HL") & dfs$label %in% c("cie", "kwa", "qχ'ui")), mapping = aes(x=sample, y=pitch, color=label), alpha=0.1)+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HL") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HL") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  annotate("text", x=12, y=124, label="cíé")+
  annotate("text", x=30, y=124, label="kwà")+
  annotate("text", x=51, y=124, label="{exemplar}")+
  annotate("text", x=70, y=124, label="ka")+
  annotate("text", x=87, y=124, label="qχ'úī")+
  annotate("text", x=12, y=119, label="1SS")+
  annotate("text", x=30, y=119, label="PROG")+
  annotate("text", x=70, y=119, label="POST")+
  annotate("text", x=87, y=119, label="say")+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_cartesian(ylim=c(120, 200))+
  guides(color= guide_legend(title="", order=1), linetype=guide_legend(title="Other Categories"))+
  ggtitle("HL Category in Carrier Phrase")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values="red", limits=c("HL"))

#Figure 14 
ggplot(subset(df, df$tone %in% c("MM")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 160), breaks = c(260, 220, 190, 160)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("MM Category, Average Raw F0 (Isolation)")

#Figure 15 
ggplot(subset(dfp, dfp$tone %in% c("MM")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean_p, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean_p, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 160), breaks = c(260, 220, 190, 160)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("MM Category, Average Raw F0 (Phrasal)")

#Figure 16 - Isolation
MM.iso <- ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)-2, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(df, df$tone %in% c("HH","HM","HL","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(df, df$tone=="MM"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("MM Category (Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

MM.iso.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(df, df$tone=="MM"))

MM.iso + 
  geom_point(mapping = aes(x = id(df, MM.iso.model, "change")$peaksX, y = id(df, MM.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, MM.iso.model, "change")$valleysX, y = id(df, MM.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, MM.iso.model, "change")$peaksX+5, y = (id(df, MM.iso.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(df, MM.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, MM.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, MM.iso.model, "change")$valleysX, y = (id(df, MM.iso.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(df, MM.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, MM.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, MM.iso.model, "maxX"), y = id(df, MM.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, MM.iso.model, "minX"), y = id(df, MM.iso.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(dfp, MM.iso.model), "Hz/T", sep=""), size=3)

#Figure 16 - Phrase 
MM.p <- ggplot()+  
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)-2, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(dfp, dfp$tone %in% c("HH","HM","HL","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(dfp, dfp$tone=="MM"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("MM Category (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

MM.p.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(dfp, dfp$tone=="MM"))

MM.p + 
  geom_point(mapping = aes(x = id(dfp, MM.p.model, "change")$peaksX, y = id(dfp, MM.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(dfp, MM.p.model, "change")$valleysX, y = id(dfp, MM.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(dfp, MM.p.model, "change")$peaksX, y = (id(dfp, MM.p.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(dfp, MM.p.model, "change")$peaksY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, MM.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(dfp, MM.p.model, "change")$valleysX-6, y = (id(dfp, MM.p.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(dfp, MM.p.model, "change")$valleysY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, MM.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(dfp, MM.p.model, "maxX"), y = id(dfp, MM.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(dfp, MM.p.model, "minX"), y = id(dfp, MM.p.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(dfp, MM.p.model), "Hz/T", sep=""), size=3)

#Figure 35- MM  
ggplot()+
  geom_hline(yintercept = mean(dfs$pitch, na.rm=T), linetype=2)+
  annotate("text", x=7, y=mean(dfs$pitch, na.rm=T)+3, label=paste("Mean: ", round(mean(dfs$pitch, na.rm=T), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LL","LM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LL","LM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = !(dfs.dHM$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = dfs.dHM$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset = !(dfs.dHL$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset =dfs.dHL$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("MM") & dfs$label %in% c("cie", "kwa", "qχ'ui")), mapping = aes(x=sample, y=pitch, color=label), alpha=0.1)+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("MM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("MM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  annotate("text", x=12, y=124, label="cíé")+
  annotate("text", x=30, y=124, label="kwà")+
  annotate("text", x=51, y=124, label="{exemplar}")+
  annotate("text", x=70, y=124, label="ka")+
  annotate("text", x=87, y=124, label="qχ'úī")+
  annotate("text", x=12, y=119, label="1SS")+
  annotate("text", x=30, y=119, label="PROG")+
  annotate("text", x=70, y=119, label="POST")+
  annotate("text", x=87, y=119, label="say")+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_cartesian(ylim=c(120, 200))+
  guides(color= guide_legend(title="", order=1), linetype=guide_legend(title="Other Categories"))+
  ggtitle("MM Category in Carrier Phrase")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values="red", limits=c("MM"))

#Figure 18 
ggplot(subset(df, df$tone %in% c("LL")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean, mapping = aes(x = 80, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("LL Category, Average Raw F0 (Isolation)")

#Figure 19
ggplot(subset(dfp, dfp$tone %in% c("LL")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean_p, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean_p, mapping = aes(x = 80, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("LL Category, Average Raw F0 (Phrasal)")

#Figure 20 - Isolation
LL.iso <- ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)+3, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(df, df$tone %in% c("HH","HM","HL","MM", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(df, df$tone=="LL"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("LL Category (Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

LL.iso.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(df, df$tone=="LL"))

LL.iso+ 
  geom_point(mapping = aes(x = id(df, LL.iso.model, "change")$peaksX, y = id(df, LL.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, LL.iso.model, "change")$valleysX, y = id(df, LL.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, LL.iso.model, "change")$peaksX+5, y = (id(df, LL.iso.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(df, LL.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, LL.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, LL.iso.model, "change")$valleysX-4, y = (id(df, LL.iso.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(df, LL.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, LL.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, LL.iso.model, "maxX"), y = id(df, LL.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, LL.iso.model, "minX"), y = id(df, LL.iso.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", round((at(LL.iso.model, 100)-at(LL.iso.model, 54))/(100-54), digits=2), "Hz/T", sep=""), size=3)

#Figure 20 - Phrase 
LL.p <- ggplot()+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)-3, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(dfp, dfp$tone %in% c("HH","HM","HL","MM", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(dfp, dfp$tone=="LL"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("LL Category (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

LL.p.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(dfp, dfp$tone=="LL"))

LL.p+ 
  geom_point(mapping = aes(x = id(dfp, LL.p.model, "change")$peaksX, y = id(dfp, LL.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(dfp, LL.p.model, "change")$valleysX, y = id(dfp, LL.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(dfp, LL.p.model, "change")$peaksX+3, y = (id(dfp, LL.p.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(dfp, LL.p.model, "change")$peaksY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, LL.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(dfp, LL.p.model, "change")$valleysX-6, y = (id(dfp, LL.p.model, "change")$valleysY)-2.5, label = paste(sprintf("%+00.2f", (round(id(dfp, LL.p.model, "change")$valleysY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, LL.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(dfp, LL.p.model, "maxX"), y = id(dfp, LL.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(dfp, LL.p.model, "minX"), y = id(dfp, LL.p.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(dfp, LL.p.model), "Hz/T", sep=""), size=3)

#Figure 35 - LL
ggplot()+
  geom_hline(yintercept = mean(dfs$pitch, na.rm=T), linetype=2)+
  annotate("text", x=7, y=mean(dfs$pitch, na.rm=T)+3, label=paste("Mean: ", round(mean(dfs$pitch, na.rm=T), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LM","MM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LM","MM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = !(dfs.dHM$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = dfs.dHM$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset = !(dfs.dHL$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset =dfs.dHL$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("LL") & dfs$label %in% c("cie", "kwa", "qχ'ui")), mapping = aes(x=sample, y=pitch, color=label), alpha=0.1)+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("LL") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("LL") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  annotate("text", x=12, y=124, label="cíé")+
  annotate("text", x=30, y=124, label="kwà")+
  annotate("text", x=51, y=124, label="{exemplar}")+
  annotate("text", x=70, y=124, label="ka")+
  annotate("text", x=87, y=124, label="qχ'úī")+
  annotate("text", x=12, y=119, label="1SS")+
  annotate("text", x=30, y=119, label="PROG")+
  annotate("text", x=70, y=119, label="POST")+
  annotate("text", x=87, y=119, label="say")+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_cartesian(ylim=c(120, 200))+
  guides(color= guide_legend(title="", order=1), linetype=guide_legend(title="Other Categories"))+
  ggtitle("LL Category in Carrier Phrase")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values="red", limits=c("LL"))

#Figure 21
ggplot(subset(df, df$tone %in% c("LM")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean, mapping = aes(x = 80, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("LM Category, Average Raw F0 (Isolation)")

#Figure 22
ggplot(subset(dfp, dfp$tone %in% c("LM")), mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean_p, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean_p, mapping = aes(x = 80, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("LM Category, Average Raw F0 (Phrasal)")

#Figure 23 - Isolation 
LM.iso <- ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)+3, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(df, df$tone %in% c("HH","HM","HL","LL", "MM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(df, df$tone=="LM"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("LM Category (Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

LM.iso.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(df, df$tone=="LM"))

LM.iso + 
  geom_point(mapping = aes(x = id(df, LM.iso.model, "change")$peaksX, y = id(df, LM.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, LM.iso.model, "change")$valleysX, y = id(df, LM.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, LM.iso.model, "change")$peaksX+5, y = (id(df, LM.iso.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(df, LM.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, LM.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, LM.iso.model, "change")$valleysX-6, y = (id(df, LM.iso.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(df, LM.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, LM.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, LM.iso.model, "maxX"), y = id(df, LM.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, LM.iso.model, "minX"), y = id(df, LM.iso.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(df, LM.iso.model), "Hz/T", sep=""), size=3)

#Figure 23 -  Phrase
LM.p <- ggplot()+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)-3, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(dfp, dfp$tone %in% c("HH","HM","HL","LL", "MM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(dfp, dfp$tone=="LM"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("LM Category (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

LM.p.model   <- gam(pitch ~ s(sample, bs = "cs"), data = subset(dfp, dfp$tone=="LM"))

LM.p + 
  geom_point(mapping = aes(x = id(dfp, LM.p.model, "change")$peaksX, y = id(dfp, LM.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(dfp, LM.p.model, "change")$valleysX, y = id(dfp, LM.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(dfp, LM.p.model, "change")$peaksX+5, y = (id(dfp, LM.p.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(dfp, LM.p.model, "change")$peaksY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, LM.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(dfp, LM.p.model, "change")$valleysX-6, y = (id(dfp, LM.p.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(dfp, LM.p.model, "change")$valleysY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, LM.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(dfp, LM.p.model, "maxX"), y = id(dfp, LM.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(dfp, LM.p.model, "minX"), y = id(dfp, LM.p.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(dfp, LM.p.model), "Hz/T", sep=""), size=3)

#Figure 35 - LM
ggplot()+
  geom_hline(yintercept = mean(dfs$pitch, na.rm=T), linetype=2)+
  annotate("text", x=7, y=mean(dfs$pitch, na.rm=T)+3, label=paste("Mean: ", round(mean(dfs$pitch, na.rm=T), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LL","MM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LL","MM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = !(dfs.dHM$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = dfs.dHM$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset = !(dfs.dHL$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset =dfs.dHL$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("LM") & dfs$label %in% c("cie", "kwa", "qχ'ui")), mapping = aes(x=sample, y=pitch, color=label), alpha=0.1)+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("LM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("LM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  annotate("text", x=12, y=124, label="cíé")+
  annotate("text", x=30, y=124, label="kwà")+
  annotate("text", x=51, y=124, label="{exemplar}")+
  annotate("text", x=70, y=124, label="ka")+
  annotate("text", x=87, y=124, label="qχ'úī")+
  annotate("text", x=12, y=119, label="1SS")+
  annotate("text", x=30, y=119, label="PROG")+
  annotate("text", x=70, y=119, label="POST")+
  annotate("text", x=87, y=119, label="say")+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_cartesian(ylim=c(120, 200))+
  guides(color= guide_legend(title="", order=1), linetype=guide_legend(title="Other Categories"))+
  ggtitle("LM Category in Carrier Phrase")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values="red", limits=c("LM"))

#Figure 24 
ggplot(df.dHM, mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("dHM Category, Average Raw F0 (Isolation)")

#Figure 25
ggplot(dfp.dHM, mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean_p, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean_p, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("dHM Category, Average Raw F0 (Phrasal)")

#Figure 26 - Isolation
dHM.iso <- ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)+3, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(df, df$tone %in% c("HH","HM", "HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("dHM Category (All Depressors, Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

dHM.iso.model   <- gam(pitch ~ s(sample, bs = "cs"), data = df.dHM)

dHM.iso + 
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "change")$peaksX, y = id(df, dHM.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "change")$valleysX, y = id(df, dHM.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, dHM.iso.model, "change")$peaksX+5, y = (id(df, dHM.iso.model, "change")$peaksY)+3.5, label = paste(sprintf("%+00.2f", (round(id(df, dHM.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, dHM.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, dHM.iso.model, "change")$valleysX-6, y = (id(df, dHM.iso.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(df, dHM.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, dHM.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "maxX"), y = id(df, dHM.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "minX"), y = id(df, dHM.iso.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(df, dHM.iso.model), "Hz/T", sep=""), size=3)

#Figure 26 - Phrase  
dHM.p <- ggplot()+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)+3, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(dfp, dfp$tone %in% c("HH","HM", "HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("dHM Category (All Depressors, Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

dHM.p.model   <- gam(pitch ~ s(sample, bs = "cs"), data = dfp.dHM)

dHM.p + 
  geom_point(mapping = aes(x = id(dfp, dHM.p.model, "change")$peaksX, y = id(dfp, dHM.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(dfp, dHM.p.model, "change")$valleysX, y = id(dfp, dHM.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(dfp, dHM.p.model, "change")$peaksX, y = (id(dfp, dHM.p.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(dfp, dHM.p.model, "change")$peaksY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, dHM.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(dfp, dHM.p.model, "change")$valleysX+5, y = (id(dfp, dHM.p.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(dfp, dHM.p.model, "change")$valleysY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, dHM.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(dfp, dHM.p.model, "maxX"), y = id(dfp, dHM.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(dfp, dHM.p.model, "minX"), y = id(dfp, dHM.p.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", sprintf("%+00.2f", vmax(dfp, dHM.p.model)), "Hz/T", sep=""), size=3)

#Figure 35 - dHM
ggplot()+
  geom_hline(yintercept = mean(dfs$pitch, na.rm=T), linetype=2)+
  annotate("text", x=7, y=mean(dfs$pitch, na.rm=T)+3, label=paste("Mean: ", round(mean(dfs$pitch, na.rm=T), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LL","MM", "LM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LL","MM", "LM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = !(dfs.dHM$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs.dHM, subset = dfs.dHM$label %in% c("ka")), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs.dHL, subset = !(dfs.dHL$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset =dfs.dHL$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = dfs.dHM$label %in% c("cie", "kwa", "qχ'ui")), mapping = aes(x=sample, y=pitch, color=label), alpha=0.1)+
  annotate("text", x=12, y=124, label="cíé")+
  annotate("text", x=30, y=124, label="kwà")+
  annotate("text", x=51, y=124, label="{exemplar}")+
  annotate("text", x=70, y=124, label="ka")+
  annotate("text", x=87, y=124, label="qχ'úī")+
  annotate("text", x=12, y=119, label="1SS")+
  annotate("text", x=30, y=119, label="PROG")+
  annotate("text", x=70, y=119, label="POST")+
  annotate("text", x=87, y=119, label="say")+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_cartesian(ylim=c(120, 200))+
  guides(color= guide_legend(title="", order=1), linetype=guide_legend(title="Other Categories"))+
  ggtitle("dHM Category in Carrier Phrase")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values="red", limits=c("dHM"))

#Figure 27 - Isolation 
ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)+3, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(df, df$tone %in% c("HH","HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(df, df$tone=="HM"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+  
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "change")$peaksX, y = id(df, dHM.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "change")$valleysX, y = id(df, dHM.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, dHM.iso.model, "change")$peaksX+5, y = (id(df, dHM.iso.model, "change")$peaksY)+3, label = paste(round(id(df, dHM.iso.model, "change")$peaksY, digits=2), "Hz (T", id(df, dHM.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, dHM.iso.model, "change")$valleysX-6, y = (id(df, dHM.iso.model, "change")$valleysY)-5, label = paste(round(id(df, dHM.iso.model, "change")$valleysY, digits=2), "Hz (T", id(df, dHM.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "maxX"), y = id(df, dHM.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "minX"), y = id(df, dHM.iso.model, "minY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HM.iso.model, "change")$peaksX, y = id(df, HM.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, HM.iso.model, "change")$valleysX, y = id(df, HM.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, HM.iso.model, "change")$peaksX+6, y = (id(df, HM.iso.model, "change")$peaksY)+5, label = paste(round(id(df, HM.iso.model, "change")$peaksY, digits=2), "Hz (T", id(df, HM.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, HM.iso.model, "change")$valleysX-6, y = (id(df, HM.iso.model, "change")$valleysY)+5, label = paste(round(id(df, HM.iso.model, "change")$valleysY, digits=2), "Hz (T", id(df, HM.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, HM.iso.model, "maxX"), y = id(df, HM.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HM.iso.model, "minX"), y = id(df, HM.iso.model, "minY")), color="red", shape=21, size=3)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HM vs. dHM Categories (Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend(title="", order=1), color= guide_legend(title="", order=1), linetype= guide_legend(title="Other Categories"))

#Figure 27 - Phrase
ggplot()+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)-2, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(dfp, dfp$tone %in% c("HH","HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(dfp, dfp$tone=="HM"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+  
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+
  geom_point(mapping = aes(x = id(df, dHM.p.model, "change")$peaksX, y = id(df, dHM.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, dHM.p.model, "change")$valleysX, y = id(df, dHM.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, dHM.p.model, "change")$peaksX, y = (id(df, dHM.p.model, "change")$peaksY)+5, label = paste(round(id(df, dHM.p.model, "change")$peaksY, digits=2), "Hz (T", id(df, dHM.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, dHM.p.model, "change")$valleysX+5, y = (id(df, dHM.p.model, "change")$valleysY)-5, label = paste(round(id(df, dHM.p.model, "change")$valleysY, digits=2), "Hz (T", id(df, dHM.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, dHM.p.model, "maxX"), y = id(df, dHM.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, dHM.p.model, "minX"), y = id(df, dHM.p.model, "minY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HM.p.model, "change")$peaksX, y = id(df, HM.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, HM.p.model, "change")$valleysX, y = id(df, HM.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, HM.p.model, "change")$peaksX+6, y = (id(df, HM.p.model, "change")$peaksY)+5, label = paste(round(id(df, HM.p.model, "change")$peaksY, digits=2), "Hz (T", id(df, HM.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, HM.p.model, "change")$valleysX-6, y = (id(df, HM.p.model, "change")$valleysY)+5, label = paste(round(id(df, HM.p.model, "change")$valleysY, digits=2), "Hz (T", id(df, HM.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, HM.p.model, "maxX"), y = id(df, HM.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HM.p.model, "minX"), y = id(df, HM.p.model, "minY")), color="red", shape=21, size=3)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HM vs. dHM Categories (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend(title="", order=1), color= guide_legend(title="", order=1), linetype= guide_legend(title="Other Categories"))

#Figure 28 
ggplot(df.dHL, mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("dHL Category, Average Raw F0 (Isolation)")

#Figure 29
ggplot(dfp.dHL, mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean_p, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean_p, mapping = aes(x = 20, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("dHL Category, Average Raw F0 (Phrasal)")

#Figure 30 - Isolation
dHL.iso <- ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)+3, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(df, df$tone %in% c("HH","HM", "HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("dHL Category (All Depressors, Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

dHL.iso.model   <- gam(pitch ~ s(sample, bs = "cs"), data = df.dHL)

dHL.iso + 
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "change")$peaksX, y = id(df, dHL.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "change")$valleysX, y = id(df, dHL.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, dHL.iso.model, "change")$peaksX+5, y = (id(df, dHL.iso.model, "change")$peaksY)+3, label = paste(sprintf("%+00.2f", (round(id(df, dHL.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, dHL.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, dHL.iso.model, "change")$valleysX-5, y = (id(df, dHL.iso.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(df, dHL.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, dHL.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "maxX"), y = id(df, dHL.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "minX"), y = id(df, dHL.iso.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(df, dHL.iso.model), "Hz/T", sep=""), size=3)

#Figure 30 - Phrase 
dHL.p <- ggplot()+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)+3, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  geom_smooth(subset(dfp, dfp$tone %in% c("HH","HM", "HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("dHL Category (All Depressors, Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

dHL.p.model   <- gam(pitch ~ s(sample, bs = "cs"), data = dfp.dHL)

dHL.p + 
  geom_point(mapping = aes(x = id(dfp, dHL.p.model, "change")$peaksX, y = id(dfp, dHL.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(dfp, dHL.p.model, "change")$valleysX, y = id(dfp, dHL.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(dfp, dHL.p.model, "change")$peaksX+3, y = (id(dfp, dHL.p.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(dfp, dHL.p.model, "change")$peaksY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, dHL.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(dfp, dHL.p.model, "change")$valleysX-1, y = (id(dfp, dHL.p.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(dfp, dHL.p.model, "change")$valleysY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, dHL.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(dfp, dHL.p.model, "maxX"), y = id(dfp, dHL.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(dfp, dHL.p.model, "minX"), y = id(dfp, dHL.p.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", vmax(dfp, dHL.p.model), "Hz/T", sep=""), size=3)+ 
  coord_cartesian(clip="off")


# Figure 31 - Isolation
ggplot()+
  geom_smooth(subset(df, df$tone %in% c("HH","HM","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(df, df$tone=="HL"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+  
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "change")$peaksX, y = id(df, dHL.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "change")$valleysX, y = id(df, dHL.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, dHL.iso.model, "change")$peaksX+5, y = (id(df, dHL.iso.model, "change")$peaksY)+6, label = paste(round(id(df, dHL.iso.model, "change")$peaksY, digits=2), "Hz (T", id(df, dHL.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, dHL.iso.model, "change")$valleysX-6, y = (id(df, dHL.iso.model, "change")$valleysY)+5, label = paste(round(id(df, dHL.iso.model, "change")$valleysY, digits=2), "Hz (T", id(df, dHL.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "maxX"), y = id(df, dHL.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "minX"), y = id(df, dHL.iso.model, "minY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HL.iso.model, "change")$peaksX, y = id(df, HL.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, HL.iso.model, "change")$valleysX, y = id(df, HL.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, HL.iso.model, "change")$peaksX+6, y = (id(df, HL.iso.model, "change")$peaksY)+5, label = paste(round(id(df, HL.iso.model, "change")$peaksY, digits=2), "Hz (T", id(df, HL.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, HL.iso.model, "change")$valleysX-6, y = (id(df, HL.iso.model, "change")$valleysY)-2, label = paste(round(id(df, HL.iso.model, "change")$valleysY, digits=2), "Hz (T", id(df, HL.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, HL.iso.model, "maxX"), y = id(df, HL.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HL.iso.model, "minX"), y = id(df, HL.iso.model, "minY")), color="red", shape=21, size=3)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HL vs. dHL Categories (Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend(title="", order=1), color= guide_legend(title="", order=1), linetype= guide_legend(title="Other Categories"))

#Figure 31 - Phrase 
ggplot()+
  geom_smooth(subset(dfp, dfp$tone %in% c("HH","HM","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(subset(dfp, dfp$tone=="HL"), mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+  
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.1)+
  geom_point(mapping = aes(x = id(df, dHL.p.model, "change")$peaksX, y = id(df, dHL.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, dHL.p.model, "change")$valleysX, y = id(df, dHL.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, dHL.p.model, "change")$peaksX, y = (id(df, dHL.p.model, "change")$peaksY)+5, label = paste(round(id(df, dHL.p.model, "change")$peaksY, digits=2), "Hz (T", id(df, dHL.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, dHL.p.model, "change")$valleysX-6, y = (id(df, dHL.p.model, "change")$valleysY)-5, label = paste(round(id(df, dHL.p.model, "change")$valleysY, digits=2), "Hz (T", id(df, dHL.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, dHL.p.model, "maxX"), y = id(df, dHL.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, dHL.p.model, "minX"), y = id(df, dHL.p.model, "minY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HL.p.model, "change")$peaksX, y = id(df, HL.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, HL.p.model, "change")$valleysX, y = id(df, HL.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, HL.p.model, "change")$peaksX+6, y = (id(df, HL.p.model, "change")$peaksY)+5, label = paste(round(id(df, HL.p.model, "change")$peaksY, digits=2), "Hz (T", id(df, HL.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, HL.p.model, "change")$valleysX-6, y = (id(df, HL.p.model, "change")$valleysY)+5, label = paste(round(id(df, HL.p.model, "change")$valleysY, digits=2), "Hz (T", id(df, HL.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, HL.p.model, "maxX"), y = id(df, HL.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, HL.p.model, "minX"), y = id(df, HL.p.model, "minY")), color="red", shape=21, size=3)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("HL vs. dHL Categories (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend(title="", order=1), color= guide_legend(title="", order=1), linetype= guide_legend(title="Other Categories"))

#Figure 35 - dHL 
ggplot()+
  geom_hline(yintercept = mean(dfs$pitch, na.rm=T), linetype=2)+
  annotate("text", x=7, y=mean(dfs$pitch, na.rm=T)+3, label=paste("Mean: ", round(mean(dfs$pitch, na.rm=T), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LL","MM", "LM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LL","MM", "LM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = !(dfs.dHM$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset =dfs.dHM$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset = !(dfs.dHL$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs.dHL, subset = dfs.dHL$label %in% c("ka")), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs.dHL, subset = dfs.dHL$label %in% c("cie", "kwa", "qχ'ui")), mapping = aes(x=sample, y=pitch, color=label), alpha=0.1)+
  annotate("text", x=12, y=124, label="cíé")+
  annotate("text", x=30, y=124, label="kwà")+
  annotate("text", x=51, y=124, label="{exemplar}")+
  annotate("text", x=70, y=124, label="ka")+
  annotate("text", x=87, y=124, label="qχ'úī")+
  annotate("text", x=12, y=119, label="1SS")+
  annotate("text", x=30, y=119, label="PROG")+
  annotate("text", x=70, y=119, label="POST")+
  annotate("text", x=87, y=119, label="say")+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_cartesian(ylim=c(120, 200))+
  guides(color= guide_legend(title="", order=1), linetype=guide_legend(title="Other Categories"))+
  ggtitle("dHL Category in Carrier Phrase")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values="red", limits=c("dHL"))

#Figure 32 
ggplot(df.dHH, mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean, mapping = aes(x = 80, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("dHH Category, Average Raw F0 (Isolation)")

#Figure 33 
ggplot(dfp.dHH, mapping=aes(x=sample, y=pitch))+
  stat_summary(aes(y = pitch,group=1), fun.y=mean, colour="red", geom="line",group=1)+
  geom_hline(data= speaker_mean_p, aes(yintercept = mean_val), linetype=2)+
  geom_text(data = speaker_mean_p, mapping = aes(x = 80, y = mean_val+(mean_val*0.08), label = paste("Speaker Mean: ", round(mean_val, digits=2), "Hz", sep="")), size=3)+
  facet_grid(factor(speaker, levels=c('MP','PM', 'RT','KP', 'MK', 'NS'))~., scales = "free_y")+  
  facetted_pos_scales(y = list(
    scale_y_continuous(limits = c(200, 110), breaks = c(200, 165, 135, 110)),
    scale_y_continuous(limits = c(260, 170), breaks = c(260, 230, 200, 170)),
    scale_y_continuous(limits = c(300, 190), breaks = c(300, 265, 225, 190)),
    scale_y_continuous(limits = c(130, 70), breaks = c(130, 110, 90, 70)),
    scale_y_continuous(limits = c(170, 90), breaks = c(170, 145, 115, 90)),
    scale_y_continuous(limits = c(130, 60), breaks = c(130, 110, 85, 60))
  ))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("dHH Category, Average Raw F0 (Phrasal)")

#Figure 34 - Isolation   
dHH.iso <- ggplot()+
  geom_smooth(subset(df, df$tone %in% c("HH","HM", "HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(df.dHH, mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 88, y = mean(df$pitch)+3, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("Marginal dHH Category (All Depressors, Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

dHH.iso.model   <- gam(pitch ~ s(sample, bs = "cs"), data = df.dHH)

dHH.iso+
  geom_point(mapping = aes(x = id(df, dHH.iso.model, "change")$peaksX, y = id(df, dHH.iso.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(df, dHH.iso.model, "change")$valleysX, y = id(df, dHH.iso.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(df, dHH.iso.model, "change")$peaksX+5, y = (id(df, dHH.iso.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(df, dHH.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, dHH.iso.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(df, dHH.iso.model, "change")$valleysX+5, y = (id(df, dHH.iso.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(df, dHH.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2))), "Hz (T", id(df, dHH.iso.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(df, dHH.iso.model, "maxX"), y = id(df, dHH.iso.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(df, dHH.iso.model, "minX"), y = id(df, dHH.iso.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", sprintf("%+00.2f", vmax(df, dHH.iso.model)), "Hz/T", sep=""), size=3)

#Figure 34 - Phrase 
dHH.p <- ggplot()+
  geom_smooth(subset(dfp, dfp$tone %in% c("HH","HM", "HL","MM","LL", "LM")), mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHM, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHL, mapping=aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(dfp.dHH, mapping=aes(x=sample, y=pitch, color=tone, fill=tone), alpha=0.2)+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 88, y = mean(dfp$pitch)+3, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("Marginal dHH Category (All Depressors, Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(120, 200))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(fill = guide_legend("", order=1), color= guide_legend("", order=1), linetype=guide_legend("Other Categories"))

dHH.p.model   <- gam(pitch ~ s(sample, bs = "cs"), data = dfp.dHH)

dHH.p + 
  geom_point(mapping = aes(x = id(dfp, dHH.p.model, "change")$peaksX, y = id(dfp, dHH.p.model, "change")$peaksY))+
  geom_point(mapping = aes(x = id(dfp, dHH.p.model, "change")$valleysX, y = id(dfp, dHH.p.model, "change")$valleysY))+
  geom_text(mapping = aes(x = id(dfp, dHH.p.model, "change")$peaksX, y = (id(dfp, dHH.p.model, "change")$peaksY)+5, label = paste(sprintf("%+00.2f", (round(id(dfp, dHH.p.model, "change")$peaksY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, dHH.p.model, "change")$peaksX, ")", sep="")), size=3)+
  geom_text(mapping = aes(x = id(dfp, dHH.p.model, "change")$valleysX-5, y = (id(dfp, dHH.p.model, "change")$valleysY)-5, label = paste(sprintf("%+00.2f", (round(id(dfp, dHH.p.model, "change")$valleysY, digits=2)-round(mean(dfp$pitch), digits=2))), "Hz (T", id(dfp, dHH.p.model, "change")$valleysX, ")", sep="")), size=3)+
  geom_point(mapping = aes(x = id(dfp, dHH.p.model, "maxX"), y = id(dfp, dHH.p.model, "maxY")), color="red", shape=21, size=3)+
  geom_point(mapping = aes(x = id(dfp, dHH.p.model, "minX"), y = id(dfp, dHH.p.model, "minY")), color="red", shape=21, size=3)+
  annotate("text", x = 12, y = 120, label=paste("Vmax = ", sprintf("%+00.2f", vmax(dfp, dHH.p.model)), "Hz/T", sep=""), size=3)

#Figure 35 - dHH
ggplot()+
    geom_hline(yintercept = mean(dfs$pitch, na.rm=T), linetype=2)+
  annotate("text", x=7, y=mean(dfs$pitch, na.rm=T)+3, label=paste("Mean: ", round(mean(dfs$pitch, na.rm=T), digits=2), "Hz", sep=""), size=3)+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$file %in% c("lie_down","wildebeest","heart") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LL","MM", "LM") & !(dfs$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs, subset = dfs$tone %in% c("HM","HL","LL","MM", "LM") & dfs$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = !(dfs.dHM$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHM, subset = dfs.dHM$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset = !(dfs.dHL$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHL, subset =dfs.dHL$label %in% c("ka")), mapping = aes(x=sample, y=pitch, linetype=tone), alpha=0.1, color=alpha("azure4", 0.5))+
  geom_smooth(data = subset(dfs.dHH, subset = dfs.dHH$file %in% c("graze", "make_fire") & dfs.dHH$label %in% c("cie", "kwa", "qχ'ui")), mapping = aes(x=sample, y=pitch, color=label), alpha=0.1)+
  geom_smooth(data = subset(dfs.dHH, subset = dfs.dHH$file %in% c("graze", "make_fire") & !(dfs.dHH$label %in% c("cie", "kwa", "ka", "qχ'ui"))), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  geom_smooth(data = subset(dfs.dHH, subset = dfs.dHH$file %in% c("graze", "make_fire") & dfs.dHH$label %in% c("ka")), mapping = aes(x=sample, y=pitch, color=tone), alpha=0.1, fill="red")+
  annotate("text", x=12, y=124, label="cíé")+
  annotate("text", x=30, y=124, label="kwà")+
  annotate("text", x=51, y=124, label="{exemplar}")+
  annotate("text", x=70, y=124, label="ka")+
  annotate("text", x=87, y=124, label="qχ'úī")+
  annotate("text", x=12, y=119, label="1SS")+
  annotate("text", x=30, y=119, label="PROG")+
  annotate("text", x=70, y=119, label="POST")+
  annotate("text", x=87, y=119, label="say")+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  coord_cartesian(ylim=c(120, 200))+
  guides(color= guide_legend(title="", order=1), linetype=guide_legend(title="Other Categories"))+
  ggtitle("Marginal dHH Category in Carrier Phrase")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_color_manual(values="red", limits=c("dHH"))

## Substudy 2 
## EMM Means of Duration
#subset only non-depressed Categories 
dfp.emm <- subset(dfp, !(dfp$tone %in% c("HH-v","HM-v","HL-v", "MM-v","LL-v","LM-v")) & !(dfp$file %in% c("say", "there", "tongue", "thorn","whisk","arrow_connection")) & dfp$sample == 100)
#unify tone label of depressed categories 
dfp.emm$tone[dfp.emm$tone %in% c("dHL-dh", "dHL-h", "dHL-v")] = "dHL"
dur.emm <- emmeans(gam(time ~ sample + tone, data = df.emm), specs = pairwise ~ tone)
dur.emm.df <- as.data.frame(dur.emm)
dur.emm.p <- emmeans(gam(time ~ sample + tone, data = dfp.emm), specs = pairwise ~ tone)
dur.emm.dfp <- as.data.frame(dur.emm.p)

#Figure 36 - Isolation 
ggplot(subset(dur.emm.df, !(dur.emm.df$tone == ".")), aes(x = reorder(tone, -emmean), y = emmean, ymin = lower.CL, ymax = upper.CL, color=tone))+
  geom_col(position = position_dodge(), alpha = .5)+
  geom_point(position = position_dodge(width = 0.9))+
  coord_flip()+
  geom_errorbar(position=position_dodge(width=0.9), width = 0.20)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(x = "Tone Category",
       y = "Duration (ms)")+
  guides(color= guide_legend(title="Category", order=1))+
  ggtitle("Duration by Tone Category in Isolation (Averaged)")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#Figure 36 - Phrase  
ggplot(subset(dur.emm.dfp, !(dur.emm.dfp$tone == ".")), aes(x = reorder(tone, -emmean), y = emmean, ymin = lower.CL, ymax = upper.CL, color=tone))+
  geom_col(position = position_dodge(), alpha = .5)+
  geom_point(position = position_dodge(width = 0.9))+
  coord_flip()+
  geom_errorbar(position=position_dodge(width=0.9), width = 0.20)+
  scale_y_continuous(limits=c(0,1), breaks = scales::pretty_breaks(n = 10))+
  labs(x = "Tone Category",
       y = "Duration (ms)")+
  guides(color= guide_legend(title="Category", order=1))+
  ggtitle("Duration by Tone Category in Phrase (Averaged)")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#Figure 39 - Isolation 
ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)-2, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  
  geom_point(mapping = aes(x = 1, y = at(HH.iso.model, 1), color="HH"))+
  annotate("text", x = 12, y = at(HH.iso.model, 1), label = paste(sprintf("%+00.2f", (round(at(HH.iso.model, 1), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#F8766D")+
  geom_point(mapping = aes(x = 100, y = at(HH.iso.model, 100), color="HH"))+
  annotate("text", x = 89, y = at(HH.iso.model, 100), label = paste(sprintf("%+00.2f", (round(at(HH.iso.model, 100), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#F8766D")+
  
  geom_point(mapping = aes(x = 1, y = at(HM.iso.model, 1), color="HM"))+
  annotate("text", x = 12, y = at(HM.iso.model, 1), label = paste(sprintf("%+00.2f", (round(at(HM.iso.model, 1), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#CD9600")+
  geom_point(mapping = aes(x = 100, y = at(HM.iso.model, 100), color="HM"))+
  annotate("text", x = 89, y = at(HM.iso.model, 100), label = paste(sprintf("%+00.2f", (round(at(HM.iso.model, 100), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#CD9600")+
  
  geom_point(mapping = aes(x = 1, y = at(HL.iso.model, 1), color="HL"))+
  annotate("text", x = 3, y = at(HL.iso.model, 1)+3, label = paste(sprintf("%+00.2f", (round(at(HL.iso.model, 1), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#7CAE00")+
  geom_point(mapping = aes(x = 100, y = at(HL.iso.model, 100), color="HL"))+
  annotate("text", x = 89, y = at(HL.iso.model, 100), label = paste(sprintf("%+00.2f", (round(at(HL.iso.model, 100), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#7CAE00")+
  
  geom_point(mapping = aes(x = 1, y = at(MM.iso.model, 1), color="MM"))+
  annotate("text", x = -9, y = at(MM.iso.model, 1)+1, label = paste(sprintf("%+00.2f", (round(at(MM.iso.model, 1), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#00BE67")+
  geom_point(mapping = aes(x = 100, y = at(MM.iso.model, 100), color="MM"))+
  annotate("text", x = 89, y = at(MM.iso.model, 100), label = paste(sprintf("%+00.2f", (round(at(MM.iso.model, 100), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#00BE67")+
  
  geom_point(mapping = aes(x = 1, y = at(LL.iso.model, 1), color="LL"))+
  annotate("text", x = 12, y = at(LL.iso.model, 1), label = paste(sprintf("%+00.2f", (round(at(LL.iso.model, 1), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#00BFC4")+
  geom_point(mapping = aes(x = 100, y = at(LL.iso.model, 100), color="LL"))+
  annotate("text", x = 89, y = at(LL.iso.model, 100)+1, label = paste(sprintf("%+00.2f", (round(at(LL.iso.model, 100), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#00BFC4")+
  
  geom_point(mapping = aes(x = 1, y = at(LM.iso.model, 1), color="LM"))+
  annotate("text", x = 12, y = at(LM.iso.model, 1), label = paste(sprintf("%+00.2f", (round(at(LM.iso.model, 1), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#00A9FF")+
  geom_point(mapping = aes(x = 100, y = at(LM.iso.model, 100), color="LM"))+
  annotate("text", x = 112, y = at(LM.iso.model, 100), label = paste(sprintf("%+00.2f", (round(at(LM.iso.model, 100), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#00A9FF")+
  
  geom_point(mapping = aes(x = 1, y = at(dHM.iso.model, 1), color="dHM"))+
  annotate("text", x = 12, y = at(dHM.iso.model, 1)-2, label = paste(sprintf("%+00.2f", (round(at(dHM.iso.model, 1), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#C77CFF")+
  geom_point(mapping = aes(x = 100, y = at(dHM.iso.model, 100), color="dHM"))+
  annotate("text", x = 89, y = at(dHM.iso.model, 100), label = paste(sprintf("%+00.2f", (round(at(dHM.iso.model, 100), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#C77CFF")+
  
  geom_point(mapping = aes(x = 1, y = at(dHL.iso.model, 1), color="dHL"))+
  annotate("text", x = 12, y = at(dHL.iso.model, 1), label = paste(sprintf("%+00.2f", (round(at(dHL.iso.model, 1), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#FF61CC")+
  geom_point(mapping = aes(x = 100, y = at(dHL.iso.model, 100), color="dHL"))+
  annotate("text", x = 112, y = at(dHL.iso.model, 100), label = paste(sprintf("%+00.2f", (round(at(dHL.iso.model, 100), digits=2)-round(mean(df$pitch), digits=2))), "Hz", sep=""), size=3, color="#FF61CC")+
  
  coord_cartesian(ylim=c(120, 200), xlim=c(1, 100), clip="off")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(color= guide_legend(title="Tone Category"))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("F0 Onsets and Offsets (Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5), legend.position="right", legend.justification.right = "top", legend.background = element_rect(fill=alpha("white", alpha=0)))+ 
  scale_x_continuous(breaks = c(1, 100), minor_breaks = NULL, n.breaks = 2)+
  scale_color_manual(values = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"), breaks=c("HH", "HM", "HL", "MM", "LL", "LM", "dHM", "dHL") )

#Figure 39 - Phrase 
ggplot()+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)-2, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+

  geom_point(mapping = aes(x = 1, y = at(HH.p.model, 1), color="HH"))+
  annotate("text", x = 5, y = at(HH.p.model, 1)+3, label = paste(sprintf("%+00.2f", (round(at(HH.p.model, 1), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#F8766D")+
  geom_point(mapping = aes(x = 100, y = at(HH.p.model, 100), color="HH"))+
  annotate("text", x = 89, y = at(HH.p.model, 100), label = paste(sprintf("%+00.2f", (round(at(HH.p.model, 100), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#F8766D")+
  
  geom_point(mapping = aes(x = 1, y = at(HM.p.model, 1), color="HM"))+
  annotate("text", x = 12, y = at(HM.p.model, 1)-2, label = paste(sprintf("%+00.2f", (round(at(HM.p.model, 1), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#CD9600")+
  geom_point(mapping = aes(x = 100, y = at(HM.p.model, 100), color="HM"))+
  annotate("text", x = 96, y = at(HM.p.model, 100)+4, label = paste(sprintf("%+00.2f", (round(at(HM.p.model, 100), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#CD9600")+
  
  geom_point(mapping = aes(x = 1, y = at(HL.p.model, 1), color="HL"))+
  annotate("text", x = 12, y = at(HL.p.model, 1)+1, label = paste(sprintf("%+00.2f", (round(at(HL.p.model, 1), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#7CAE00")+
  geom_point(mapping = aes(x = 100, y = at(HL.p.model, 100), color="HL"))+
  annotate("text", x = 89, y = at(HL.p.model, 100)-1, label = paste(sprintf("%+00.2f", (round(at(HL.p.model, 100), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#7CAE00")+
  
  geom_point(mapping = aes(x = 1, y = at(MM.p.model, 1), color="MM"))+
  annotate("text", x = -9, y = at(MM.p.model, 1)-2, label = paste(sprintf("%+00.2f", (round(at(MM.p.model, 1), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#00BE67")+
  geom_point(mapping = aes(x = 100, y = at(MM.p.model, 100), color="MM"))+
  annotate("text", x = 89, y = at(MM.p.model, 100), label = paste(sprintf("%+00.2f", (round(at(MM.p.model, 100), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#00BE67")+
  
  geom_point(mapping = aes(x = 1, y = at(LL.p.model, 1), color="LL"))+
  annotate("text", x = 12, y = at(LL.p.model, 1), label = paste(sprintf("%+00.2f", (round(at(LL.p.model, 1), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#00BFC4")+
  geom_point(mapping = aes(x = 100, y = at(LL.p.model, 100), color="LL"))+
  annotate("text", x = 89, y = at(LL.p.model, 100)+1, label = paste(sprintf("%+00.2f", (round(at(LL.p.model, 100), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#00BFC4")+
  
  geom_point(mapping = aes(x = 1, y = at(LM.p.model, 1), color="LM"))+
  annotate("text", x = 12, y = at(LM.p.model, 1), label = paste(sprintf("%+00.2f", (round(at(LM.p.model, 1), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#00A9FF")+
  geom_point(mapping = aes(x = 100, y = at(LM.p.model, 100), color="LM"))+
  annotate("text", x = 110, y = at(LM.p.model, 100), label = paste(sprintf("%+00.2f", (round(at(LM.p.model, 100), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#00A9FF")+
  
  geom_point(mapping = aes(x = 1, y = at(dHM.p.model, 1), color="dHM"))+
  annotate("text", x = 12, y = at(dHM.p.model, 1), label = paste(sprintf("%+00.2f", (round(at(dHM.p.model, 1), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#C77CFF")+
  geom_point(mapping = aes(x = 100, y = at(dHM.p.model, 100), color="dHM"))+
  annotate("text", x = 92, y = at(dHM.p.model, 100)-2, label = paste(sprintf("%+00.2f", (round(at(dHM.p.model, 100), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#C77CFF")+
  
  geom_point(mapping = aes(x = 1, y = at(dHL.p.model, 1), color="dHL"))+
  annotate("text", x = 12, y = at(dHL.p.model, 1), label = paste(sprintf("%+00.2f", (round(at(dHL.p.model, 1), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#FF61CC")+
  geom_point(mapping = aes(x = 100, y = at(dHL.p.model, 100), color="dHL"))+
  annotate("text", x = 110, y = at(dHL.p.model, 100)-2, label = paste(sprintf("%+00.2f", (round(at(dHL.p.model, 100), digits=2)-round(mean(dfp$pitch), digits=2))), "Hz", sep=""), size=3, color="#FF61CC")+
  
  coord_cartesian(ylim=c(120, 200), xlim=c(1, 100), clip="off")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(color= guide_legend(title="Tone Category"))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("F0 Onsets and Offsets (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5), legend.position="right", legend.justification.right = "top", legend.background = element_rect(fill=alpha("white", alpha=0)))+ 
  scale_x_continuous(breaks = c(1, 100), minor_breaks = NULL, n.breaks = 2)+
  scale_color_manual(values = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"), breaks=c("HH", "HM", "HL", "MM", "LL", "LM", "dHM", "dHL") )

#Figure 40 - Isolation  
ggplot()+
  geom_hline(yintercept = mean(df$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(df$pitch)-2, label = paste("Mean: ", round(mean(df$pitch), digits=2), "Hz", sep="")), size=3)+
  
  geom_point(mapping = aes(x = id(df, HH.iso.model, "change")$peaksX, y = id(df, HH.iso.model, "change")$peaksY, color="HH"))+
  geom_point(mapping = aes(x = id(df, HH.iso.model, "change")$peaksX, y = id(df, HH.iso.model, "change")$peaksY, color="HH"),  shape=21, size=4)+
  annotate("text", x = id(df, HH.iso.model, "change")$peaksX+11, y = id(df, HH.iso.model, "change")$peaksY+1, label = paste(sprintf("%+00.2f", round(id(df, HH.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, HH.iso.model, "change")$peaksX, ")", sep=""), size=3, color="#F8766D")+
  geom_point(mapping = aes(x = id(df, HH.iso.model, "change")$valleysX, y = id(df, HH.iso.model, "change")$valleysY, color="HH"))+
  annotate("text", x = id(df, HH.iso.model, "change")$valleysX+9, y = id(df, HH.iso.model, "change")$valleysY, label = paste(sprintf("%+00.2f", round(id(df, HH.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, HH.iso.model, "change")$valleysX, ")", sep=""), size=3, color="#F8766D")+
  
  geom_point(mapping = aes(x = id(df, HM.iso.model, "change")$peaksX, y = id(df, HM.iso.model, "change")$peaksY, color="HM"))+
  geom_point(mapping = aes(x = id(df, HM.iso.model, "change")$peaksX, y = id(df, HM.iso.model, "change")$peaksY, color="HM"), shape=21, size=4)+
  annotate("text", x = id(df, HM.iso.model, "change")$peaksX+11, y = id(df, HM.iso.model, "change")$peaksY-2, label = paste(sprintf("%+00.2f", round(id(df, HM.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, HM.iso.model, "change")$peaksX, ")", sep=""), size=3, color="#CD9600")+
  geom_point(mapping = aes(x = id(df, HM.iso.model, "change")$valleysX, y = id(df, HM.iso.model, "change")$valleysY, color="HM"))+
  annotate("text", x = id(df, HM.iso.model, "change")$valleysX-9, y = id(df, HM.iso.model, "change")$valleysY, label = paste(sprintf("%+00.2f", round(id(df, HM.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, HM.iso.model, "change")$valleysX, ")", sep=""), size=3, color="#CD9600")+
  
  geom_point(mapping = aes(x = id(df, HL.iso.model, "change")$peaksX, y = id(df, HL.iso.model, "change")$peaksY, color="HL"))+
  geom_point(mapping = aes(x = id(df, HL.iso.model, "change")$peaksX, y = id(df, HL.iso.model, "change")$peaksY, color="HL"), shape=21, size=4)+
  annotate("text", x = id(df, HL.iso.model, "change")$peaksX, y = id(df, HL.iso.model, "change")$peaksY+5, label = paste(sprintf("%+00.2f", round(id(df, HL.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, HL.iso.model, "change")$peaksX, ")", sep=""), size=3, color="#7CAE00")+
  geom_point(mapping = aes(x = id(df, HL.iso.model, "change")$valleysX, y = id(df, HL.iso.model, "change")$valleysY, color="HL"))+
  annotate("text", x = id(df, HL.iso.model, "change")$valleysX+9, y = id(df, HL.iso.model, "change")$valleysY, label = paste(sprintf("%+00.2f", round(id(df, HL.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, HL.iso.model, "change")$valleysX, ")", sep=""), size=3, color="#7CAE00")+
  
  geom_point(mapping = aes(x = id(df, MM.iso.model, "change")$peaksX, y = id(df, MM.iso.model, "change")$peaksY, color="MM"))+
  geom_point(mapping = aes(x = id(df, MM.iso.model, "change")$peaksX, y = id(df, MM.iso.model, "change")$peaksY, color="MM"), shape=21, size=4)+
  annotate("text", x = id(df, MM.iso.model, "change")$peaksX+11, y = id(df, MM.iso.model, "change")$peaksY, label = paste(sprintf("%+00.2f", round(id(df, MM.iso.model, "change")$peaksY, digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, MM.iso.model, "change")$peaksX, ")", sep=""), size=3, color="#00BE67")+
  geom_point(mapping = aes(x = id(df, MM.iso.model, "change")$valleysX, y = id(df, MM.iso.model, "change")$valleysY, color="MM"))+
  annotate("text", x = id(df, MM.iso.model, "change")$valleysX-9, y = id(df, MM.iso.model, "change")$valleysY, label = paste(sprintf("%+00.2f", round(id(df, MM.iso.model, "change")$valleysY, digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, MM.iso.model, "change")$valleysX, ")", sep=""), size=3, color="#00BE67")+
  
  geom_point(mapping = aes(x = id(df, LL.iso.model, "maxX"), y = id(df, LL.iso.model, "maxY"), color="LL"))+
  geom_point(mapping = aes(x = id(df, LL.iso.model, "maxX"), y = id(df, LL.iso.model, "maxY"), color="LL"), shape=21, size=4)+
  annotate("text", x = id(df, LL.iso.model, "maxX")+11, y = id(df, LL.iso.model, "maxY"), label = paste(sprintf("%+00.2f", round(id(df, LL.iso.model, "maxY"), digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, LL.iso.model, "maxX"), ")", sep=""), size=3, color="#00BFC4")+
  geom_point(mapping = aes(x = id(df, LL.iso.model, "minX"), y = id(df, LL.iso.model, "minY"), color="LL"))+
  annotate("text", x = id(df, LL.iso.model, "minX")-9, y = id(df, LL.iso.model, "minY")+2, label = paste(sprintf("%+00.2f", round(id(df, LL.iso.model, "minY"), digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, LL.iso.model, "minX"), ")", sep=""), size=3, color="#00BFC4")+
  
  geom_point(mapping = aes(x = id(df, LM.iso.model, "maxX"), y = id(df, LM.iso.model, "maxY"), color="LM"))+
  geom_point(mapping = aes(x = id(df, LM.iso.model, "maxX"), y = id(df, LM.iso.model, "maxY"), color="LM"), shape=21, size=4)+
  
  annotate("text", x = id(df, LM.iso.model, "maxX")+11, y = id(df, LM.iso.model, "maxY"), label = paste(sprintf("%+00.2f", round(id(df, LM.iso.model, "maxY"), digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, LM.iso.model, "maxX"), ")", sep=""), size=3, color="#00A9FF")+
  geom_point(mapping = aes(x = id(df, LM.iso.model, "minX"), y = id(df, LM.iso.model, "minY"), color="LM"))+
  annotate("text", x = id(df, LM.iso.model, "minX")-9, y = id(df, LM.iso.model, "minY"), label = paste(sprintf("%+00.2f", round(id(df, LM.iso.model, "minY"), digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, LM.iso.model, "minX"), ")", sep=""), size=3, color="#00A9FF")+
  
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "maxX"), y = id(df, dHM.iso.model, "maxY"), color="dHM"))+
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "maxX"), y = id(df, dHM.iso.model, "maxY"), color="dHM"), shape=21, size=4)+
  annotate("text", x = id(df, dHM.iso.model, "maxX")+9, y = id(df, dHM.iso.model, "maxY")+1, label = paste(sprintf("%+00.2f", round(id(df, dHM.iso.model, "maxY"), digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, dHM.iso.model, "maxX"), ")", sep=""), size=3, color="#C77CFF")+
  geom_point(mapping = aes(x = id(df, dHM.iso.model, "minX"), y = id(df, dHM.iso.model, "minY"), color="dHM"))+
  annotate("text", x = id(df, dHM.iso.model, "minX")-9, y = id(df, dHM.iso.model, "minY")+2, label = paste(sprintf("%+00.2f", round(id(df, dHM.iso.model, "minY"), digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, dHM.iso.model, "minX"), ")", sep=""), size=3, color="#C77CFF")+
  
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "maxX"), y = id(df, dHL.iso.model, "maxY"), color="dHL"))+
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "maxX"), y = id(df, dHL.iso.model, "maxY"), color="dHL"), shape=21, size=4)+
  annotate("text", x = id(df, dHL.iso.model, "maxX"), y = id(df, dHL.iso.model, "maxY")+4, label = paste(sprintf("%+00.2f", round(id(df, dHL.iso.model, "maxY"), digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, dHL.iso.model, "maxX"), ")", sep=""), size=3, color="#FF61CC")+
  geom_point(mapping = aes(x = id(df, dHL.iso.model, "minX"), y = id(df, dHL.iso.model, "minY"), color="dHL"))+
  annotate("text", x = id(df, dHL.iso.model, "minX")+9, y = id(df, dHL.iso.model, "minY")+2, label = paste(sprintf("%+00.2f", round(id(df, dHL.iso.model, "minY"), digits=2)-round(mean(df$pitch), digits=2)), "Hz (T", id(df, dHL.iso.model, "minX"), ")", sep=""), size=3, color="#FF61CC")+
  
  coord_cartesian(ylim=c(120, 200), xlim=c(1, 100), clip="off")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(color= guide_legend(title="Tone Category"))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("Timing Distributions of F0 Global Maxima and Minima (Isolation)")+ 
  theme(plot.title = element_text(hjust = 0.5), legend.position="right", legend.justification.right = "top", legend.background = element_rect(fill=alpha("white", alpha=0)))+ 
  scale_color_manual(values = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"), breaks=c("HH", "HM", "HL", "MM", "LL", "LM", "dHM", "dHL") )

#Figure 40 - Phrase  
ggplot()+
  geom_hline(yintercept = mean(dfp$pitch), linetype=2)+
  geom_text(mapping = aes(x = 7, y = mean(dfp$pitch)-2, label = paste("Mean: ", round(mean(dfp$pitch), digits=2), "Hz", sep="")), size=3)+
  
  geom_point(mapping = aes(x = id(dfp, HH.p.model, "maxX"), y = id(dfp, HH.p.model, "maxY"), color="HH"))+
  geom_point(mapping = aes(x = id(dfp, HH.p.model, "maxX"), y = id(dfp, HH.p.model, "maxY"), color="HH"),  shape=21, size=4)+
  annotate("text", x = id(dfp, HH.p.model, "maxX"), y = id(dfp, HH.p.model, "maxY")+4, label = paste(sprintf("%+00.2f", round(id(dfp, HH.p.model, "maxY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, HH.p.model, "maxX"), ")", sep=""), size=3, color="#F8766D")+
  geom_point(mapping = aes(x = id(dfp, HH.p.model, "minX"), y = id(dfp, HH.p.model, "minY"), color="HH"))+
  annotate("text", x = id(dfp, HH.p.model, "minX")-8, y = id(dfp, HH.p.model, "minY")-3, label = paste(sprintf("%+00.2f", round(id(dfp, HH.p.model, "minY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, HH.p.model, "minX"), ")", sep=""), size=3, color="#F8766D")+
  
  geom_point(mapping = aes(x = id(dfp, HM.p.model, "maxX"), y = id(dfp, HM.p.model, "maxY"), color="HM"))+
  geom_point(mapping = aes(x = id(dfp, HM.p.model, "maxX"), y = id(dfp, HM.p.model, "maxY"), color="HM"), shape=21, size=4)+
  annotate("text", x = id(dfp, HM.p.model, "maxX")+9, y = id(dfp, HM.p.model, "maxY")-1, label = paste(sprintf("%+00.2f", round(id(dfp, HM.p.model, "maxY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, HM.p.model, "maxX"), ")", sep=""), size=3, color="#CD9600")+
  geom_point(mapping = aes(x = id(dfp, HM.p.model, "minX"), y = id(dfp, HM.p.model, "minY"), color="HM"))+
  annotate("text", x = id(dfp, HM.p.model, "minX"), y = id(dfp, HM.p.model, "minY")+4, label = paste(sprintf("%+00.2f", round(id(dfp, HM.p.model, "minY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, HM.p.model, "minX"), ")", sep=""), size=3, color="#CD9600")+
  
  geom_point(mapping = aes(x = id(dfp, HL.p.model, "maxX"), y = id(dfp, HL.p.model, "maxY"), color="HL"))+
  geom_point(mapping = aes(x = id(dfp, HL.p.model, "maxX"), y = id(dfp, HL.p.model, "maxY"), color="HL"), shape=21, size=4)+
  annotate("text", x = id(dfp, HL.p.model, "maxX")-8, y = id(dfp, HL.p.model, "maxY")-2, label = paste(sprintf("%+00.2f", round(id(dfp, HL.p.model, "maxY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, HL.p.model, "maxX"), ")", sep=""), size=3, color="#7CAE00")+
  geom_point(mapping = aes(x = id(dfp, HL.p.model, "minX"), y = id(dfp, HL.p.model, "minY"), color="HL"))+
  annotate("text", x = id(dfp, HL.p.model, "minX")-10, y = id(dfp, HL.p.model, "minY")-1, label = paste(sprintf("%+00.2f", round(id(dfp, HL.p.model, "minY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, HL.p.model, "minX"), ")", sep=""), size=3, color="#7CAE00")+
  
  geom_point(mapping = aes(x = id(dfp, MM.p.model, "maxX"), y = id(dfp, MM.p.model, "maxY"), color="MM"))+
  geom_point(mapping = aes(x = id(dfp, MM.p.model, "maxX"), y = id(dfp, MM.p.model, "maxY"), color="MM"), shape=21, size=4)+
  annotate("text", x = id(dfp, MM.p.model, "maxX")+8, y = id(dfp, MM.p.model, "maxY")+3, label = paste(sprintf("%+00.2f", round(id(dfp, MM.p.model, "maxY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, MM.p.model, "maxX"), ")", sep=""), size=3, color="#00BE67")+
  geom_point(mapping = aes(x = id(dfp, MM.p.model, "minX"), y = id(dfp, MM.p.model, "minY"), color="MM"))+
  annotate("text", x = id(dfp, MM.p.model, "minX")-9, y = id(dfp, MM.p.model, "minY"), label = paste(sprintf("%+00.2f", round(id(dfp, MM.p.model, "minY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, MM.p.model, "minX"), ")", sep=""), size=3, color="#00BE67")+
  
  geom_point(mapping = aes(x = id(dfp, LL.p.model, "maxX"), y = id(dfp, LL.p.model, "maxY"), color="LL"))+
  geom_point(mapping = aes(x = id(dfp, LL.p.model, "maxX"), y = id(dfp, LL.p.model, "maxY"), color="LL"), shape=21, size=4)+
  annotate("text", x = id(dfp, LL.p.model, "maxX")+9, y = id(dfp, LL.p.model, "maxY")+1, label = paste(sprintf("%+00.2f", round(id(dfp, LL.p.model, "maxY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, LL.p.model, "maxX"), ")", sep=""), size=3, color="#00BFC4")+
  geom_point(mapping = aes(x = id(dfp, LL.p.model, "minX"), y = id(dfp, LL.p.model, "minY"), color="LL"))+
  annotate("text", x = id(dfp, LL.p.model, "minX")-8, y = id(dfp, LL.p.model, "minY")+2, label = paste(sprintf("%+00.2f", round(id(dfp, LL.p.model, "minY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, LL.p.model, "minX"), ")", sep=""), size=3, color="#00BFC4")+
  
  geom_point(mapping = aes(x = id(dfp, LM.p.model, "maxX"), y = id(dfp, LM.p.model, "maxY"), color="LM"))+
  geom_point(mapping = aes(x = id(dfp, LM.p.model, "maxX"), y = id(dfp, LM.p.model, "maxY"), color="LM"), shape=21, size=4)+
  
  annotate("text", x = id(dfp, LM.p.model, "maxX")+9, y = id(dfp, LM.p.model, "maxY")+1, label = paste(sprintf("%+00.2f", round(id(dfp, LM.p.model, "maxY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, LM.p.model, "maxX"), ")", sep=""), size=3, color="#00A9FF")+
  geom_point(mapping = aes(x = id(dfp, LM.p.model, "minX"), y = id(dfp, LM.p.model, "minY"), color="LM"))+
  annotate("text", x = id(dfp, LM.p.model, "minX")-8, y = id(dfp, LM.p.model, "minY")+3, label = paste(sprintf("%+00.2f", round(id(dfp, LM.p.model, "minY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, LM.p.model, "minX"), ")", sep=""), size=3, color="#00A9FF")+
  
  geom_point(mapping = aes(x = id(dfp, dHM.p.model, "maxX"), y = id(dfp, dHM.p.model, "maxY"), color="dHM"))+
  geom_point(mapping = aes(x = id(dfp, dHM.p.model, "maxX"), y = id(dfp, dHM.p.model, "maxY"), color="dHM"), shape=21, size=4)+
  annotate("text", x = id(dfp, dHM.p.model, "maxX")+9, y = id(dfp, dHM.p.model, "maxY"), label = paste(sprintf("%+00.2f", round(id(dfp, dHM.p.model, "maxY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, dHM.p.model, "maxX"), ")", sep=""), size=3, color="#C77CFF")+
  geom_point(mapping = aes(x = id(dfp, dHM.p.model, "minX"), y = id(dfp, dHM.p.model, "minY"), color="dHM"))+
  annotate("text", x = id(dfp, dHM.p.model, "minX")+9, y = id(dfp, dHM.p.model, "minY")-1, label = paste(sprintf("%+00.2f", round(id(dfp, dHM.p.model, "minY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, dHM.p.model, "minX"), ")", sep=""), size=3, color="#C77CFF")+
  
  geom_point(mapping = aes(x = id(dfp, dHL.p.model, "maxX"), y = id(dfp, dHL.p.model, "maxY"), color="dHL"))+
  geom_point(mapping = aes(x = id(dfp, dHL.p.model, "maxX"), y = id(dfp, dHL.p.model, "maxY"), color="dHL"), shape=21, size=4)+
  annotate("text", x = id(dfp, dHL.p.model, "maxX")-8, y = id(dfp, dHL.p.model, "maxY")-2, label = paste(sprintf("%+00.2f", round(id(dfp, dHL.p.model, "maxY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, dHL.p.model, "maxX"), ")", sep=""), size=3, color="#FF61CC")+
  geom_point(mapping = aes(x = id(dfp, dHL.p.model, "minX"), y = id(dfp, dHL.p.model, "minY"), color="dHL"))+
  annotate("text", x = id(dfp, dHL.p.model, "minX")+9, y = id(dfp, dHL.p.model, "minY")-3, label = paste(sprintf("%+00.2f", round(id(dfp, dHL.p.model, "minY"), digits=2)-round(mean(dfp$pitch), digits=2)), "Hz (T", id(dfp, dHL.p.model, "minX"), ")", sep=""), size=3, color="#FF61CC")+
  
  coord_cartesian(ylim=c(120, 200), xlim=c(1, 100), clip="off")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(color= guide_legend(title="Tone Category"))+
  xlab("Time (Normalized)")+
  ylab("F0 (Hz)")+
  ggtitle("Timing Distributions of F0 Global Maxima and Minima (Phrasal)")+ 
  theme(plot.title = element_text(hjust = 0.5), legend.position="right", legend.justification.right = "top", legend.background = element_rect(fill=alpha("white", alpha=0)))+ 
  scale_color_manual(values = c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC"), breaks=c("HH", "HM", "HL", "MM", "LL", "LM", "dHM", "dHL") )