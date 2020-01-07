#******************************************************************************
# Replicate the final step of the Access db data munging that provides the volume 
# diameter class breakdown as a proportion of the parent class volume,
# Margules Grroome 
# December 2019
#******************************************************************************

#### Set libraries ####

#library(RODBC)
library(tidyverse) 
library(purrrlyr)
library(here)
options(scipen=999)
library(xlsx)


#### Set Mag Groome Custom colour plotter ####
# Create a vector of colours from the Margules Groome stationary

MagGroome_colours <- c(
  `light green` =  "#a1d696",
  `mid green` = "#6c916c",
  `dark green` = "#1f3d1f",
  `blue` = "#25496e")


#' Function to extract Mag Groome colors as hex codes
#'
#' @param ... Character names of drsimonj_colors 
#'
MagGroome_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (MagGroome_colours)
  
  MagGroome_colours[cols]
}


MagGroome_cols() 


MagGroome_palettes <- list(
  `main`  = MagGroome_cols("light green", "mid green", "dark green", "blue"))


#' Return function to interpolate a Mag Groome color palette
#'
#' @param palette Character name of palette in MagGroome_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
MagGroome_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- MagGroome_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


# This can now be used to interpolate colour through the Mag Groome colours and this
# can then be used for custom ggplot scales.
MagGroome_pal('main') (10)

# Now create functions for fill and scolour separately using the Mag Groome colours

#' Color scale constructor for MagGroome colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_colour_MagGroome <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MagGroome_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("MagGroome_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for MagGroome colors
#'
#' @param palette Character name of palette in drsimonj_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_MagGroome <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- MagGroome_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("MagGroome_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



#### Functions ####

# Function for model dataframe
makeModelDF<-function(low.df, high.df)
{
  ratio.df<- inner_join(low.df, high.df, by=c("HarvestTypeCode", 'YieldRequestName', 'PopulationName')) %>%
    mutate(ymtv = ifelse(yield == 'CCC', MTVS/1, ifelse(yield == 'TTT', MTVSN/1,0)),
           p0 = ifelse(vt0>0,sp0/vt0,0),
           p1 = ifelse(vt1>0,sp1/vt1,0),
           p2 = ifelse(vt2>0,sp2/vt2,0),
           p3 = ifelse(vt3>0,sp3/vt3,0),
           p4 = ifelse(vt4>0,sp4/vt4,0),
           p5 = ifelse(vt5>0,sp5/vt5,0),
           p6 = ifelse(vt6>0,sp6/vt6,0),
           p7 = ifelse(vt7>0,sp7/vt7,0),
           p8 = ifelse(vt8>0,sp8/vt8,0),
           p9 = ifelse(vt9>0,sp9/vt9,0),
           p10 = ifelse(vt10>0,sp10/vt10,0),
           p11 = ifelse(vt11>0,sp11/vt11,0),
           p12 = ifelse(vt12>0,sp1/vt12,0),
           pt = ifelse(vtt>0,spt/vtt,0), 
           ymtv = ymtv/1000) %>% # This should be questioned with Jan.
    filter(ifelse(yield == 'CCC', MTVS/1,ifelse(yield == 'TTT', MTVSN/1,0))>0) %>%
    select(Low, High, HarvestTypeCode, YieldRequestName, PopulationName, PopArea.x, NOP, yield, STB,STA,
           ymtv, p0, p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,pt, vt0, vt1, vt2, vt3, vt4, vt5, vt6,
           vt7, vt8, vt9, vt10, vt11, vt12, vtt, sp0, sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8,
           sp9, sp10, sp11, sp12, spt) %>%
    rename(Area = PopArea.x,
           vhigh = vtt)
  return(ratio.df)
}

#### Read Data ####
# These tables come from the Access database using ForestrySA queries

#l.trv<-read.xlsx(here('data', 'LowTRV.xlsx'),1, stringsAsFactors=FALSE)
#h.tsv<-read.xlsx(here('data', 'HighTSV.xlsx'),1, stringsAsFactors=FALSE)
#h.trv<-read.xlsx(here('data', 'HighTRV.xlsx'),1, stringsAsFactors=FALSE)
#l.waste<-read.xlsx(here('data', 'LowWaste.xlsx'),1, stringsAsFactors=FALSE)
#l.nonlog<-read.xlsx(here('data', 'Lownonlog.xlsx'),1, stringsAsFactors=FALSE)
#l.log<-read.xlsx(here('data', 'Lowlog.xlsx'),1, stringsAsFactors=FALSE)
#h.nonlog<-read.xlsx(here('data', 'Highnonlog.xlsx'),1, stringsAsFactors=FALSE)
#h.log<-read.xlsx(here('data', 'Highlog.xlsx'),1, stringsAsFactors=FALSE)
#l.pres<-read.xlsx(here('data', 'LowPres.xlsx'),1, stringsAsFactors=FALSE)
#l.pulp<-read.xlsx(here('data', 'LowPulp.xlsx'),1, stringsAsFactors=FALSE)
#l.sawlog<-read.xlsx(here('data', 'LowSawlog.xlsx'),1, stringsAsFactors=FALSE)
#l.industrial<-read.xlsx(here('data', 'LowIndustrial.xlsx'),1, stringsAsFactors=FALSE)




#### Make Modelling Data frames ####

# Apply funtion to produce the modelling dataframe
p_TRV_TSV<-makeModelDF(low.df = l.trv, high.df = h.tsv)
p_Waste_TSV<-makeModelDF(low.df = l.waste, high.df = h.tsv)
p_nonlog_TRV<-makeModelDF(low.df = l.nonlog, high.df = h.trv)
p_log_TRV<-makeModelDF(low.df = l.log, high.df = h.trv)
p_pulp_nonlog<-makeModelDF(low.df = l.pulp, high.df = h.nonlog)
p_pres_nonlog<-makeModelDF(low.df = l.pres, high.df = h.nonlog)
p_sawlog_log<-makeModelDF(low.df = l.sawlog, high.df = h.log)
p_industrial_log<-makeModelDF(low.df = l.industrial, high.df = h.log)


p_TRV_TSV %>%
  ggplot(aes(x=ymtv, y=p5))+
  geom_point() +
  geom_smooth()



#### Graph Model Fitting Datasets ####
pl.w.tsv<-p_Waste_TSV %>% ungroup() %>%
  select(PopulationName, ymtv, yield, p0, sp0, vt0,
                       p1, sp1, vt1, p2, sp2, vt2,
                       p3, sp3, vt3, p4, sp4, vt4,
                       p5, sp5, vt5, p5, sp5, vt5,
                       p6, sp6, vt6, p7, sp7, vt7,
                       p8, sp8, vt8, p9, sp9, vt9,
                       p10, sp10, vt10, p11, sp11, vt11,
                       p12, sp12, vt12) %>% 
  gather(key = key, value = value, -PopulationName, -ymtv, -yield) %>%
  separate(key, 
           into = c("Source", "DC"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(DC = as.numeric(DC)) %>%
  filter(Source == 'vt' | value>0) %>%
  filter(Source == 'p') %>%
  filter(!PopulationName  %in% c('5518502', '5017001')) %>% # This removes the plot with the windthrown trees
  filter(DC != 9) %>%
  ggplot(aes(x=ymtv, value)) +
  geom_point(aes(colour=yield)) +
  geom_smooth(method = 'loess', span=0.7, se=FALSE, colour='grey') +
  scale_colour_MagGroome()+
  labs(y = 'Proportion of parent volume', x='MTV', title = 'Waste-TSV') +
  facet_wrap(.~DC) 

pl.w.tsv

#t<-p_Waste_TSV %>% select(YieldRequest, Population, p7, sp7, vt7) 

png(here('out', 'p_waste_tsv.png'), h=30, w=25, units='cm', res=500)
pl.w.tsv
dev.off()

p.trv.tsv<-p_TRV_TSV %>% ungroup() %>%
  select(PopulationName, ymtv, yield, p0, sp0, vt0,
                       p1, sp1, vt1, p2, sp2, vt2,
                       p3, sp3, vt3, p4, sp4, vt4,
                       p5, sp5, vt5, p5, sp5, vt5,
                       p6, sp6, vt6, p7, sp7, vt7,
                       p8, sp8, vt8, p9, sp9, vt9,
                       p10, sp10, vt10, p11, sp11, vt11,
                       p12, sp12, vt12) %>% 
  gather(key = key, value = value, -PopulationName, -ymtv, -yield) %>%
  separate(key, 
           into = c("Source", "DC"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(DC = as.numeric(DC)) %>%
  filter(Source == 'vt' | value>0) %>%
  filter(Source == 'p') %>%
  filter(DC!=12)%>%
  ggplot(aes(x=ymtv, value)) +
  geom_point(aes(colour=yield)) +
  geom_smooth(method = 'loess', span=0.5, se=FALSE, colour = 'grey') +
  scale_colour_MagGroome()+
  labs(y = 'Proportion of parent volume', x='MTV', title = 'NonWaste-TSV') +
  facet_wrap(.~DC) 

p.trv.tsv

#t<-p_TRV_TSV %>% select(YieldRequest, Population, p12, sp12, vt12) 

png(here('out', 'p_nonwaste_tsv.png'), h=30, w=25, units='cm', res=500)
p.trv.tsv
dev.off()



  

p.nonlog.trv<-p_nonlog_TRV %>% ungroup() %>%
  select(PopulationName, ymtv, yield, p0, sp0, vt0,
                     p1, sp1, vt1, p2, sp2, vt2,
                     p3, sp3, vt3, p4, sp4, vt4,
                     p5, sp5, vt5, p5, sp5, vt5,
                     p6, sp6, vt6, p7, sp7, vt7,
                     p8, sp8, vt8, p9, sp9, vt9,
                     p10, sp10, vt10, p11, sp11, vt11,
                     p12, sp12, vt12) %>% 
  gather(key = key, value = value, -PopulationName, -ymtv, -yield) %>%
  separate(key, 
           into = c("Source", "DC"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(DC = as.numeric(DC)) %>%
  filter(Source == 'vt' | value>0) %>%
  filter(Source == 'p') %>%
  filter(DC!=12)%>%
  ggplot(aes(x=ymtv, value)) +
  geom_point(aes(colour=yield)) +
  geom_smooth(method = 'loess', span=0.5, se=FALSE, colour = 'grey') +
  scale_colour_MagGroome()+
  labs(y = 'Proportion of parent volume', x='MTV', title = 'Nonlog-TRV') +
  facet_wrap(.~DC) 

p.nonlog.trv


png(here('out', 'p_nonlog_trv.png'), h=30, w=25, units='cm', res=500)
p.nonlog.trv
dev.off()



#t<-p_nonlog_TRV %>% select(YieldRequest, p0, vt0, sp0)



p.log.trv<-p_log_TRV %>% select(PopulationName, ymtv, yield, p0, sp0, vt0,
                        p1, sp1, vt1, p2, sp2, vt2,
                        p3, sp3, vt3, p4, sp4, vt4,
                        p5, sp5, vt5, p5, sp5, vt5,
                        p6, sp6, vt6, p7, sp7, vt7,
                        p8, sp8, vt8, p9, sp9, vt9,
                        p10, sp10, vt10, p11, sp11, vt11,
                        p12, sp12, vt12) %>% 
  gather(key = key, value = value, -PopulationName, -ymtv, -yield) %>%
  separate(key, 
           into = c("Source", "DC"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>% #separate the numbers and letters from the string
  mutate(DC = as.numeric(DC)) %>%
  filter(Source == 'vt' | value>0) %>%
  filter(Source == 'p') %>%
  ggplot(aes(x=ymtv, value)) +
  geom_point(aes(colour=yield)) +
  geom_smooth(method = 'loess', span=0.5, se=FALSE, colour='grey') +
  scale_colour_MagGroome()+
  labs(y = 'Proportion of parent volume', x='MTV', title = 'log-TRV') +
  facet_wrap(.~DC) 

p.log.trv

png(here('out', 'p_log_trv.png'), h=30, w=25, units='cm', res=500)
p.log.trv
dev.off()



p.pres.nonlog<-p_pres_nonlog %>% select(PopulationName, ymtv, yield, p0, sp0, vt0,
                        p1, sp1, vt1, p2, sp2, vt2,
                        p3, sp3, vt3, p4, sp4, vt4,
                        p5, sp5, vt5, p5, sp5, vt5,
                        p6, sp6, vt6, p7, sp7, vt7,
                        p8, sp8, vt8, p9, sp9, vt9,
                        p10, sp10, vt10, p11, sp11, vt11,
                        p12, sp12, vt12) %>% 
  gather(key = key, value = value, -PopulationName, -ymtv, -yield) %>%
  separate(key, 
           into = c("Source", "DC"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(DC = as.numeric(DC)) %>%
  filter(Source == 'vt' | value>0) %>%
  filter(Source == 'p') %>%
  filter(PopulationName != '6019006')%>%
  ggplot(aes(x=ymtv, value)) +
  geom_point(aes(colour=yield)) +
  geom_smooth(method = 'loess', span=0.7, se=FALSE, colour='grey') +
  scale_colour_MagGroome()+
  labs(y = 'Proportion of parent volume', x='MTV', title = 'Pres-nonlog') +
  facet_wrap(.~DC) 
p.pres.nonlog


png(here('out', 'p_pres_nonlog.png'), h=30, w=25, units='cm', res=500)
p.pres.nonlog
dev.off()


p.pulp.nonlog<-p_pulp_nonlog %>% select(PopulationName, ymtv, yield, p0, sp0, vt0,
                         p1, sp1, vt1, p2, sp2, vt2,
                         p3, sp3, vt3, p4, sp4, vt4,
                         p5, sp5, vt5, p5, sp5, vt5,
                         p6, sp6, vt6, p7, sp7, vt7,
                         p8, sp8, vt8, p9, sp9, vt9,
                         p10, sp10, vt10, p11, sp11, vt11,
                         p12, sp12, vt12) %>% 
  gather(key = key, value = value, -PopulationName, -ymtv, -yield) %>%
  separate(key, 
           into = c("Source", "DC"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(DC = as.numeric(DC)) %>%
  filter(Source == 'vt' | value>0) %>%
  filter(Source == 'p') %>%
  filter(!DC %in% c(11,12)) %>%
  ggplot(aes(x=ymtv, value)) +
  geom_point(aes(colour=yield)) +
  geom_smooth(method = 'loess', span=0.9, se=FALSE, colour='grey') +
  scale_colour_MagGroome()+
  labs(y = 'Proportion of parent volume', x='MTV', title = 'Pulp-nonlog') +
  facet_wrap(.~DC) 
p.pulp.nonlog

png(here('out', 'p_pulp_nonlog.png'), h=30, w=25, units='cm', res=500)
p.pulp.nonlog
dev.off()





p.sawlog.log<-p_sawlog_log%>% select(PopulationName, ymtv, yield, p0, sp0, vt0,
                         p1, sp1, vt1, p2, sp2, vt2,
                         p3, sp3, vt3, p4, sp4, vt4,
                         p5, sp5, vt5, p5, sp5, vt5,
                         p6, sp6, vt6, p7, sp7, vt7,
                         p8, sp8, vt8, p9, sp9, vt9,
                         p10, sp10, vt10, p11, sp11, vt11,
                         p12, sp12, vt12) %>% 
  gather(key = key, value = value, -PopulationName, -ymtv, -yield) %>%
  separate(key, 
           into = c("Source", "DC"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(DC = as.numeric(DC)) %>%
  filter(Source == 'vt' | value>0) %>%
  filter(Source == 'p') %>%
  ggplot(aes(x=ymtv, value)) +
  geom_point(aes(colour=yield)) +
  geom_smooth(method = 'loess', span=0.7, se=FALSE, colour='grey') +
  scale_colour_MagGroome()+
  labs(y = 'Proportion of parent volume', x='MTV', title = 'sawlog-log') +
  facet_wrap(.~DC) 
p.sawlog.log

png(here('out', 'p_sawlog_log.png'), h=30, w=25, units='cm', res=500)
p.sawlog.log
dev.off()



p.industrial.log<-p_industrial_log%>% select(PopulationName, ymtv, yield, p0, sp0, vt0,
                       p1, sp1, vt1, p2, sp2, vt2,
                       p3, sp3, vt3, p4, sp4, vt4,
                       p5, sp5, vt5, p5, sp5, vt5,
                       p6, sp6, vt6, p7, sp7, vt7,
                       p8, sp8, vt8, p9, sp9, vt9,
                       p10, sp10, vt10, p11, sp11, vt11,
                       p12, sp12, vt12) %>% 
  gather(key = key, value = value, -PopulationName, -ymtv, -yield) %>%
  separate(key, 
           into = c("Source", "DC"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  mutate(DC = as.numeric(DC)) %>%
  filter(Source == 'vt' | value>0) %>%
  filter(Source == 'p') %>%
  filter(!DC %in% c(10,11)) %>%
  filter(PopulationName != '6019603') %>%
  ggplot(aes(x=ymtv, value)) +
  geom_point(aes(colour=yield)) +
  scale_colour_MagGroome()+
  geom_smooth(method = 'loess', span=0.5, se=FALSE, colour = 'grey') +
  labs(y = 'Proportion of parent volume', x='MTV', title = 'Industrial-log') +
  facet_wrap(.~DC) 
p.industrial.log

png(here('out', 'industrial_log.png'), h=30, w=25, units='cm', res=500)
p.industrial.log
dev.off()


#### Fit Models ####


# This must be a result holder
mtv <-rep(NA,11)
p0 <-rep(NA,11)
p1 <-rep(NA,11)
p2 <-rep(NA,11)
p3 <-rep(NA,11)
p4 <-rep(NA,11)
p5 <-rep(NA,11)
p6 <-rep(NA,11)
p7 <-rep(NA,11)
p8 <-rep(NA,11)
p9 <-rep(NA,11)
p10 <-rep(NA,11)
p11 <-rep(NA,11)

# This opens a pdf device for the models to be plotted into.
pdf(here('out', 'ProdModels.pdf'))


# Waste model
d <- data.frame(mtv,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
d$mtv<-c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)



data<- as.data.frame(p_Waste_TSV)

# Looks like the proportions are still included in column 12 - 33 as specified below.
# This should be reviewed and rewritten so that the models are still implemented correctly if the inputs
# change
  
#i=0
for (i in 0:11) {
  sd<-data[data[,26+i]>0,]  # What is this referring to and why - I think this is a logic check on if the subject DC contains any volume.
  if (length(sd$ymtv)>0) {
    v<-try(scatter.smooth(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("Waste:DC ",i),family=c("gaussian"),degree=2, span=0.5))
    if(is.null(v)){
      #      write.csv(titl,file="out.csv",append=TRUE)
      m.df<-sd %>% select(p = 12+i, ymtv) # This will limit the model fir dataset to only the current dataset 
      #lo<-loess(sd[,12+i]~sd$ymtv,family=c("gaussian"),degree=2, span=0.5) #This was the original format and cause of the bug
      lo<-loess(p~ymtv, data=m.df, family=c("gaussian"),degree=2, span=0.5)
      p<-predict(lo,data.frame(ymtv=c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)),se=TRUE)
      #      write.csv(p[1],file="out.csv",append=TRUE)
      d[,i+2]<-p[1]
    }
    if(!is.null(v))plot(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("Waste: DC ",i))  
  }
  if (length(d[,i+2][is.na(d[,i+2])==TRUE])==length(d[,i+2])) d[,i+2]<-0 
  
  
}
titl
#write_csv(titl,here('out', 'out.csv'),append=FALSE)
d$Model<-'Waste'
write_csv(d,here('out', 'out.csv'),append=TRUE, col_names = TRUE)


# Test area
#lo.test<-loess(data[,12]~data$ymtv, family=c("gaussian"),degree=2, span=0.5)
#class(lo.test)

# p.test<-predict(lo.test, data.frame(ymtv=c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)))
# p.test[1]
# 
# scatter.smooth(data$p0~data$ymtv, family=c("gaussian"),degree=2, span=0.5)
# lo.lm<-lm(p0~ymtv, data=data)
# abline(lo.lm, col = 'red')
# 
# 
# p.lm.test<-predict(lo.lm,data.frame(ymtv=c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)))
# p.lm.test[1]
# 
# 
# colnames(data)

#**
# Now that I see the problem... how best to proceed. 
# Alternative could be to produce the formula on the fly.
# alternatively iteratively select from the data, rename the response and then use a standard formula
# I have implemented the third option... this should be put into a function
#**





# Non waste model
d <- data.frame(mtv,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
d$mtv<-c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)



data<- as.data.frame(p_TRV_TSV)

#i=1
for (i in 0:11) {
  sd<-data[data[,26+i]>0,]  # What is this referring to and why - I think this is a logic check on if the subject DC contains any volume.
  if (length(sd$ymtv)>0) {
    v<-try(scatter.smooth(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("TRV:DC ",i),family=c("gaussian"),degree=2, span=0.5))
    if(is.null(v)){
      #      write.csv(titl,file="out.csv",append=TRUE)
      m.df<-sd %>% select(p = 12+i, ymtv) # This will limit the model fir dataset to only the current dataset 
      #lo<-loess(sd[,12+i]~sd$ymtv,family=c("gaussian"),degree=2, span=0.5) #This was the original format and cause of the bug
      lo<-loess(p~ymtv, data=m.df, family=c("gaussian"),degree=2, span=0.5)
      p<-predict(lo,data.frame(ymtv=c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)),se=TRUE)
      #      write.csv(p[1],file="out.csv",append=TRUE)
      d[,i+2]<-p[1]
    }
    if(!is.null(v))plot(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("TRV: DC ",i))  
  }
  if (length(d[,i+2][is.na(d[,i+2])==TRUE])==length(d[,i+2])) d[,i+2]<-0 
  
  
}
titl
#write_csv(titl,here('out', 'out.csv'),append=FALSE)
d$Model<-'NonWaste'
write_csv(d,here('out', 'out.csv'),append=TRUE, col_names = FALSE)


# Non log model
d <- data.frame(mtv,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
d$mtv<-c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)


data<- as.data.frame(p_nonlog_TRV)

#i=1
for (i in 0:11) {
  sd<-data[data[,26+i]>0,]  # What is this referring to and why - I think this is a logic check on if the subject DC contains any volume.
  if (length(sd$ymtv)>0) {
    v<-try(scatter.smooth(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("non-log:DC ",i),family=c("gaussian"),degree=2, span=0.5))
    if(is.null(v)){
      #      write.csv(titl,file="out.csv",append=TRUE)
      m.df<-sd %>% select(p = 12+i, ymtv) # This will limit the model fir dataset to only the current dataset 
      #lo<-loess(sd[,12+i]~sd$ymtv,family=c("gaussian"),degree=2, span=0.5) #This was the original format and cause of the bug
      lo<-loess(p~ymtv, data=m.df, family=c("gaussian"),degree=2, span=0.5)
      p<-predict(lo,data.frame(ymtv=c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)),se=TRUE)
      #      write.csv(p[1],file="out.csv",append=TRUE)
      d[,i+2]<-p[1]
    }
    if(!is.null(v))plot(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("non-log: DC ",i))  
  }
  if (length(d[,i+2][is.na(d[,i+2])==TRUE])==length(d[,i+2])) d[,i+2]<-0 
  
  
}
titl
#write_csv(titl,here('out', 'out.csv'),append=FALSE)
d$Model<-'NonLog'
write_csv(d,here('out', 'out.csv'),append=TRUE, col_names = FALSE)


# Log model
d <- data.frame(mtv,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
d$mtv<-c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)


data<- as.data.frame(p_log_TRV)

#i=1
for (i in 0:11) {
  sd<-data[data[,26+i]>0,]  # What is this referring to and why - I think this is a logic check on if the subject DC contains any volume.
  if (length(sd$ymtv)>0) {
    v<-try(scatter.smooth(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("log:DC ",i),family=c("gaussian"),degree=2, span=0.5))
    if(is.null(v)){
      #      write.csv(titl,file="out.csv",append=TRUE)
      m.df<-sd %>% select(p = 12+i, ymtv) # This will limit the model fir dataset to only the current dataset 
      #lo<-loess(sd[,12+i]~sd$ymtv,family=c("gaussian"),degree=2, span=0.5) #This was the original format and cause of the bug
      lo<-loess(p~ymtv, data=m.df, family=c("gaussian"),degree=2, span=0.5)
      p<-predict(lo,data.frame(ymtv=c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)),se=TRUE)
      #      write.csv(p[1],file="out.csv",append=TRUE)
      d[,i+2]<-p[1]
    }
    if(!is.null(v))plot(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("log: DC ",i))  
  }
  if (length(d[,i+2][is.na(d[,i+2])==TRUE])==length(d[,i+2])) d[,i+2]<-0 
  
  
}
titl
#write_csv(titl,here('out', 'out.csv'),append=FALSE)
d$Model<-'Log'
write_csv(d,here('out', 'out.csv'),append=TRUE, col_names = FALSE)


# Pres model
d <- data.frame(mtv,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
d$mtv<-c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)


data<- as.data.frame(p_pres_nonlog)

#i=1
for (i in 0:11) {
  sd<-data[data[,26+i]>0,]  # What is this referring to and why - I think this is a logic check on if the subject DC contains any volume.
  if (length(sd$ymtv)>0) {
    v<-try(scatter.smooth(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("Prese:DC ",i),family=c("gaussian"),degree=2, span=0.5))
    if(is.null(v)){
      #      write.csv(titl,file="out.csv",append=TRUE)
      m.df<-sd %>% select(p = 12+i, ymtv) # This will limit the model fir dataset to only the current dataset 
      #lo<-loess(sd[,12+i]~sd$ymtv,family=c("gaussian"),degree=2, span=0.5) #This was the original format and cause of the bug
      lo<-loess(p~ymtv, data=m.df, family=c("gaussian"),degree=2, span=0.5)
      p<-predict(lo,data.frame(ymtv=c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)),se=TRUE)
      #      write.csv(p[1],file="out.csv",append=TRUE)
      d[,i+2]<-p[1]
    }
    if(!is.null(v))plot(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("Pres: DC ",i))  
  }
  if (length(d[,i+2][is.na(d[,i+2])==TRUE])==length(d[,i+2])) d[,i+2]<-0 
  
  
}
titl
#write_csv(titl,here('out', 'out.csv'),append=FALSE)
d$Model<-'Pres'
write_csv(d,here('out', 'out.csv'),append=TRUE, col_names = FALSE)


# Pulp model
d <- data.frame(mtv,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
d$mtv<-c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)


data<- as.data.frame(p_pulp_nonlog)

#i=1
for (i in 0:11) {
  sd<-data[data[,26+i]>0,]  # What is this referring to and why - I think this is a logic check on if the subject DC contains any volume.
  if (length(sd$ymtv)>0) {
    v<-try(scatter.smooth(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("Pulp:DC ",i),family=c("gaussian"),degree=2, span=0.5))
    if(is.null(v)){
      #      write.csv(titl,file="out.csv",append=TRUE)
      m.df<-sd %>% select(p = 12+i, ymtv) # This will limit the model fir dataset to only the current dataset 
      #lo<-loess(sd[,12+i]~sd$ymtv,family=c("gaussian"),degree=2, span=0.5) #This was the original format and cause of the bug
      lo<-loess(p~ymtv, data=m.df, family=c("gaussian"),degree=2, span=0.5)
      p<-predict(lo,data.frame(ymtv=c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)),se=TRUE)
      #      write.csv(p[1],file="out.csv",append=TRUE)
      d[,i+2]<-p[1]
    }
    if(!is.null(v))plot(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("Pulp: DC ",i))  
  }
  if (length(d[,i+2][is.na(d[,i+2])==TRUE])==length(d[,i+2])) d[,i+2]<-0 
  
  
}
titl
#write_csv(titl,here('out', 'out.csv'),append=FALSE)
d$Model<-'Pulp'
write_csv(d,here('out', 'out.csv'),append=TRUE, col_names = FALSE)

# sawlog model
d <- data.frame(mtv,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
d$mtv<-c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)


data<- as.data.frame(p_sawlog_log)

#i=1
for (i in 0:11) {
  sd<-data[data[,26+i]>0,]  # What is this referring to and why - I think this is a logic check on if the subject DC contains any volume.
  if (length(sd$ymtv)>0) {
    v<-try(scatter.smooth(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("Sawlog:DC ",i),family=c("gaussian"),degree=2, span=0.5))
    if(is.null(v)){
      #      write.csv(titl,file="out.csv",append=TRUE)
      m.df<-sd %>% select(p = 12+i, ymtv) # This will limit the model fir dataset to only the current dataset 
      #lo<-loess(sd[,12+i]~sd$ymtv,family=c("gaussian"),degree=2, span=0.5) #This was the original format and cause of the bug
      lo<-loess(p~ymtv, data=m.df, family=c("gaussian"),degree=2, span=0.5)
      p<-predict(lo,data.frame(ymtv=c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)),se=TRUE)
      #      write.csv(p[1],file="out.csv",append=TRUE)
      d[,i+2]<-p[1]
    }
    if(!is.null(v))plot(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("Sawlog: DC ",i))  
  }
  if (length(d[,i+2][is.na(d[,i+2])==TRUE])==length(d[,i+2])) d[,i+2]<-0 
  
  
}
titl
#write_csv(titl,here('out', 'out.csv'),append=FALSE)
d$Model<-'Sawlog'
write_csv(d,here('out', 'out.csv'),append=TRUE, col_names = FALSE)


# Industrial model
data<- as.data.frame(p_industrial_log)

d <- data.frame(mtv,p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
d$mtv<-c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)

#i=1
for (i in 0:11) {
  sd<-data[data[,26+i]>0,]  # What is this referring to and why - I think this is a logic check on if the subject DC contains any volume.
  if (length(sd$ymtv)>0) {
    v<-try(scatter.smooth(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("ISW:DC ",i),family=c("gaussian"),degree=2, span=0.5))
    if(is.null(v)){
      #      write.csv(titl,file="out.csv",append=TRUE)
      m.df<-sd %>% select(p = 12+i, ymtv) # This will limit the model fir dataset to only the current dataset 
      #lo<-loess(sd[,12+i]~sd$ymtv,family=c("gaussian"),degree=2, span=0.5) #This was the original format and cause of the bug
      lo<-loess(p~ymtv, data=m.df, family=c("gaussian"),degree=2, span=0.5)
      p<-predict(lo,data.frame(ymtv=c(0.25,0.5,0.75,1,1.5,2,2.5,3,3.5,4,5)),se=TRUE)
      #      write.csv(p[1],file="out.csv",append=TRUE)
      d[,i+2]<-p[1]
    }
    if(!is.null(v))plot(sd[,12+i]~sd$ymtv,xaxp=c(0,5,10),yaxp=c(0,1,10),main=titl<-paste("ISW: DC ",i))  
  }
  if (length(d[,i+2][is.na(d[,i+2])==TRUE])==length(d[,i+2])) d[,i+2]<-0 
  
  
}
titl
#write_csv(titl,here('out', 'out.csv'),append=FALSE)
d$Model<-'Industrial'
write_csv(d,here('out', 'out.csv'),append=TRUE, col_names = FALSE)



dev.off()



