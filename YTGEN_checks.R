#******************************************************************************
# Analysis for the report - Checks on YTGEN Yield Tables
# Margules Groome - ForestrySA
# December 2019
#******************************************************************************

library(RODBC)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

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


#### Read Data ####

dbhandle <- odbcDriverConnect('driver={SQL Server};server=LAPTOP-BRBA7G7Q;database=YTGEN;trusted_connection=true')
currTableSQL<-paste("SELECT * From LogDetail",sep="") # SQL query to push

l.det<-sqlQuery(dbhandle,currTableSQL)

currTableSQL<-paste("SELECT * From StandSummary",sep="") # SQL query to push
s.sum<-sqlQuery(dbhandle,currTableSQL)

currTableSQL<-paste("SELECT * From GradeSummary",sep="") # SQL query to push
g.sum<-sqlQuery(dbhandle,currTableSQL)



#### Plotting ####


s.sum%>% 
  separate(YieldRequestName, into = c('Event', 'Source'), sep = '_', remove = FALSE) %>%
  filter(!HarvestTypeCode == 'CROP', 
         StratumName == '#') %>%
  ggplot(aes(x=Age, y=TotalRecoverableVolume, colour = HarvestTypeCode)) +
  geom_point() +
  facet_grid(Event~Source)


# Number of populations in each dataset
s.sum%>% 
  separate(YieldRequestName, into = c('Event', 'Source'), sep = '_', remove = FALSE) %>%
  filter(!HarvestTypeCode == 'CROP', 
         StratumName == '#') %>% 
  group_by(Event) %>%
  summarise(count = n())

# Number of plots in each dataset
s.sum%>% 
  separate(YieldRequestName, into = c('Event', 'Source'), sep = '_', remove = FALSE) %>%
  filter(!HarvestTypeCode == 'CROP', 
         StratumName != '#') %>% 
  group_by(Event) %>%
  summarise(count = n())



# Bar plot of volumes 
bar.vol<-g.sum%>%
  separate(YieldRequestName, into = c('Event', 'Source'), sep = '_', remove = FALSE) %>%
  filter(!HarvestTypeCode == 'CROP', 
         StratumName == '#') %>%
  group_by(YieldRequestName, GradeName, Event, Source) %>%
  summarise(vol = mean(Volume)) %>%
  #mutate(percent = vol/sum(vol)) 
  ggplot(aes(x=reorder(GradeName, vol), y=vol, colour = GradeName, fill = GradeName)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme(legend.position = 'none')+
  facet_grid(Event~Source) +
  scale_fill_MagGroome()+
  scale_colour_MagGroome()+
  labs(y = 'Mean volume', x='Grade')
bar.vol

png(here('out', 'vol_bar.png'), h=25, w=20, units='cm', res=500 )
bar.vol
dev.off()



# Plot percentages per hectare

mean.vols<-g.sum%>%
  separate(YieldRequestName, into = c('Event', 'Source'), sep = '_', remove = FALSE) %>%
  filter(!HarvestTypeCode == 'CROP', 
         StratumName == '#')


vol.sums<-s.sum %>% separate(YieldRequestName, into = c('Event', 'Source'), sep = '_', remove = FALSE) %>%
  filter(!HarvestTypeCode == 'CROP', 
         StratumName == '#')
 
nn<-mean.vols %>% left_join(vol.sums, by=c('YieldRequestName', 'PopulationName', 
                                           'HarvestTypeCode', 'Source', 'Event'))


vol.props<-nn %>% mutate(relvol = Volume/TotalStemVolume) %>%
  group_by(YieldRequestName, GradeName, Event, Source) %>%
  summarise(relvol = mean(relvol, na.rm=TRUE)*100) %>%
  ggplot(aes(x=reorder(GradeName, relvol), y=relvol, colour = GradeName, fill = GradeName)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme(legend.position = 'none')+
  facet_grid(Event~Source) +
  scale_fill_MagGroome()+
  scale_colour_MagGroome()+
  labs(y = 'Percentage of TSV', x='Grade')

vol.props

png(here('out', 'vol_props.png'), h=25, w=20, units='cm', res=500 )
vol.props
dev.off()

# These figures show that there are some differences between Interpine and the new data


