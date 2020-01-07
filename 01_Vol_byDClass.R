#******************************************************************************
# This script provides functions to replicate the log product matrix originally
# developed by Jan Rombouts.
# The script imports data from a SQL database containing the database table
# output from YTGEN, Care should be taken to ensure that the YTGEN Yield Request
# is set up correctly. Yield requests that include thinnings should be named
# so that the yield request name begins with a T. The clearfell yield requests 
# Should begin with a C. An additional table R2CLI also needs to be populated in
# the SQL database. This contains 'in house' data including the next operation 
# (NOP) and the stand densities before and after the operation.
# Margules Grroome 
# November 2019
#******************************************************************************


#### Set Libraries ####
library(RODBC)
library(tidyverse) # The tidyverse is going to be used for applying functions
library(purrrlyr)
options(scipen=999)
library(devtools)
#library(roxygen2)
library(here)
library(janitor)


#### User inputs and globals ####

# In the original query there is a user input for whether the data should be weighted or not

# Do you want to use area weightings?
wt<- 'y'



# Think about adding extra here for generality... I think this is going to have to be used again.

# Enter server name or server and database strings?

#### Read data from SQL database ####
# Connect to SQL database




dbhandle <- odbcDriverConnect('driver={SQL Server};server=LAPTOP-BRBA7G7Q;database=YTGEN;trusted_connection=true')
currTableSQL<-paste("SELECT * From LogDetail",sep="") # SQL query to push


LogDetail<-sqlQuery(dbhandle,currTableSQL, stringsAsFactors=FALSE)
o.df<-LogDetail
o.df$StratumName<-as.character(o.df$StratumName)
o.df$PlotName<-as.character(o.df$PlotName)

currTableSQL<-paste("SELECT * From StandSummary",sep="") # SQL query to push


ss<-sqlQuery(dbhandle,currTableSQL, stringsAsFactors=FALSE)


currTableSQL<-paste("SELECT * From R2LCI",sep="") # SQL query to push

r2lci<-sqlQuery(dbhandle,currTableSQL, stringsAsFactors=FALSE) %>%
  rename(PopulationName = KEY)
r2lci$PopulationName<-as.character(r2lci$PopulationName)




#### Custom Functions ####

#' Calculate cylindrical volume from mid point stem diameter
#' 
#' @param length Log length
#' @param SED Log small end diameter
#' @param LED Log large end diameter
#' @return The log cylindrical volume based on the mid stem diameter
#' @examples cylVol(3.9, 32.2, 42.2)


cylVol<-function(length, SED, LED)
{
   out<-3.1416/80000*length*(SED^2+LED^2)
   return(out)

}



# Calculate rv
# ratio of volume to cylinder

calRV<-function(weight, TreeFreq, vsl, stratArea, Volume)
{
  rv<- ifelse(weight=='y',
              ifelse(TreeFreq>0,
                     vsl/TreeFreq/stratArea/Volume,0),
              vsl/Volume) 

return(rv)
}


calRV(weight = wt, TreeFreq =0, vsl = 0.0767022914, stratArea = 1.2, Volume = 0.07610665)


# Function to calculate rV20 - What is rv20?
calRV20<-function(weight, TreeFreq, stratArea)
{
  
  rv20<-ifelse(weight=='y',
               ifelse(TreeFreq>0,
                      1/TreeFreq/stratArea,0),
               1)
  
  return(rv20)
}



# Function to calculate vS
# half log volume based on SED

calvS<-function(TreeFreq, length, sed, rv)
{
  vS<- ifelse(TreeFreq>0,
              3.1416/80000*length*sed^2/rv,
              0) 
  return(vS)
}

# function to calculate vL
# half log volume based on LED

calvL<-function(TreeFreq, length, led, rv)
{
  vL<- ifelse(TreeFreq>0,
              3.1416/80000*length*led^2/rv,
              0) 

  return(vL)

}


# Function to calculate iS
# "index small" being diamater class based on SED


calciS<-function(sed)
{
  iS<- ifelse(sed<6.999,
              -1,
              ifelse(sed<12 & sed>=7, 
                     0,
                     ifelse(sed>=12 & sed<15,
                            1,
                            floor(sed/5)-1)))

  return(iS)
}


# Function to calculate iL
# "index large" being diamater class based on LED

calciL<-function(led)
{
iL<-ifelse(led<7.01,
       -1,
       ifelse(led<12 & led>7.001,
              0,ifelse(led >=12 & led<15,
                       1,
                       floor(led/5)-1))) 
return(iL)
}






# Assign vol class and volume

AssignV0<-function(iS,iL, vS, vL)
{

v0<-ifelse((iS==0 & iL ==0),
           vS+vL,
           ifelse(iS==0,
                  vS,
                  ifelse(iL==0,
                         vL,
                         0)))

return(v0)
}

AssignV1<-function(iS,iL, vS, vL)
{
v1<-ifelse((iS==1 & iL ==1),
           vS+vL,
           ifelse(iS==1,
                  vS,
                  ifelse(iL==1,
                         vL,
                         0)))

return(v1)
}

AssignV2<-function(iS,iL, vS, vL)
{
v2<-ifelse((iS==2 & iL ==2),
           vS+vL,
           ifelse(iS==2,
                  vS,
                  ifelse(iL==2,
                         vL,
                         0)))
return(v2)
}


AssignV3<-function(iS,iL, vS, vL)
{
v3<-ifelse((iS==3 & iL ==3),
           vS+vL,
           ifelse(iS==3,
                  vS,
                  ifelse(iL==3,
                         vL,
                         0)))
return(v3)
}

AssignV4<-function(iS,iL, vS, vL)
{
v4<-ifelse((iS==4 & iL ==4),
           vS+vL,
           ifelse(iS==4,
                  vS,
                  ifelse(iL==4,
                         vL,
                         0)))
return(v4)
}


AssignV5<-function(iS,iL, vS, vL)
{
v5<-ifelse((iS==5 & iL ==5),
           vS+vL,
           ifelse(iS==5,
                  vS,
                  ifelse(iL==5,
                         vL,
                         0)))
return(v5)
}

AssignV6<-function(iS,iL, vS, vL)
{
v6<-ifelse((iS==6 & iL ==6),
           vS+vL,
           ifelse(iS==6,
                  vS,
                  ifelse(iL==6,
                         vL,
                         0)))
return(v6)
}

AssignV7<-function(iS,iL, vS, vL)
{

v7<-ifelse((iS==7 & iL ==7),
           vS+vL,
           ifelse(iS==7,
                  vS,
                  ifelse(iL==7,
                         vL,
                         0)))
return(v7)
}

AssignV8<-function(iS,iL, vS, vL)
{
v8<-ifelse((iS==8 & iL ==8),
           vS+vL,
           ifelse(iS==8,
                  vS,
                  ifelse(iL==8,
                         vL,
                         0)))

return(v8)
}

AssignV9<-function(iS,iL, vS, vL)
{
v9<-ifelse((iS==9 & iL ==9),
           vS+vL,
           ifelse(iS==9,
                  vS,
                  ifelse(iL==9,
                         vL,
                         0)))

return(v9)
}

AssignV10<-function(iS,iL, vS, vL)
{
v10<-ifelse((iS==10 & iL ==10),
           vS+vL,
           ifelse(iS==10,
                  vS,
                  ifelse(iL==10,
                         vL,
                         0)))

return(v10)
}

AssignV11<-function(iS,iL, vS, vL)
{
v11<-ifelse((iS==11 & iL ==11),
            vS+vL,
            ifelse(iS==11,
                   vS,
                   ifelse(iL==11,
                          vL,
                          0)))

return(v11)
}

AssignV12<-function(iS,iL, vS, vL)
{
v12<-ifelse((iS==12 & iL ==12),
            vS+vL,
            ifelse(iS==12,
                   vS,
                   ifelse(iL==12,
                          vL,
                          0)))

return(v12)
}





# Calcuate Taper

CalcTpr<-function(LED, SED, length)
{
  
  tpr<-(LED^2-SED^2)/length  
  
  return(tpr)

}


 

# Assign length by diameter class

#  Length of log by diameter classes 20, 30, 40
AssignLength15<-function(TreeFreq, SED, LED, Length)
{
  
  l15<-ifelse(TreeFreq>0,
              ifelse(SED<15 & LED>15,
                     Length*(LED^2-15^2)/(LED^2-SED^2),
                     ifelse(SED>=15,Length,0)),
              0) 
  return(l15)
}

AssignLength20<-function(TreeFreq, SED, LED, Length)
{ 
  l20<-ifelse(TreeFreq>0,
        ifelse(SED<20 & LED>20,
                Length*(LED^2-20^2)/(LED^2-SED^2),
                ifelse(SED>=20,Length,0)),
        0) 
  return(l20)
}



AssignLength30<-function(TreeFreq, SED, LED, Length)
{
  l30<-ifelse(TreeFreq>0,
              ifelse(SED<30 & LED>30,
                     Length*(LED^2-30^2)/(LED^2-SED^2),
                     ifelse(SED>=30, 
                            Length,0)),
              0)
  return(l30)
}

AssignLength40<-function(TreeFreq, SED, LED, Length)
{  
  l40<-ifelse(TreeFreq>0,
              ifelse(SED<40 & LED>40,
                     Length*(LED^2-40^2)/(LED^2-SED^2),
                     ifelse(SED>=40, 
                            Length,0)),
              0)
  return(l40)
}




#Volume of log with SED > 15cm
AssignVolSED15 <- function(TreeFreq, SED, LED, taper, Volume, rv20, l15)
{

v15<-ifelse(TreeFreq>0,
            ifelse(SED<15 & LED>15,
                   3.1416/40000*l15*(15^2+l15/2*taper),
                   ifelse(SED>=15,
                          Volume,0))/rv20,0)
return(v15)
}



#Volume of log with SED > 20cm
AssignVolSED20 <- function(TreeFreq, SED, LED, taper, Volume, rv20, l20)
{
v20<-ifelse(TreeFreq>0,
       ifelse(SED<20 & LED>20,
              3.1416/40000*l20*(20^2+l20/2*taper),
              ifelse(SED>=20,
                     Volume,0))/rv20,0) 
return(v20)
}

#Volume of log with SED > 30cm
AssignVolSED30 <- function(TreeFreq, SED, LED, taper, Volume, rv20, l30)
{
v30<-ifelse(TreeFreq>0,
            ifelse(SED<30 & LED>30,3.1416/40000*l30*(30^2+l30/2*taper),
                   ifelse(SED>=30,
                          Volume,0))/rv20,
            0)  
return(v30)
}

#Volume of log with SED > 40cm
AssignVolSED40 <- function(TreeFreq, SED, LED, taper, Volume, rv20, l40)
{
v40<-ifelse(TreeFreq>0,
            ifelse(SED<40 & LED>40,3.1416/40000*l40*(30^2+l40/2*taper),
                   ifelse(SED>=40,
                          Volume,0))/rv20,
            0)
return(v40)
}


CalculateVt<-function(v0,v1, v2, v3,v4,v5,v6,v7,v8,v9,v10,v11,v12)
{
Vt<-v0+v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12

return(Vt)
  
}



# Function for model dataframe
makeModelDF<-function(low.df, high.df)
{
  ratio.df<- left_join(low.df, high.df, by=c("HarvestTypeCode", 'YieldRequestName', 'PopulationName', 'PopArea')) %>%
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
    ungroup()%>%
    select(Low, High, HarvestTypeCode, YieldRequestName, PopulationName, PopArea, NOP, yield, STB,STA,
           ymtv, p0, p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,pt, vt0, vt1, vt2, vt3, vt4, vt5, vt6,
           vt7, vt8, vt9, vt10, vt11, vt12, vtt, sp0, sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8,
           sp9, sp10, sp11, sp12, spt) %>%
    rename(vhigh = vtt)
  return(ratio.df)
}




#### Build tables ####


# Calculate population area... As long as the crews have entered the stratum areas correctly this will work
# If they haven't it wont... run some checks on this - JD.


pop.areas <- ss %>% filter(StratumName %in% c('#', '*')) %>%
  select(HarvestTypeCode, YieldRequestName, PopulationName, Area) %>%
  rename(PopArea = Area)


t<-o.df %>%left_join(pop.areas, by=c('HarvestTypeCode', 'YieldRequestName', 'PopulationName'))


t.o.df<-o.df%>% filter(PopulationName == '6028101')
t<-t.o.df %>%left_join(pop.areas, by=c('HarvestTypeCode', 'YieldRequestName', 'PopulationName'))

ts<-get_dupes(o.df)

# Make VolbyDCCallTree
VolbyDClassTree<-o.df %>% select(HarvestTypeCode, YieldRequestName, PopulationName, StratumName, StratumArea,
                PlotName, Age, TreeName, TreeFrequency, StemNumber, GradeName, Volume,
                Length, SmallEndDiameter, LargeEndDiameter) %>%
                rowwise() %>%
                mutate(vsl = cylVol(length = Length, SED = SmallEndDiameter, LED = LargeEndDiameter),
                       rV = calRV(weight = wt, TreeFreq = TreeFrequency, vsl = vsl, Volume = Volume, stratArea = StratumArea),
                       rv20 = calRV20(weight = wt, TreeFreq = TreeFrequency, stratArea = StratumArea),
                       vS = calvS(TreeFreq = TreeFrequency, length = Length, sed = SmallEndDiameter, rv = rV),
                       vL = calvL(TreeFreq = TreeFrequency, length = Length, led = LargeEndDiameter, rv = rV),
                       iS = calciS(sed = SmallEndDiameter),
                       iL = calciL(led = LargeEndDiameter),
                       taper = CalcTpr(LED = LargeEndDiameter, SED = SmallEndDiameter, length = Length),
                       v0 = AssignV0(iS = iS, iL = iL, vS = vS, vL = vL),
                       v1 = AssignV1(iS = iS, iL = iL, vS = vS, vL = vL),
                       v2 = AssignV2(iS = iS, iL = iL, vS = vS, vL = vL),
                       v3 = AssignV3(iS = iS, iL = iL, vS = vS, vL = vL),
                       v4 = AssignV4(iS = iS, iL = iL, vS = vS, vL = vL),
                       v5 = AssignV5(iS = iS, iL = iL, vS = vS, vL = vL),
                       v6 = AssignV6(iS = iS, iL = iL, vS = vS, vL = vL),
                       v7 = AssignV7(iS = iS, iL = iL, vS = vS, vL = vL),
                       v8 = AssignV8(iS = iS, iL = iL, vS = vS, vL = vL),
                       v9 = AssignV9(iS = iS, iL = iL, vS = vS, vL = vL),
                       v10 = AssignV10(iS = iS, iL = iL, vS = vS, vL = vL),
                       v11 = AssignV11(iS = iS, iL = iL, vS = vS, vL = vL),
                       v12 = AssignV12(iS = iS, iL = iL, vS = vS, vL = vL),
                       vT = CalculateVt(v0 = v0, v1 = v1, v2 = v2, v3 = v3, v4 = v4, v5 = v5, v6 = v6, v7 = v7, v8 = v8, v9 = v9, v10 = v10, v11 = v11, v12 = v12),
                       l15 = AssignLength15(TreeFreq = TreeFrequency, SED = SmallEndDiameter, LED = LargeEndDiameter, Length = Length),
                       l20 = AssignLength20(TreeFreq = TreeFrequency, SED = SmallEndDiameter, LED = LargeEndDiameter, Length = Length),
                       l30 = AssignLength30(TreeFreq = TreeFrequency, SED = SmallEndDiameter, LED = LargeEndDiameter, Length = Length),
                       l40 = AssignLength40(TreeFreq = TreeFrequency, SED = SmallEndDiameter, LED = LargeEndDiameter, Length = Length),
                       v15 = AssignVolSED15(TreeFreq = TreeFrequency, SED = SmallEndDiameter, LED = LargeEndDiameter, taper = taper, Volume = Volume, rv20 = rv20, l15 = l15),
                       v20 = AssignVolSED20(TreeFreq = TreeFrequency, SED = SmallEndDiameter, LED = LargeEndDiameter, taper = taper, Volume = Volume, rv20 = rv20, l20 = l20),
                       v30 = AssignVolSED30(TreeFreq = TreeFrequency, SED = SmallEndDiameter, LED = LargeEndDiameter, taper = taper, Volume = Volume, rv20 = rv20, l30 = l30),
                       v40 = AssignVolSED40(TreeFreq = TreeFrequency, SED = SmallEndDiameter, LED = LargeEndDiameter, taper = taper, Volume = Volume, rv20 = rv20, l40 = l40))%>%
  ungroup() %>%
  left_join(pop.areas, by=c('HarvestTypeCode', 'YieldRequestName', 'PopulationName')) %>% # Add in the population areas based on the sum of the stratum areas
  ungroup()

#Above table tested.



# Make VolbyDCPlot
VolbyDClassPlot<- VolbyDClassTree %>% filter(TreeFrequency > 0) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, StratumName, StratumArea, PlotName, GradeName, PopArea) %>%
  summarise(SV = sum(Volume * TreeFrequency),
            av0 = sum(v0),
            av1 = sum(v1),
            av2 = sum(v2),
            av3 = sum(v3),
            av4 = sum(v4),
            av5 = sum(v5),
            av6 = sum(v6),
            av7 = sum(v7),
            av8 = sum(v8),
            av9 = sum(v9),
            av10 = sum(v10),
            av11 = sum(v11),
            av12 = sum(v12),
            aVt = sum(vT),
            av15 = sum(v15),
            av20 = sum(v20),
            av30 = sum(v30),
            av40 = sum(v40)) %>%
  mutate(v0 = av0 /StratumArea,
         v1 = av1 /StratumArea,
         v2 = av2 /StratumArea,
         v3 = av3 /StratumArea,
         v4 = av4 /StratumArea,
         v5 = av5 /StratumArea,
         v6 = av6 /StratumArea,
         v7 = av7 /StratumArea,
         v8 = av8 /StratumArea,
         v9 = av9 /StratumArea,
         v10 = av10 /StratumArea,
         v11 = av11 /StratumArea,
         v12 = av12 /StratumArea,
         Vt = aVt /StratumArea) %>% ungroup()



# Make counts per stratum df
CountPlotsPerStratum<-ss %>% filter(StratumName != '#') %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, StratumName) %>%
  summarise(n = n_distinct(PlotName))



# Make VolbyDCClassByStratum

VolbyDClassStratum<-VolbyDClassPlot%>% 
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, PopArea, StratumName, StratumArea, GradeName) %>%
  inner_join(CountPlotsPerStratum)  %>%
  summarise(aSV = sum(SV),
            ssv = sum(SV*StratumArea/n),
            sv0 = sum(av0/n),
            sv1 = sum(av1/n),
            sv2 = sum(av2/n),
            sv3 = sum(av3/n),
            sv4 = sum(av4/n),
            sv5 = sum(av5/n),
            sv6 = sum(av6/n),
            sv7 = sum(av7/n),
            sv8 = sum(av8/n),
            sv9 = sum(av9/n),
            sv10 = sum(av10/n),
            sv11 = sum(av11/n),
            sv12 = sum(av12/n),
            svt = sum(aVt/n),
            sv15 = sum(av15/n),
            sv20 = sum(av20/n),
            sv30 = sum(av30/n),
            sv40 = sum(av40/n), 
            n = max(n)) %>%
  mutate(v = aSV/n) %>% ungroup()



# Make VolbyDCClassByLC
VolbyDCClassByLC<-VolbyDClassStratum %>% filter(GradeName != 'top') %>%
  mutate(ProductGroup = ifelse(GradeName %in% c('stump', 'waste, break'), 'NM', 'product')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, PopArea, GradeName) %>%
  summarise(SumOfn = sum(n),
            SumOfssv = sum(ssv),
            ssv0 = sum(sv0),
            ssv1 = sum(sv1),
            ssv2 = sum(sv2),
            ssv3 = sum(sv3),
            ssv4 = sum(sv4),
            ssv5 = sum(sv5),
            ssv6 = sum(sv6),
            ssv7 = sum(sv7),
            ssv8 = sum(sv8),
            ssv9 = sum(sv9),
            ssv10 = sum(sv10),
            ssv11 = sum(sv11),
            ssv12 = sum(sv12),
            ssvt = sum(svt),
            ssv15 = sum(sv15),
            ssv20 = sum(sv20),
            ssv30 = sum(sv30),
            ssv40 = sum(sv40)) %>%
  mutate(vt = ssvt / PopArea,
         v15 = ssv15 / PopArea,
         v20 = ssv20/ PopArea,
         v30 = ssv30 / PopArea,
         v40 = ssv40 / PopArea) %>% ungroup()



# Make High_TSV dataframe

h.tsv<-VolbyDCClassByLC %>% filter(!GradeName %in% c('top', 'stump')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, PopArea) %>%
  summarise(vt0 = sum(ssv0),
            vt1 = sum(ssv1),
            vt2 = sum(ssv2),
            vt3 = sum(ssv3),
            vt4 = sum(ssv4), 
            vt5 = sum(ssv5), 
            vt6 = sum(ssv6),
            vt7 = sum(ssv7),
            vt8 = sum(ssv8),
            vt9 = sum(ssv9),
            vt10 = sum(ssv10),
            vt11 = sum(ssv11),
            vt12 = sum(ssv12),
            vtt = sum(ssvt)) %>%
  mutate(vttha = vtt / PopArea,
         High = 'TSV') %>% ungroup()



# Make low waste data frame
l.waste<-VolbyDCClassByLC %>% filter(GradeName %in% c('break', 'waste' )) %>%
  inner_join(r2lci, by = 'PopulationName') %>%
  mutate(yield = paste(substring(HarvestTypeCode, 1,1), substring(YieldRequestName, 1,1), substring(NOP, 1,1), sep='')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, PopArea, NOP, yield, STB, STA, MTVSN, MTVS) %>%
  summarise(sp0 = sum(ssv0),
            sp1 = sum(ssv1),
            sp2 = sum(ssv2),
            sp3 = sum(ssv3),
            sp4 = sum(ssv4),
            sp5 = sum(ssv5), 
            sp6 = sum(ssv6), 
            sp7 = sum(ssv7),
            sp8 = sum(ssv8),
            sp9 = sum(ssv9),
            sp10 = sum(ssv10),
            sp11 = sum(ssv11),
            sp12 = sum(ssv12),
            spt = sum(ssvt)) %>%
  mutate(sptha = spt/PopArea,
         Low = 'waste') %>%
  filter(yield %in% c('CCC', 'TTT')) %>% ungroup()



# Make low trv data frame
l.trv<-VolbyDCClassByLC %>% filter(!GradeName %in% c('break', 'waste', 'top', 'stump' )) %>%
  inner_join(r2lci, by = 'PopulationName') %>%
  mutate(yield = paste(substring(HarvestTypeCode, 1,1), substring(YieldRequestName, 1,1), substring(NOP, 1,1), sep='')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, PopArea, NOP, yield, STB, STA, MTVSN, MTVS) %>%
  summarise(sp0 = sum(ssv0),
            sp1 = sum(ssv1),
            sp2 = sum(ssv2),
            sp3 = sum(ssv3),
            sp4 = sum(ssv4),
            sp5 = sum(ssv5), 
            sp6 = sum(ssv6), 
            sp7 = sum(ssv7),
            sp8 = sum(ssv8),
            sp9 = sum(ssv9),
            sp10 = sum(ssv10),
            sp11 = sum(ssv11),
            sp12 = sum(ssv12),
            spt = sum(ssvt)) %>%
  mutate(sptha = spt/PopArea,
         Low = 'notwaste') %>%
  filter(yield %in% c('CCC', 'TTT')) %>% ungroup()

 

# Make high trv data frame
h.trv<-VolbyDCClassByLC %>% filter(!GradeName %in% c('break', 'waste', 'top', 'stump')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, PopArea) %>%
  summarise(vt0 = sum(ssv0),
            vt1 = sum(ssv1),
            vt2 = sum(ssv2),
            vt3 = sum(ssv3),
            vt4 = sum(ssv4), 
            vt5 = sum(ssv5), 
            vt6 = sum(ssv6),
            vt7 = sum(ssv7),
            vt8 = sum(ssv8),
            vt9 = sum(ssv9),
            vt10 = sum(ssv10),
            vt11 = sum(ssv11),
            vt12 = sum(ssv12),
            vtt = sum(ssvt)) %>%
  mutate(vttha = vtt / PopArea,
         High = 'TRV') %>% ungroup()


# Make low log data frame
l.log<-VolbyDCClassByLC %>% filter(GradeName %in% c('Oversize_Sawlog', 'Long_Sawlog', 'Short_Sawlog', 'ISW')) %>%
  inner_join(r2lci, by = 'PopulationName') %>%
  mutate(yield = paste(substring(HarvestTypeCode, 1,1), substring(YieldRequestName, 1,1), substring(NOP, 1,1), sep='')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName,PopArea, NOP, yield, STB, STA, MTVSN, MTVS) %>%
  summarise(sp0 = sum(ssv0),
            sp1 = sum(ssv1),
            sp2 = sum(ssv2),
            sp3 = sum(ssv3),
            sp4 = sum(ssv4),
            sp5 = sum(ssv5), 
            sp6 = sum(ssv6), 
            sp7 = sum(ssv7),
            sp8 = sum(ssv8),
            sp9 = sum(ssv9),
            sp10 = sum(ssv10),
            sp11 = sum(ssv11),
            sp12 = sum(ssv12),
            spt = sum(ssvt)) %>%
  mutate(sptha = spt/ PopArea,
         Low = 'log') %>%
  filter(yield %in% c('CCC', 'TTT')) %>% ungroup()


# Make low non log data frame
l.nonlog<-VolbyDCClassByLC %>% filter(!GradeName %in% c('Oversize_Sawlog', 'Long_Sawlog', 'Short_Sawlog', 'ISW',
                                                         'top', 'break', 'stump', 'waste')) %>%
  inner_join(r2lci, by = 'PopulationName') %>%
  mutate(yield = paste(substring(HarvestTypeCode, 1,1), substring(YieldRequestName, 1,1), substring(NOP, 1,1), sep='')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, PopArea, NOP, yield, STB, STA, MTVSN, MTVS) %>%
  summarise(sp0 = sum(ssv0),
            sp1 = sum(ssv1),
            sp2 = sum(ssv2),
            sp3 = sum(ssv3),
            sp4 = sum(ssv4),
            sp5 = sum(ssv5), 
            sp6 = sum(ssv6), 
            sp7 = sum(ssv7),
            sp8 = sum(ssv8),
            sp9 = sum(ssv9),
            sp10 = sum(ssv10),
            sp11 = sum(ssv11),
            sp12 = sum(ssv12),
            spt = sum(ssvt)) %>%
  mutate(sptha = spt/PopArea,
         Low = 'nonlog') %>%
  filter(yield %in% c('CCC', 'TTT')) %>% ungroup()


# Make high non lo data frame
h.nonlog<-VolbyDCClassByLC %>% filter(!GradeName %in% c('Oversize_Sawlog', 'Long_Sawlog', 'Short_Sawlog', 'ISW',
                                                       'top', 'break', 'stump', 'waste')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, PopArea) %>%
  summarise(vt0 = sum(ssv0),
            vt1 = sum(ssv1),
            vt2 = sum(ssv2),
            vt3 = sum(ssv3),
            vt4 = sum(ssv4), 
            vt5 = sum(ssv5), 
            vt6 = sum(ssv6),
            vt7 = sum(ssv7),
            vt8 = sum(ssv8),
            vt9 = sum(ssv9),
            vt10 = sum(ssv10),
            vt11 = sum(ssv11),
            vt12 = sum(ssv12),
            vtt = sum(ssvt)) %>%
  mutate(vttha = vtt / PopArea,
         High = 'nonlog') %>% ungroup()


# Make low pulp data frame
l.pulp<-VolbyDCClassByLC %>% filter(GradeName %in% c('Pulp')) %>%
  inner_join(r2lci, by = 'PopulationName') %>%
  mutate(yield = paste(substring(HarvestTypeCode, 1,1), substring(YieldRequestName, 1,1), substring(NOP, 1,1), sep='')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName,PopArea, NOP, yield, STB, STA, MTVSN, MTVS) %>%
  summarise(sp0 = sum(ssv0),
            sp1 = sum(ssv1),
            sp2 = sum(ssv2),
            sp3 = sum(ssv3),
            sp4 = sum(ssv4),
            sp5 = sum(ssv5), 
            sp6 = sum(ssv6), 
            sp7 = sum(ssv7),
            sp8 = sum(ssv8),
            sp9 = sum(ssv9),
            sp10 = sum(ssv10),
            sp11 = sum(ssv11),
            sp12 = sum(ssv12),
            spt = sum(ssvt)) %>%
  mutate(sptha = spt/ PopArea,
         Low = 'Pulp') %>%
  filter(yield %in% c('CCC', 'TTT')) %>% ungroup()


# Make low pres data frame
l.pres<-VolbyDCClassByLC %>% filter(GradeName %in% c('Pres')) %>%
  inner_join(r2lci, by = 'PopulationName') %>%
  mutate(yield = paste(substring(HarvestTypeCode, 1,1), substring(YieldRequestName, 1,1), substring(NOP, 1,1), sep='')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName,PopArea, NOP, yield, STB, STA, MTVSN, MTVS) %>%
  summarise(sp0 = sum(ssv0),
            sp1 = sum(ssv1),
            sp2 = sum(ssv2),
            sp3 = sum(ssv3),
            sp4 = sum(ssv4),
            sp5 = sum(ssv5), 
            sp6 = sum(ssv6), 
            sp7 = sum(ssv7),
            sp8 = sum(ssv8),
            sp9 = sum(ssv9),
            sp10 = sum(ssv10),
            sp11 = sum(ssv11),
            sp12 = sum(ssv12),
            spt = sum(ssvt)) %>%
  mutate(sptha = spt/PopArea,
         Low = 'Pres') %>%
  filter(yield %in% c('CCC', 'TTT')) %>% ungroup()


# Make high log data frame
h.log<-VolbyDCClassByLC %>% filter(GradeName %in% c('Oversize_Sawlog', 'Long_Sawlog', 'Short_Sawlog', 'ISW')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, PopArea) %>%
  summarise(vt0 = sum(ssv0),
            vt1 = sum(ssv1),
            vt2 = sum(ssv2),
            vt3 = sum(ssv3),
            vt4 = sum(ssv4), 
            vt5 = sum(ssv5), 
            vt6 = sum(ssv6),
            vt7 = sum(ssv7),
            vt8 = sum(ssv8),
            vt9 = sum(ssv9),
            vt10 = sum(ssv10),
            vt11 = sum(ssv11),
            vt12 = sum(ssv12),
            vtt = sum(ssvt)) %>%
  mutate(vttha = vtt / PopArea,
         High = 'log') %>% ungroup()


# Make low sawlog data frame
l.sawlog<-VolbyDCClassByLC %>% filter(GradeName %in% c('Oversize_Sawlog', 'Long_Sawlog', 'Short_Sawlog')) %>%
  inner_join(r2lci, by = 'PopulationName') %>%
  mutate(yield = paste(substring(HarvestTypeCode, 1,1), substring(YieldRequestName, 1,1), substring(NOP, 1,1), sep='')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName, PopArea, NOP, yield, STB, STA, MTVSN, MTVS) %>%
  summarise(sp0 = sum(ssv0),
            sp1 = sum(ssv1),
            sp2 = sum(ssv2),
            sp3 = sum(ssv3),
            sp4 = sum(ssv4),
            sp5 = sum(ssv5), 
            sp6 = sum(ssv6), 
            sp7 = sum(ssv7),
            sp8 = sum(ssv8),
            sp9 = sum(ssv9),
            sp10 = sum(ssv10),
            sp11 = sum(ssv11),
            sp12 = sum(ssv12),
            spt = sum(ssvt)) %>%
  mutate(sptha = spt/PopArea,
         Low = 'Sawlog') %>%
  filter(yield %in% c('CCC', 'TTT')) %>% ungroup()


# Make low industrial data frame
l.industrial<-VolbyDCClassByLC %>% filter(GradeName %in% c('ISW')) %>%
  inner_join(r2lci, by = 'PopulationName') %>%
  mutate(yield = paste(substring(HarvestTypeCode, 1,1), substring(YieldRequestName, 1,1), substring(NOP, 1,1), sep='')) %>%
  group_by(HarvestTypeCode, YieldRequestName, PopulationName,PopArea, NOP, yield, STB, STA, MTVSN, MTVS) %>%
  summarise(sp0 = sum(ssv0),
            sp1 = sum(ssv1),
            sp2 = sum(ssv2),
            sp3 = sum(ssv3),
            sp4 = sum(ssv4),
            sp5 = sum(ssv5), 
            sp6 = sum(ssv6), 
            sp7 = sum(ssv7),
            sp8 = sum(ssv8),
            sp9 = sum(ssv9),
            sp10 = sum(ssv10),
            sp11 = sum(ssv11),
            sp12 = sum(ssv12),
            spt = sum(ssvt)) %>%
  mutate(sptha = spt/PopArea,
         Low = 'Industrial') %>%
  filter(yield %in% c('CCC', 'TTT')) %>% ungroup()
                                                
                                                
                                               
                                                
                                                
                                                
                                                
  











#### Prepare model fitting datasets ####

# Apply funtion to produce the modelling dataframe
p_TRV_TSV<-makeModelDF(low.df = l.trv, high.df = h.tsv) 
p_Waste_TSV<-makeModelDF(low.df = l.waste, high.df = h.tsv)
p_nonlog_TRV<-makeModelDF(low.df = l.nonlog, high.df = h.trv) 
p_log_TRV<-makeModelDF(low.df = l.log, high.df = h.trv) %>% ungroup()
p_pulp_nonlog<-makeModelDF(low.df = l.pulp, high.df = h.nonlog) %>% ungroup()
p_pres_nonlog<-makeModelDF(low.df = l.pres, high.df = h.nonlog) %>% ungroup()
p_sawlog_log<-makeModelDF(low.df = l.sawlog, high.df = h.log) %>% ungroup()
p_industrial_log<-makeModelDF(low.df = l.industrial, high.df = h.log) %>% ungroup()
