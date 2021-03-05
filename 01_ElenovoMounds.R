#### Processing Elenovo 2017 Mounds

## using both full 2017 dataset and dataset spatially constrained to fall within Yambol boundsries (Bara dataset, Attributes.csv) 

# Load the libraries (install package if needed)
#install.packages('tidyverse')
library(tidyverse)
library(RColorBrewer)
library(ggplot2)

# Set your workspace if desirable
#setwd("C:/Users/au616760/Documents/RStudio/Elenovo2017")

# Load the datasets: cleaned data by Adela and spatial subset by Bara

mounds_adela <- read_csv("data/ElenovoMounds_cleaned.csv")
mounds_bara <- read_csv2("data/Attributes.csv")

# Alternative source https://raw.githubusercontent.com/adivea/Elenovo2017/master/data/ElenovoMounds_cleaned.csv
?download.file() # 


# Visual check
dim(mounds_adela)
dim(mounds_bara)
colnames(mounds_adela)
colnames(mounds_bara)
head(mounds_adela)

### Merge the two to get final 2017 mound dataset, which includes 
# all attributes from my cleaned mounds for all rows included in Bara's (subset of those in Yambol region) 

mounds <- merge(mounds_adela, mounds_bara[,c(2,3,9)], # picking uuid, identifier and type from Bara
                by ="identifier", all.y=TRUE) 

# alternative but without bara's type
mounds_adela %>% 
    filter(mounds_adela$identifier%in%mounds_bara$MoundID)

colnames(mounds)
dim(mounds)
mounds$Latitude[1:5]

#rename duplicate columns 
colnames(mounds)[5]<- "Type_Adela" #Type.x column renamed
colnames(mounds)[43]<-"Type" #Type.y renamed

#check goodness of data
summary(mounds$HeightMax) # sanity check of heights range
which(mounds$HeightMax[mounds$Type!="Settlement Mound"]>7) #65 burial mounds are over 7 m 
mounds[65,]  # this shows that Bara's types are better than Adela's

# check categories of mound type (via conversion to factor) 
levels(factor(mounds$Type))
class(mounds$Type)


mounds$Type[5:10]

# Calculate Height statistics per type of features

Ele <- mounds %>% 
  group_by(Type) %>% 
  tally()
Ele

MedianH <- mounds %>% 
  group_by(Type) %>% 
  summarize(medianHeight=median(HeightMax, na.rm=TRUE),medianDiam=median(DiameterMax, na.rm=TRUE)) 
MedianH

MeanH <- mounds %>% 
  group_by(Type) %>% 
  summarize(minHeight=min(HeightMax, na.rm=TRUE), maxHeight=max(HeightMax,na.rm=TRUE), meanHeight = mean(HeightMax, na.rm=TRUE)) 
MeanH  # beware of the Uncertain feature if reusing
  
MeanDiam <- mounds %>% 
  group_by(Type) %>% 
  summarize(minDiam=min(DiameterMax, na.rm=TRUE), maxDiam=max(DiameterMax,na.rm=TRUE), meanDiam = mean(DiameterMax, na.rm=TRUE)) 
MeanDiam

Ele_stats <- cbind(Ele[-3:-4,],MeanH[-3:-4,2:4], MeanDiam[-3:-4, -1])
Ele_stats

write_csv(Ele_stats, "Ele_Dimensions_Baramounds.csv")
write_csv(mounds, "output/2017ElenovoMoundsInYambol.csv")

# Calculate statistics by source (survey or legacy data verification)

Source <- mounds %>% 
  group_by(Source) %>% 
  tally()
Source

Stats <- mounds %>% 
  group_by(Source) %>% 
  summarize(minHeight=min(HeightMax, na.rm=TRUE), maxHeight=max(HeightMax,na.rm=TRUE), meanHeight = mean(HeightMax, na.rm=TRUE), 
            minDiam=min(DiameterMax, na.rm=TRUE), maxDiam=max(DiameterMax,na.rm=TRUE), meanDiam = mean(DiameterMax, na.rm=TRUE)) 
Stats

Source_stats <- cbind(Source,Stats[,-1])  # eliminate duplicate column
write_csv(Source_stats, 'Ele_Sourcestats.csv')


### Create a BoxPlot comparing the height distributions of mounds (exluding other stuff)


# index burial and uncertain mounds
mounds_index <- which(mounds$Type=="Burial Mound")
uncertain_index <- which(mounds$Type=="Uncertain Mound")
extinct_index <- which(mounds$Type=="Extinct Burial Mound")

# create boxplot of heights for mound phenomena (no surf. scatter or other)
mound_index <- mounds[c(mounds_index,extinct_index,uncertain_index),]

head(mound_index)
boxplot(HeightMax~Type, mound_index)

boxplot(DiameterMax~Type, mound_index)


pdf("output/2017Combined2.pdf", 13, 5 )
# run the code below to generate the figure for pdf:

par(mfrow=c(1,2)) # to combine the two plots below horizontally

boxplot(HeightMax~Type, data = mound_index, 
        # col = gray.colors(3),
        main = "Height distribution",
        xlab = "", # to eliminate Type as x label
        ylab = "meters", cex.lab = 1.3,    #cex = increases symbols in plot, cex.lab - increases axis labels
        cex.axis = 1,                      #cex.axis = increases data labels
        las = 1) # rotate y axis

boxplot(DiameterMax~Type, mound_index,
          # col = gray.colors(3),
          main = "Diameter distribution",
          ylab = "",
          xlab = "",   
          cex.axis = 1,                      #cex.axis = increases data labels
          las = 1) # rotate y axis

dev.off()

####################################################
### Wish to try a Shiny application? 
### Run the 02_interactive_data_explorer.R


### Streamlining Mound Condition (can be done in OpenRefine)
levels(factor(mounds$Condition))

mounds <- mounds %>%
  mutate(Condition = str_extract(Condition, "\\d")) %>%
  mutate(Condition = case_when(Condition == 0 ~ "NA",
                               Condition == 6 ~ "5",
                               Condition != 0 ~ Condition))
mounds$Condition <- as.numeric(mounds$Condition)
unique(mounds$Condition)

write_csv(mounds, "output/Condition.csv")

####################################################
### Wish to create a map?
## Playing with mound height visualisation 

p <- ggplot(mound_index, aes(Type, HeightMax, color=Type)) +
  geom_violin(trim=FALSE)
p
# violin plot with mean points
p + stat_summary(fun=mean, geom="point", shape=23, size=2)
# violin plot with median points
p + stat_summary(fun=median, geom="point", size=2, color="red")
# violin plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))


# Get a tally of visited features by team leader and day
mounds %>% 
  group_by(createdBy) %>% 
  tally()

# Review the progress of individual teams
teamprogress <- mounds %>% 
  group_by(createdBy, Date) %>% 
  tally()

teamprogress %>% 
  arrange(desc(n))


## Create a quick Map

library(leaflet)

map <- leaflet() %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Topo") %>%
  addProviderTiles("Esri.WorldImagery", group = "ESRI Aerial") %>%
  addCircleMarkers(lng = as.numeric(mounds$Longitude),
                   lat = as.numeric(mounds$Latitude),
                   radius = mounds$HeightMax, group="Legacy",
                   popup = paste0("MoundID: ", mounds$identifier,
                                  "<br> Height: ", mounds$HeightMax,
                                  "<br> Robber's trenches: ", mounds$RTDescription)) %>% 
  addLayersControl(
    baseGroups = c("Topo","ESRI Aerial"),
    overlayGroups = c("Legacy"),
    options = layersControlOptions(collapsed = T))

map

