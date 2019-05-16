library("ggplot2")
library("dplyr")


#scientirif not
options(scipen = 999)

dataset = read.table("repdata_data_stormData.csv", sep = ",", skip=1)
colnames(dataset) = c("STATE__","BGN_DATE","BGN_TIME","TIME_ZONE","COUNTY","COUNTYNAME","STATE","EVTYPE","BGN_RANGE","BGN_AZI","BGN_LOCATI","END_DATE","END_TIME","COUNTY_END","COUNTYENDN","END_RANGE","END_AZI","END_LOCATI","LENGTH","WIDTH","F","MAG","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP","WFO","STATEOFFIC","ZONENAMES","LATITUDE","LONGITUDE","LATITUDE_E","LONGITUDE_","REMARKS","REFNUM")



# Get rows with fatalities greater than 0
subsetFatality = subset(dataset,dataset$FATALITIES>0)

# Tally the fatalities by EVTYPE

fatalityTallyDataset = subsetFatality %>% group_by(EVTYPE) %>% summarise("tally" = sum(FATALITIES))


# Get top 10

fatalityTallyDataset = head(arrange(fatalityTallyDataset, desc(fatalityTallyDataset$tally)), 10)

# Plot

ggplot(fatalityTallyDataset,aes(x= reorder(tally, EVTYPE), y=EVTYPE, group = 1)) + ggtitle("Number of fatalities based on top 10 causes") + geom_bar(stat="identity")

# ----------------------------------

# Get rows with injuries greater than 0
subsetInjury = subset(dataset,dataset$INJURIES>0)

# Tally the injuries by EVTYPE

injuryTallyDataset = subsetInjury %>% group_by(EVTYPE) %>% summarise("tally" = sum(INJURIES))

# get 10
topTenInjuryTallyDataset = head(arrange(injuryTallyDataset,desc(injuryTallyDataset$tally)),10)

# Plot


ggplot(data=topTenInjuryTallyDataset, aes(x=reorder(EVTYPE, -tally), y=tally,  group = 1)) +
    geom_bar(stat="identity") + coord_flip() +  ggtitle("Number of injuries based on top 10 causes")



#Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
#Across the United States, which types of events have the greatest economic consequences?




#-------------

# set column
dataset$propertyDamageComponent = 0
# if property damage exponent == K
dataset$propertyDamageComponent =  ifelse(dataset$PROPDMGEXP ==  'K' | dataset$PROPDMGEXP ==  'k', dataset$PROPDMG * 1000, dataset$propertyDamageComponent)
# if property damage exponent == M
dataset$propertyDamageComponent = ifelse(dataset$PROPDMGEXP ==  'M' | dataset$PROPDMGEXP ==  'm', dataset$PROPDMG * 1000000, dataset$propertyDamageComponent)

# if property damage exponent == B
dataset$propertyDamageComponent = ifelse(dataset$PROPDMGEXP ==  'B' | dataset$PROPDMGEXP ==  'b', dataset$PROPDMG * 1000000000, dataset$propertyDamageComponent)

# if property damage exponent > 0 & < =8
dataset$propertyDamageComponent = ifelse(!(dataset$PROPDMGEXP ==  'B' | dataset$PROPDMGEXP ==  'b' | dataset$PROPDMGEXP ==  'm' | dataset$PROPDMGEXP ==  'M' | dataset$PROPDMGEXP ==  'k' | dataset$PROPDMGEXP ==  'K') & (as.numeric(as.character(dataset$PROPDMGEXP)) > 0 & as.numeric(as.character(dataset$PROPDMGEXP))) <=  8, dataset$PROPDMG^as.numeric(as.character(dataset$PROPDMGEXP)), dataset$propertyDamageComponent)

# if property damage exponent == +, - ,?
dataset$propertyDamageComponent = ifelse((dataset$PROPDMGEXP == "+" | dataset$PROPDMGEXP == "-" | dataset$PROPDMGEXP == "?"), dataset$PROPDMG, dataset$propertyDamageComponent)


#-------------


# set column
dataset$cropDamageComponent = 0
# if crop damage exponent == K
dataset$cropDamageComponent =  ifelse(dataset$CROPDMGEXP ==  'K' | dataset$CROPDMGEXP ==  'k', dataset$CROPDMG * 1000, dataset$cropDamageComponent)
# if crop damage exponent == M
dataset$cropDamageComponent = ifelse(dataset$CROPDMGEXP ==  'M' | dataset$CROPDMGEXP ==  'm', dataset$CROPDMG * 1000000, dataset$cropDamageComponent)

# if crop damage exponent == B
dataset$cropDamageComponent = ifelse(dataset$CROPDMGEXP ==  'B' | dataset$CROPDMGEXP ==  'b', dataset$CROPDMG * 1000000000, dataset$cropDamageComponent)

# if crop damage exponent > 0 & < =8
dataset$cropDamageComponent = ifelse(!(dataset$CROPDMGEXP ==  'B' | dataset$CROPDMGEXP ==  'b' | dataset$CROPDMGEXP ==  'm' | dataset$CROPDMGEXP ==  'M' | dataset$CROPDMGEXP ==  'k' | dataset$CROPDMGEXP ==  'K') & (dataset$CROPDMGEXP == "0" & dataset$CROPDMGEXP == "2"), dataset$CROPDMG^as.numeric(as.character(dataset$CROPDMGEXP)), dataset$cropDamageComponent)

# if crop damage exponent == +, - ,?
dataset$cropDamageComponent = ifelse((dataset$CROPDMGEXP == "+" | dataset$CROPDMGEXP == "-" | dataset$CROPDMGEXP == "?"), dataset$CROPDMG, dataset$cropDamageComponent)



#--------- GEt the sum

tallyByTypePropertyDamage = dataset %>% group_by(EVTYPE) %>% summarise("tally" = sum(propertyDamageComponent, rm.na =TRUE))
tallyByTypeCropDamage = dataset %>% group_by(EVTYPE) %>% summarise("tally" = sum(cropDamageComponent, rm.na = TRUE))




#Consider writing your report as if it were to be read by a government or municipal manager who might be responsible for preparing for severe weather events and will need to prioritize resources for different types of events. However, there is no need to make any specific recommendations in your report.

View(dataset)



colnames(tallyByTypePropertyDamage) = c('EVTYPE','propertydamage')
colnames(tallyByTypeCropDamage) = c('EVTYPE','cropdamage')

tallyByTypeMerged = merge(tallyByTypePropertyDamage, tallyByTypeCropDamage, by = "EVTYPE")


tallyByTypeMerged$totalDamage = tallyByTypeMerged$cropdamage + tallyByTypeMerged$propertydamage

View(tallyByTypeMerged)


#get top 10
tallyByTypeMerged = head(arrange(tallyByTypeMerged, desc(tallyByTypeMerged$totalDamage, 10)), 10)


ggplot(tallyByTypeMerged, aes(x = reorder(EVTYPE, totalDamage), y = totalDamage)) + coord_flip() + 
    geom_bar(stat = "identity")  +labs(title="Total damage cost based on Crop and Property Damages",y ="Damage cost (Crop Damages + Property Damages)(in $)", x = "Event Type")



