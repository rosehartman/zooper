#Download and fiddle with DOP data
#and also FRP data.

library(tidyverse)
library(readxl)
library(lubridate)

#get all of DOP's accessory environmental data
DOPtrawls = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.3&entityid=a1abf498fc55b569977d252563f8fcab")

#mesozooplankton
DOPmeso = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.3&entityid=36a54d0243fada06c94344477c198bc4")

#macrozooplankton
DOPmacro = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1187.3&entityid=5dfc521f61f7494521f3d4b8df05256b")

crosswalk = read_csv("data-raw/crosswalk.csv")



#DOP data from wide to long, then merge with trawl data
DOPlong = pivot_longer(DOPmeso, cols = !ICF_ID, names_to = "DOP_Meso", values_to = "CPUE")

#make sure all names are in the crosswalk
DOPtaxa = unique(DOPlong$DOP_Meso)
DOPtaxa %in% crosswalk$DOP_Meso
DOPtaxa[which(!DOPtaxa %in% crosswalk$DOP_Meso)]

#merge with trawl data and crosswalk
DOPlong2 = left_join(DOPtrawls, DOPlong) %>%
 left_join(crosswalk) %>%
  filter(!is.na(CPUE))

#same with the macrozooplankton data
DOPmacrolong = pivot_longer(DOPmacro, cols = !ICF_ID,  names_to = "DOP_Macro", values_to = "CPUE")


#make sure all names are in the crosswalk
DOPtaxam = unique(DOPmacrolong$DOP_Macro)
DOPtaxam %in% crosswalk$DOP_Macro
DOPtaxam[which(!DOPtaxam %in% crosswalk$DOP_Macro)]

testy = left_join(DOPtrawls, DOPmeso)
testy2 = left_join(DOPtrawls, DOPmacro)

#so I need to find teh earliest record for each taxa. Grump.

earliest = function(data, taxa){
  data1 = filter(data, !is.na(.data[[taxa]]))
  print(taxa)
  print(min(data1$Date))
  print(max(data1$Date))
}

earliest(testy2, "Eogammarus_spp")

test = filter(testy2, !is.na(Eogammarus_spp))
min(test$Date)

taxaMeso = names(select(DOPmeso, -ICF_ID))
taxaMacro = names(select(DOPmacro, -ICF_ID))

for(i in 1:length(taxaMeso)){
  earliest(testy, taxaMeso[i])
}

for(i in 1:length(taxaMacro)){
  earliest(testy2, taxaMacro[i])
}


#What's going on with these missing samples?
testy2 = mutate(testy2, Source = "DOP", SampleID=paste(Source, Station_Code, Date, ICF_ID))
foo = filter(testy2, SampleID %in% test)


#Is the DOP data missing anything?

DOP = filter(zoopComb, Source == "DOP")

DOP2 = group_by(DOP, Taxlifestage) %>%
  summarize(N = n(), sum = sum(CPUE, na.rm = T))


length(DOPtrawls$ICF_ID %in% DOPmeso$ICF_ID)
length(DOPmeso$ICF_ID %in% DOPtrawls$ICF_ID)


testy = left_join(DOPtrawls, DOPmeso)
sub = filter(testy, is.na(Acanthocyclops_spp_adult))

testy2 = left_join(DOPtrawls, DOPmacro)

sub2 = filter(testy2, is.na(Eogammarus_spp), !is.na(Americorophium_spp))

test2 = filter(testy, Station_Code %in% c("19-11-LSC-02", "DOP 19-11-LSC-01"))

#Huh. We're missing coordinates for those two sites, no idea why.

DOPtrawlsx = mutate(DOPtrawls, Date1 = as.character(Date))
DOPtrawlsx = mutate(DOPtrawls, Datetime= dplyr::if_else(is.na(Start_Time), as.POSIXct(ymd(as.character(Date), tz = "America/Los_Angeles")),
                                                    lubridate::ymd(as.character(Date), tz = "America/Los_Angeles") + lubridate::hms(as.character(Start_Time))))

#fiture out what's going wrong with zooper
foo = filter(DOPmeso, ICF_ID %in% c("ICF59", "ICF647", "ICF649", "ICF2555", "ICF2487", "ICF2531", "ICF2573", "ICF4006",
                                    "ICF3069"))


##############################
#make graphs cuase they are fun!

ggplot(DOPlong2, aes(x = Habitat, y = CPUE, fill = Taxname)) + geom_col(position = "fill")

DOPlong2 = mutate(DOPlong2, Year = year(Date), Month = month(Date))

ggplot(DOPlong2, aes(x = Habitat, y = CPUE, fill = Genus)) + geom_col(position = "fill") +
  facet_wrap(~Year)

#Huh. What happened to Limnoithona?
Limno = filter(DOPlong2, Genus == "Limnoithona")
ggplot(Limno, aes(x = Habitat, y = CPUE, fill = Species)) + geom_col()+
  facet_wrap(~Year)

#maybe sample number changed?

Limnomean = group_by(Limno, Month, Year, Habitat, Species) %>%
  summarize(CPUE = mean(CPUE))
ggplot(Limnomean, aes(x = Habitat, y = CPUE, fill = Species)) + geom_col()+
  facet_grid(Month~Year)

############################################################################
FRP = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.3&entityid=5218ffbc7b8f38959704a46ffb668ad9")
sites_FRP_Macro = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.3&entityid=fa84750e51c319d309f97357b7d34315")
FRP_all = left_join(FRP, sites_FRP_Macro)
zoo_FRP_Macro = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.269.3&entityid=8de785ee47220f3893654478c79b5f8f")
FRP_allmacro = left_join(zoo_FRP_Macro, sites_FRP_Macro)

write.csv(sort(unique(FRP_all$CommonName)), file = "FRPspecies.csv")
write.csv(sort(unique(FRP_allmacro$CommonName)), file = "FRPmacrospecies.csv")


crosswalk = read_csv("data-raw/crosswalk.csv")

cross = group_by(crosswalk, Taxname) %>%
  summarize(N = n(), Class = length(unique(Class)), Order = length(unique(Order)),
            Family = length(unique(Family)),Genus = length(unique(Genus)))

cross2 = group_by(crosswalk, YBFMP) %>%
  summarize(N = n())

#Some of these names are just dumb, I'll recode
FRP_all = mutate(FRP_all, CommonName = case_when(CommonName == "Fish larvae" ~ "Fish UNID",
                                                 CommonName == "Insect Unid" ~ "Insect UNID",
                                                 CommonName == "Calanoid copepod (gravid)" ~ "Calanoid UNID",
                                                 CommonName == "Hymenoptera UNID" ~ "Hymenoptera Other",
                                                 CommonName == "Asellidae UNID" ~ "Asellidae",
                                                 CommonName == "Tricoptera larvae UNID"~"Trichoptera larvae Other",
                                                 TRUE ~ CommonName))
foo = filter(FRP, CommonName == "Calanoid copepod (gravid)")

FRP_allmacro = mutate(FRP_allmacro, CommonName = case_when(CommonName == "Fish larvae" ~ "Fish UNID",
                                                 CommonName == "Insect Unid" ~ "Insect UNID",
                                                 CommonName == "Palaemonetes" ~ "Palaemon",
                                                 CommonName == "Hymenoptera UNID" ~ "Hymenoptera Other",
                                                 CommonName == "Asellidae UNID" ~ "Asellidae",
                                                 CommonName == "Diptera adult" ~ "Diptera Adult",
                                                 CommonName == "Coleoptera other" ~ "Coleoptera Other",
                                                 CommonName == "Tricoptera larvae UNID"~"Trichoptera larvae Other",
                                                  TRUE ~ CommonName))

#make sure all names are in the crosswalk
FRPtaxa = unique(FRP_all$CommonName)
FRPtaxam = unique(FRP_allmacro$CommonName)
FRPtaxa %in% crosswalk$FRP_Meso
FRPtaxam[which(!FRPtaxam %in% crosswalk$FRP_Macro)]
FRPtaxa[which(!FRPtaxa %in% crosswalk$FRP_Meso)]

#make sure no names in the crosswalk aren't actually in the FRP data
probs = filter(crosswalk, !FRP_Macro %in% FRPtaxam)
probs2 = filter(crosswalk, !FRP_Meso %in% FRPtaxa)

crosswalkb = mutate(crosswalk, FRP_Meso = case_when(!FRP_Meso %in% FRPtaxa ~NA,
                                                    TRUE ~ FRP_Meso),
                    FRP_Macro = case_when(!FRP_Macro %in% FRPtaxam ~ NA,
                                          TRUE ~ FRP_Macro)) %>%
  filter(!if_all(EMP_Micro:DOP_Macro, is.na))
write.csv(crosswalkb, "crosswalkb.csv")

YBFMP = filter(zoopComb, Source == "YBFMP")

CrosswalkYBFMP = select(crosswalk, YBFMP, Taxname, Lifestage) %>%
  filter(!is.na(YBFMP))

Crosswalka = select(crosswalk, FRP_Macro, Taxname, Lifestage) %>%
  filter(!is.na(FRP_Macro))

Crosswalka %>%
  group_by(FRP_Macro) %>%
  filter(n()>1)

test = Zoopsynther(Data_type = "Community", Sources = c("EMP", "FRP", "FMWT"),
                   Size_class = "Meso", Years = c(2015:2021))

cross = select(zooper::crosswalk, Taxname, Level) %>%
  distinct() %>%
  group_by(Taxname) %>%
  summarize(N = n())

foo = unique(test$Taxlifestage)

test2 = group_by(test, SampleID) %>%
  summarize(N = n(), Taxa = length(unique(Taxlifestage)))

Keratellat = filter(test, Taxname == "Keratella_UnID")

ggplot(test, aes(x = Date, y = CPUE, color = Source))+ geom_point()

crosswalkx = crosswalk %>%
  group_by(Taxname, Lifestage) %>%
summarize(N= n()) %>%
  filter(N>1)

FRPstations = select(FRP_all, LatitudeStart, LongitudeStart, Location) %>%
  distinct()

Problems = filter(FRP_allmac, SampleID_frp %in% c("MAC2-BK-26APR2019", "MAC4-TR-19MAR2019", "MAC1-ND-08APR2019"))

Problems2 = filter(FRPsites, VisitNo %in% Problems$VisitNo)



Prob3 = filter(data.list$FRP_Macro, SampleID %in% c("MAC2-BK-26APR2019", "MAC4-TR-19MAR2019", "MAC1-ND-08APR2019"))
Prob3a= filter(data.list$FRP_Macro, SampleID %in% c("MAC2-BK-26APR2019", "MAC4-TR-19MAR2019", "MAC1-ND-08APR2019"))

##############################################################
#ship channel data

shipchannel = read_csv("data-raw/ZoopsCountsAll.csv")
DWSC = mutate(shipchannel, Month = month(Date), Year = year(Date))
ggplot(DWSC, aes(x = Month, y = NumberPerLiter, color = genus))+
  geom_point()+
  facet_wrap(~Year)

#sampling effort
DSWCsamps = select(DWSC, bottle.ID, Date, Station, Month, Year) %>%
  distinct()

ggplot(DSWCsamps, aes(x = Station, fill = as.factor(Month))) +
  geom_bar()+
  facet_wrap(~Year)
table(DSWCsamps$Station, DSWCsamps$Month, DSWCsamps$Year)

#taxa
DWSCtaxa = select(DWSC, genus, species, SDWSC) %>%
  distinct()
#OMG sometimes they use 'sp.' and sometimes 'spp.' grrr.

DWSC = mutate(DWSC, species = case_when(species=="spp." ~ NA, species=="sp." ~ NA,
                                        species == "(male)" ~ NA,
                                        species %in% c("galeata mendotae",
"galeata mendotae-mendotae",
"galeata-mendotae",)~ "galeata", species == "pulex complex" ~ "pulex",
species == "hulberti" ~ "hurlberti",
species == "larvae" ~ "larva",
.default = species),
genus = case_when(genus %in% c("Gastropod", "gastropod", "gastropoda") ~ "Gastropoda",
                  TRUE ~ genus),
              SDWSC = paste(genus, species))

write.csv(DWSCtaxa, "DWSCtaxa.csv")

#environmental data
evdata = read_csv("data-raw/enviro_data_forRosie.csv")
Crosswalk = read_csv("data-raw/crosswalk.csv")

shipchannel = Zoopsynther(Data_type = "Community", Sources = c("FMWT", "STN", "DOP", "DWSC"), Size_class = "Meso",
                          Crosswalk = Crosswalk, Years = c(2008:2022),Long_range = c(-121.55, -121.7),
                          Lat_range = c(38.2, 38.6),
                          Zoop =zoopComb,
                          ZoopEnv = zoopEnvComb)

shipchannela = Zoopsynther(Data_type = "Community", Sources = c("FMWT", "STN", "DOP", "DWSC"),
                           Zoop =zoopComb,Size_class = "Meso",
                           ZoopEnv = zoopEnvComb, Crosswalk = Crosswalk)
unique(shipchannela$Source)

ggplot(shipchannel, aes(x = Latitude, y = CPUE, color = Source))+geom_point()

shipchannel = mutate(shipchannel, DOY = yday(Date))
ggplot(shipchannel, aes(x = Latitude, y = DOY, size = log(CPUE+1), color = Source))+geom_point()

ggplot(shipchannel, aes(x = Latitude, y = CPUE, color = Taxname))+geom_point()

write.csv(shipchannel, "shipchannelzoops.csv", row.names = FALSE)
save(shipchannel, file = "shipchannelzoops.RData")

ggplot(shipchannel, aes(x = Date, y = CPUE, color = Taxname))+ geom_point()
