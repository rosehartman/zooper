#OK, let's work on liberty island since USGS is terribel

#Zooplankton identification was undertaken by two separate teams, with one group (University of
#Washington) taking on data from the 2015-2016 sampling years and another (BSA Environmental)
#handling 2017-2019. These groups used different taxonomical levels for identification. We have
#reconciled these differences by reporting the highest taxonomical level for each species or taxonomical
#group found in our data – for example, Cladocerans identified by BSA as being of the genus’ Alona,
#Bosmina, Ceriodaphnia, Chydorus, Daphnia, Kurzia, and Simocephalus are classified as Cladocerans. A
#full list of the taxonomical groupings and their combined/pooled form can be found below.

#That's sad. Maby i can contact them for the raw data. But at least it makes it a bit easier.


library(tidyverse)
LIzoops = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.987.2&entityid=4840838acda6692bddfb7a2eafc09562")

unique(LIzoops$Taxon)
#oof. that's very high level.

table(LIzoops$Easting)
#sooooo, some of them are in degrees, some of them are in UTMs

LIzoops = mutate(LIzoops, Date = mdy(SampleDate), Year = year(Date),
                 Month = month(Date), SampleID = paste(SampleDate, SampleTime, Trawl))

ggplot(LIzoops, aes(x = Year, y = Count, fill = Taxon)) +
  geom_col(position = "fill")+
  facet_wrap(~Month)

#I have so many questions
#why does 2015 stand out as so different?
#There seems to be much more consistancy in the 29=017-2019 than the others.
#Why is psduedoiaptomus such a small percentage of the population even in June?
#Who are those other copepods?
#How did they deal with life stages?

# In 2013 and 2014 zooplankton samples were collected using a 200-cm
# long net with a 50-cm diameter mouth and mesh size of 150 µm,
# leading to a 500 ml cod end attachment. From 2015 to 2019,
# zooplankton sampling switched to mesh size of 53 µm, with the other
# net measurements remaining the same

LIsamps = group_by(LIzoops, SampleID, Date, Month, Year, Trawl, StationCode, Temperature) %>%
  summarize(N = n())

ggplot(LIsamps, aes(x = Year, fill = as.factor(Month))) + geom_bar()
#I feel like I should basically draop the 2013-2015 data.
#Then BSA is the only contractor and everything is more consistent
#I wonder if I can get those data?

#Data from USFWS (origional)

LIzoops = read_csv("data-raw/LFWO_LibertyI_zoop.csv")


sampbyyear = LIzoops %>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Process) %>%
  summarize(n = n())


CDFW = filter(LIzoops, Process == "CDFW")
BSA = filter(LIzoops, Process =="BSA")

unique(CDFW$Genus)
unique(BSA$Genus)

#Yeah, BSA identified a lot of stuff CDFW didn't. Let's just use the BSA data

BSA2 = filter(LIzoops, Process =="BSA", year(Date) >=2017)


foo = filter(test, is.na(Datetime))
View(foo)
foo2 = filter(LIzoops, ID %in% foo$Station)


###########################
LItest = Zoopsynther("Community", Sources = c("EMP", "USGS", "LI", "FRP", "STN", "FMWT", "DOP"), Years = c(2010:2023))


LItest2 = Zoopsynther("Community", Sources = "LI", Years = c(2015:2023))

ggplot(LItest, aes(x = Date, y = CPUE))+ geom_point()+
  facet_grid(SizeClass~Source)
test = filter(zoopEnvComb, Source == "LI")%>%
  left_join(zoopComb)



LItest2 = Zoopsynther("Community", Sources = "FRP", Years = c(2015:2023))
unique(LItest2$TowType)

allwetlandzoops = LItest
save(allwetlandzoops, file = "allwetlandzoops.RData")

LItest3 = Zoopsynther("Community", Sources = c("EMP", "USGS", "LI", "FRP", "STN", "FMWT", "DOP", "YBFMP"), Years = c(2010:2023))

YBFMP = filter(LItest3, Source == "YBFMP")


USGS = Zoopsynther(Data_type = "Community", Sources = "USGS")
USGSsum = select(USGS, Station, Year, Date, SampleID) %>%
  distinct()

USGSsum2 = USGSsum %>%
  mutate(Month = month(Date)) %>%
  group_by(Year, Month, Station) %>%
  summarize(N = n(
  ))

FRP = Zoopsynther("Community", Sources = "FRP")

FRP2 = group_by(FRP, Year, Station) %>%
  summarize(N = n())
