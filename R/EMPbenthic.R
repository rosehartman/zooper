#download the EMP benthic data and attach it to the rest

library(tidyverse)
#https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1036

Benthic_counts = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1036/6/7475c46f9c8bebf01ad24d50ee7a0ff9")

Benthic_CPUE = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1036/6/5855f038ec2899f759db9ee826d0092a")

Benthic_stations = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1036/6/4e6948186ad756dc2b6de4de41b601f3" )

Benthic_taxalookup = read_csv("https://pasta.lternet.edu/package/data/eml/edi/1036/6/d0f0dd3c1835fe5b669342f8c8e77024")

crosswalk = read_csv("data-raw/crosswalk.csv") %>%
  select(Lifestage, Taxname, Level, Phylum, Class, Order, Family, Genus, Species) %>%
  distinct()

benthic_crosswalk = read_csv("Benthiccrosswalk.csv") %>%
  select(OrganismCode, Taxname, Lifestage) %>%
  left_join(crosswalk)

test = filter(benthic_crosswalk, is.na(Level))

write.csv(benthic_crosswalk, "Benthiccrosswalk.csv")
#ok, the hold-outs are all marine critters we don't much care about.

#the counts file includes zeros, CPUE does not.
#helpfully, it already has station info attached.
#ooo, but no water quality.

library(discretewq)

EMP = wq(Sources = "EMP", Start_year = 2010, End_year = 2020) %>%
  select(Source, Station, Microcystis, Chlorophyll, Secchi, Temperature, Conductivity, Salinity, TurbidityNTU, Year, Month)

unique(Benthic_CPUE$Station)%in% unique(EMP$Station)



Benthic = Benthic_CPUE %>%
  mutate(Station = str_remove(Station, "-.*"), Month = month(Date)) %>%
  left_join(EMP)

unique(Benthic$Station)%in% unique(EMP$Station)

unique(Benthic$Station)[which(!unique(Benthic$Station)%in% unique(EMP$Station))]

#so we are missing a few of the stations
#and apparently discreteq  doesn't have data for EMP psast 2020? That's odd.
Benthic = Benthic %>%
  select("Source", "Date", "OrganismCode", CPUE = "MeanCPUE", "Latitude", "Longitude", "Station",
         "Microcystis", "Chlorophyll",  "Secchi",   "Temperature" ,"Conductivity", "Salinity","TurbidityNTU") %>%
  mutate(TowType = "Ponar", sizeClass = "Macro") %>%
  left_join(benthic_crosswalk)%>%
  dplyr::filter(!is.na(.data$Level))%>% #Should remove all the summed categories in original dataset
  dplyr::mutate(Taxlifestage=paste(.data$Taxname, .data$Lifestage), #create variable for combo taxonomy x life stage
                SampleID=paste(.data$Source, .data$Station), #Create identifier for each sample
                TowType="Surface") %>%
  dplyr::select(-"OrganismCode")%>% #Remove LI taxa codes
  dtplyr::lazy_dt()%>% #Speed up code using dtplyr package that takes advantage of data.table speed
  dplyr::group_by(dplyr::across(-"CPUE"))%>%
  dplyr::summarise(CPUE=sum(.data$CPUE, na.rm=TRUE))%>%
  dplyr::ungroup()%>%
  tibble::as_tibble()




