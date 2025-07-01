
library(tidyverse)


#yolo bypass drift invertebrates
drift = read_csv("https://pasta.lternet.edu/package/data/eml/edi/818/4/4b89f0cd6b82fc3c68ff27c5b05b47de", guess_max = 10000) %>%
  mutate(LifeStage = case_when(is.na(LifeStage) ~ "NotDetermined",
                               LifeStage == "NA" ~ "NotDetermined",
                               LifeStage == "Nymphs" ~ "Larvae",
                               LifeStage %in% c("Emergent", "Emergents") ~ "Adults",
                               TRUE ~ LifeStage),
         Taxlife = paste(TaxonName, LifeStage))
driftbugs = read_csv( "https://pasta.lternet.edu/package/data/eml/edi/818/4/793364a600f9725af3f6b4ef333a79f3", guess_max = 10000)


#find duplicate enteries in the crosswalk
driftdups = group_by(driftbugs, TaxonName) %>%
  summarize(N = n()) %>%
  filter(N >1) %>%
  left_join(driftbugs)

#write.csv(driftdups, "DriftDuplicates.csv")

#look at all the TaxonName-LifeStage combinations that actually have data
driftbugsls = group_by(drift, TaxonName, LifeStage) %>%
  summarize(N = n(), total = sum(CPUE)) %>%
  filter(total != 0)

#look at the ones that have no counts
driftbugsZeros = group_by(drift, TaxonName, LifeStage) %>%
  summarize(N = n(), total = sum(CPUE)) %>%
  filter(total == 0)

#total samples per year
totalsampels = select(drift, event_id, WY) %>%
  distinct() %>%
  group_by(WY) %>%
  summarize(TotalSamples = n())

#how many zeros occur per year?
driftbugsZeros = group_by(drift, TaxonName, LifeStage, WY) %>%
  summarize(N = n(), total = sum(CPUE)) %>%
  filter(total == 0) %>%
  left_join(totalsampels)
#this is odd.

driftcounts = drift%>%
  mutate(IsZero = case_when(CPUE ==0 ~ 1,
                            TRUE ~ 0)) %>%
  group_by(WY) %>%
  summarize(ntaxa = length(unique(paste(TaxonName, LifeStage))), nsamples = length(unique(event_id)), nzeros = sum(IsZero))
#OK, so CPUE only equals zero in some year, bu tnot others? Super weird.

#focus on one of those years with zeros

drift2005 = filter(drift, WY ==2005)
drift2005zeros = filter(drift2005, CPUE ==0)

#huh, they are all from two samples and sort of random taxa.

drift2005zerotest = filter(drift2005, event_id %in% unique(drift2005zeros$event_id))

#the ones with zeros for the count for certain life stages all have an example of "not determined" for the same taxa

#try it for a different year
drift2011 = filter(drift, WY ==2011)
drift2011zeros = filter(drift2011, CPUE ==0)
drift2011zerotest = filter(drift2011, event_id %in% unique(drift2011zeros$event_id))
#this is totally different. Just zeros for all life stages
unique(drift2011zeros$event_id)
#but only four samples



#does the previous publication have the same issues?
YBFMPdrift_pkg_url2 <- paste0("https://pasta.lternet.edu/package/data/eml/edi/818/", (as.numeric(YBFMPdrift_latest_revision)-1))
YBFMPdrift_entities2 <- Tryer(n=3, fun=readLines, con=YBFMPdrift_pkg_url2, warn = FALSE)
YBFMPdrift_name_urls2 <- paste("https://pasta.lternet.edu/package/name/eml/edi/818",
                               (as.numeric(YBFMPdrift_latest_revision)-1), YBFMPdrift_entities2, sep="/")
names(YBFMPdrift_entities2) <- purrr::map_chr(YBFMPdrift_name_urls2, ~Tryer(n=3, fun=readLines, con=.x, warn = FALSE))

YBFMPdrift2<-paste0(YBFMPdrift_pkg_url2, "/", YBFMPdrift_entities2["Drift InvertebrateData"])

drift2 = read_csv(YBFMPdrift2, guess_max = 10000) %>%
  mutate(LifeStage = case_when(is.na(LifeStage) ~ "NotDetermined",
                               LifeStage == "NA" ~ "NotDetermined",
                               LifeStage == "Nymphs" ~ "Larvae",
                               LifeStage %in% c("Emergent", "Emergents") ~ "Adults",
                               TRUE ~ LifeStage),
         Taxlife = paste(TaxonName, LifeStage))

driftbugs2 = read_csv(paste0(YBFMPdrift_pkg_url2, "/", YBFMPdrift_entities2["Drift Invertebrate Taxonomy"]), guess_max = 10000)



driftdups2 = group_by(driftbugs2, TaxonName) %>%
  summarize(N = n()) %>%
  filter(N >1) %>%
  left_join(driftbugs2)

#write.csv(driftdups, "DriftDuplicates.csv")

driftbugsls2 = group_by(drift2, TaxonName, LifeStage) %>%
  summarize(N = n(), total = sum(CPUE))

driftbugsZeros2 = group_by(drift2, TaxonName, LifeStage) %>%
  summarize(N = n(), total = sum(CPUE)) %>%
  filter(total == 0)

driftcounts2 = drift2%>%
  mutate(IsZero = case_when(CPUE ==0 ~ 1,
                            TRUE ~ 0)) %>%
  group_by(WY) %>%
  summarize(ntaxa = length(unique(paste(TaxonName, LifeStage))), nsamples = length(unique(Datetime)), nzeros = sum(IsZero))
#No zeros in teh old data

unique(drift2$TaxonName) %in% unique(driftbugs2$TaxonName)

#taxa not in lookup table
unique(drift$TaxonName)[which(!unique(drift$TaxonName)%in% unique(driftbugs$TaxonName))]


