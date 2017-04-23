library(dplyr)
library(XML)


mydatFinal = data.frame(matrix(ncol = 19))

for (i in 2003:2017){
  link = paste0("http://www.sports-reference.com/cbb/seasons/",i,"-ratings.html#ratings::none")
  mydat = readHTMLTable(link, header=T, which = 1, stringsAsFactors = F)
  mydat["Season"] = i
  names(mydatFinal) = names(mydat)
  mydatFinal = rbind(mydatFinal, mydat)
}

mydatFinal = mydatFinal[-1,]
head(mydatFinal)
# Get Rid of colnames between season tables
mydatFinal = mydatFinal %>% filter(mydatFinal$` ` != "SRS", mydatFinal$` ` != "?")
mydatFinal = mydatFinal %>% dplyr::select(Season, School, Conf, W, L, Pts, Opp, MOV, SOS, OSRS, DSRS, SRS)
mydatFinal = mydatFinal %>% filter(School != "School" | School != "")
#write.csv(mydatFinal,"./data/NabeelStats.csv")

# Join with Team IDs
names(mydatFinal)[2] = "Team_Name"

teams = read.csv("./data/Teams.csv")
tourn = read.csv("./data/TourneySeeds.csv")

mydatFinal = left_join(teams, mydatFinal, by = ("Team_Name"))


#Add Season team Seeds
tourn = as.data.frame(tourn %>% mutate(Seed = substring(Seed,2)) %>% select(Season, Team, Seed) %>% filter(Season >2002))
names(tourn)
names(mydatFinal)
mydatFinal = left_join(mydatFinal, tourn, by = c("Season" = "Season", "Team_Id" = "Team"))
mydatFinal$Seed = gsub('a','',mydatFinal$Seed) # remove 'a'
mydatFinal$Seed = gsub('b', '', mydatFinal$Seed) #remove 'b'
mydatFinal$Seed = as.numeric(mydatFinal$Seed) #convert to int
unique(mydatFinal$Seed)



head(mydatFinal)


write.csv(mydatFinal, "./data/TeamRatings.csv")


