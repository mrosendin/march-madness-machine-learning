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
mydatFinal = mydatFinal %>% filter(mydatFinal$` ` != "SRS", mydatFinal$` ` != "�")
mydatFinal = mydatFinal %>% dplyr::select(Season, School, Conf, W, L, Pts, Opp, MOV, SOS, OSRS, DSRS, SRS)
mydatFinal = mydatFinal %>% filter(School != "School" | School != "")
write.csv(mydatFinal,"nabeelStats.csv")

# Join with Team IDs
names(mydatFinal)[2] = "Team_Name"

teams = read.csv("teams.csv")
mydatFinal = left_join(teams, mydatFinal, by = ("Team_Name"))
unique(mydatFinal$Season)
write.csv(mydatFinal, "teamRatings.csv")


