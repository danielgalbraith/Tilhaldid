library(plyr)
library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(ordinal)
theme_set(theme_bw())

dd = read.csv(file="tilhaldid-data.csv")

# Reshape table
newtable = dd %>%
  gather(Speaker, Response, -X., -Sentence, -Gloss, -Neg.order, -PosAdv.order, -Aux2.PosAdv.order, -Obj.movt, -Obj.x.PolAdv, -Ptc.x.PolAdv, -Subj.gend, -Subj.num, -Obj.case, -Obj.gend, -Obj.num, -Nom.obj.agr, -Dat.subj.agr, -Acc.obj.agr) %>%
  filter(Response != "-") %>%
  mutate(Response = as.numeric(Response))

# Define "not in" function
'%!in%' <- function(x,y)!('%in%'(x,y))

# Isolate only non-bimodal sentences in table
no_bimodals = newtable %>%
  filter(Sentence %!in% c("Teimum man bókina ikki altíð hava dámað","Mær dáma bátarnar","Mær hevur ikki altíð dámað bókina","Okkum dáma bátarnar","Tykkum dáma bátarnir","Teimum tørva bátarnar"))

# Get rid of sentences with no negative adverb
agreement = no_bimodals %>%
  filter(Neg.order == "-") %>%
  droplevels()

# Aggregate agreement table
agr = no_bimodals %>%
  group_by(Obj.case, Subj.num) %>%
  summarise(Mean = mean(Response), CILow = ci.low(Response), CIHigh = ci.high(Response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

dodge = position_dodge(.9)

# Plot acceptability judgements by agreement
ggplot(agr, aes(x=Obj.case, y=Mean, fill=Subj.num)) +
  geom_bar(stat="identity", position=dodge) +
  geom_errorbar(aes(ymin=YMin, ymax=YMax),position=dodge,width=.25) + coord_cartesian(ylim=c(1,5))

# Run ordered logit regression model
model = clmm(as.factor(Response) ~ Obj.case + Subj.num + (1|Speaker) + (1|Sentence), data = newtable)
summary(model)

