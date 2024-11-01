"%!in%" <-Negate( "%in%" )
library(tidyverse)
library(Hmisc)   #has %nin%
library(RODBC)
statuses<-read.csv("lastseen.csv", stringsAsFactors = F, sep=";")
#pull directly from db 

DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- "C:/db2/SeychellesWarbler1.11beta.accdb"
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

swdb<-odbcDriverConnect(PATH)


#birds that are helpers next year 
#birds are subordinates next year 
#but should compare to all birds? 

sbfp<-sqlQuery(swdb, 'SELECT sys_StatusByFieldPeriod.BirdID, sys_StatusByFieldPeriod.FieldPeriodID, sys_StatusByFieldPeriod.Status, tblFieldPeriodIDs.PeriodYear, tblFieldPeriodIDs.Island
FROM sys_StatusByFieldPeriod INNER JOIN tblFieldPeriodIDs ON sys_StatusByFieldPeriod.FieldPeriodID = tblFieldPeriodIDs.FieldPeriodID;
', stringsAsFactors=F)
statuses<-filter(sbfp, sbfp$Island=="CN")

breedgroupfp<-sqlQuery(swdb, "SELECT usys_qBreedStatusByFieldPeriod.BirdID, tblFieldPeriodIDs.PeriodEnd, usys_qBreedStatusByFieldPeriod.FieldPeriodID, usys_qBreedStatusByFieldPeriod.Status, Max(tblBreedStatus.BreedGroupID) AS BreedGroupID, tblBreedGroupLocation.TerritoryID
FROM ((usys_qBreedStatusByFieldPeriod INNER JOIN tblBreedStatus ON (usys_qBreedStatusByFieldPeriod.Status = tblBreedStatus.Status) AND (usys_qBreedStatusByFieldPeriod.BirdID = tblBreedStatus.BirdID)) INNER JOIN tblBreedGroupLocation ON (tblBreedGroupLocation.BreedGroupID = tblBreedStatus.BreedGroupID) AND (usys_qBreedStatusByFieldPeriod.FieldPeriodID = tblBreedGroupLocation.FieldPeriodID)) INNER JOIN tblFieldPeriodIDs ON usys_qBreedStatusByFieldPeriod.FieldPeriodID = tblFieldPeriodIDs.FieldPeriodID
GROUP BY usys_qBreedStatusByFieldPeriod.BirdID, tblFieldPeriodIDs.PeriodEnd, usys_qBreedStatusByFieldPeriod.FieldPeriodID, usys_qBreedStatusByFieldPeriod.Status, tblBreedGroupLocation.TerritoryID;
", stringsAsFactors=F)

birdid<-sqlQuery(swdb, 'SELECT tblBirdID.BirdID, tblBirdID.BirthDate
FROM tblBirdID;', stringsAsFactors=F)

statuses<-left_join(statuses, birdid, by="BirdID")
statuses$birthyear<-str_sub(statuses$BirthDate, 1,4)

statuses<-filter(statuses, statuses$PeriodYear>1996)
#check that year if they were not BrM if they have fitness or not 

#get population density before filtering anything 
statuses<-statuses%>%
  group_by(FieldPeriodID)%>%
  mutate(popden=length(unique(BirdID)))

popdenbyfs<-unique(statuses[,c("FieldPeriodID", 'popden')])

statuses$popden[statuses$popden<50]<-NA

#there are duplicates from this due to birds with double status 
statuses2<-left_join(statuses, breedgroupfp, by=c('BirdID', "FieldPeriodID", 'Status'))



#then you want to compare any datapoints that are helper or sub after brm or brf or bru
statuses2<-statuses2%>%
  mutate(newstat=case_when(Status == "BrF" ~ "Dom",
                           Status == "BrM" ~ "Dom",
                           Status == "BrU" ~ "Dom",
                           Status == "H" ~ "Helper",
                           Status=="TBrM"~"T",
                           Status=="TBrF"~"T",))

statuses2$newstat[statuses2$Status %!in% c("BrF",'BrM',"BrU",'H','TBrM',"TBrF")] <- "Sub"
statuses2$newstat[statuses2$Status=="NS"] <- "Dead"


no<-c("EGG","CH","FL","XF")
statuses2<-statuses2%>%filter(Status %!in% no)

########
statuses2<-statuses2%>%filter(newstat!="Dead")

changes<-statuses2 %>% arrange(BirdID, PeriodYear) %>% 
  group_by(BirdID)%>%
  mutate(next_status = lead(newstat), next_year = lead(PeriodYear)
        ,next_breedgroup=lead(BreedGroupID))

deposed<-changes%>%
  filter(newstat=="Dom" & (next_status=="Helper"|next_status=="Sub"))

#rows are from previous season but makes sense because they decide somewhere in between
#logic can be that conditions from previous season dictate behaviour of next season 




#check for birds with gaps between br and helper/ sub 
nexthelper<-changes%>%filter(next_status=="Helper")%>%filter(BirdID %!in% deposed$BirdID)

nextsub<-changes%>%filter(next_status=="Sub")%>%filter(BirdID %!in% deposed$BirdID)

subtohelp<-changes%>%filter(newstat=="Sub"& next_status=="Helper")

Ttohelp<-changes%>%filter(newstat=="T" & (next_status=="Sub"|next_status=="Helper"))
#none from tbr to helper 

addbirds<-c(476,1042,1152,1158,1271,1660,5125,5319,5607,5644,6026,6161,6558)
toadd<-changes%>%filter(BirdID %in% addbirds)

toadd<-toadd[c(16,49,57,63,78,97,119,123,135,157,181,189,205,216),]

#take the row where next status is helper, and put conditions then. 
deposed<-rbind(deposed, toadd)
deposed<-deposed%>%
  mutate(change=case_when(next_status=="Helper" ~ "br2help",
                          next_status=="Sub" ~ "br2sub"))


#there are bird IDs repeated.


nochange<-changes%>%filter(BirdID %!in% deposed$BirdID)
nochange<-nochange%>%filter(!is.na(next_status))%>%group_by(BirdID)%>%
  filter(FieldPeriodID==max(FieldPeriodID)&PeriodYear==max(PeriodYear))



#birds that were never dominant 
#for each bird, find if they were ever dominant, if no, return bird ID 

nondom<-vector()
for(i in unique(changes$BirdID)){
  onebird<-changes%>%filter(BirdID==i)
  status<-onebird%>%filter(newstat=="Dom")
  if(nrow(status)==0){add<-i}else{add<-NA}
  nondom<-c(add, nondom)
}

#check and need to remove never dominant birds from data 
nondom<-na.omit(nondom)
dom<-changes%>%filter(BirdID %!in% nondom)

alwaysdom<-dom%>%filter(BirdID %!in% deposed$BirdID)
#alwaysdom<-filter(alwaysdom, !is.na(alwaysdom$next_status))
alwaysdom<-as.data.frame(alwaysdom)
#create non-deposed 
#for each bird get the most recent status change 
domdata<-data.frame()
for(i in unique(alwaysdom$BirdID)){
  onebird<-filter(alwaysdom, alwaysdom$BirdID==i)
  if(nrow(onebird)==1){maxyear<-onebird}else{
    maxyear<-onebird[(nrow(onebird)-1),]
  }
  domdata<-rbind(domdata,maxyear)
}

domdata$change<-'lifebr'


combinedchanges<-rbind(deposed,domdata)
duplicated<-combinedchanges%>%
  group_by(BirdID)%>%
  summarise(count=n())
duplicated<-subset(duplicated, duplicated$count>1)
duplicated<-unique(duplicated$BirdID)

discard<-combinedchanges%>%filter(BirdID %in% duplicated)%>%arrange(by=BirdID)
discard<-discard[c(1,3,5,7,9,10,12,14,16,18,20,23,24,27,28,30,31,33,34,36,38,40,43,44,
                   46,48,51,52),]


#there are duplicates from when birds flipped back and forth between 
#brf and H or brf to sub or sub to H 

#do i remove or not 

nondupchanges<-anti_join(combinedchanges, discard)
# nondupchanges<-combinedchanges[-c(5,29,31,33,38,39,41,49,56,60,61,66,76,91,116,119,
#                                   120,124,128,147,150,159,162,168,172,175,181,223,226),]


#insect abundance
bugs<-readxl::read_excel('usys_qTerrQualityInsectsPerDM2.xlsx')
bugs<-filter(bugs, bugs$Island=="CN")
#bugs[,c(5:47)]<-as.numeric(bugs[,c(5:47)])

bugs$invertsum<-rowSums(bugs[,c(29:47)], na.rm=T)
total_invert<-bugs[,c("Island","Year","FieldPeriodID","Location","invertsum")]
total_invert<-total_invert%>%
  group_by(Year, FieldPeriodID)%>%
  mutate(avg_invert=mean(invertsum))
total_invert$occasionyear<-total_invert$Year
total_invert<-total_invert[,c("occasionyear", "FieldPeriodID","avg_invert")]
total_invert<-unique(total_invert)

write.csv(total_invert, "mean_insect_new.csv")

combinedchanges<-left_join(combinedchanges, total_invert, by="FieldPeriodID")
nondupchanges<-left_join(nondupchanges, total_invert, by="FieldPeriodID")



#add age 
combinedchanges$birthyear<-as.numeric(combinedchanges$birthyear)
nondupchanges$birthyear<-as.numeric(nondupchanges$birthyear)

combinedchanges$age<-combinedchanges$PeriodYear-combinedchanges$birthyear
nondupchanges$age<-nondupchanges$PeriodYear-nondupchanges$birthyear


#add sex 
sexestimates<-read.csv('C:/PhD/Data/sys_SexEstimates.csv', sep=';',stringsAsFactors = F)
sexestimates<-sexestimates[,c(1,2)]
combinedchanges<-left_join(combinedchanges, sexestimates, by='BirdID')
nondupchanges<-left_join(nondupchanges, sexestimates, by='BirdID')


# write.csv(nondupchanges, 'gphelp_nodup.csv')
# write.csv(combinedchanges, "gphelp_dup.csv")

nondupchanges<-read.csv('gphelp_nodup.csv')
combinedchanges<-read.csv('gphelp_dup.csv')

combinedchanges<-combinedchanges[,-c(1)]
nondupchanges<-nondupchanges[,-c(1)]


#breedgroup info 

ccbrgp<-combinedchanges[,c("BirdID","next_breedgroup")]
# ndcbrgp<-nondupchanges[,c("BirdID","next_breedgroup")]

#for each breed group in that list, return the brf and brm of the group

ccinfo<-subset(breedgroupfp, breedgroupfp$BreedGroupID %in% ccbrgp$next_breedgroup)
# ndcinfo<-subset(breedgroupfp, breedgroupfp$BreedGroupID %in% ndcbrgp$next_breedgroup)

bginfo<-data.frame()
for(i in unique(ccbrgp$next_breedgroup)){
  onebg<-filter(ccinfo, ccinfo$BreedGroupID==i)
  brf<-onebg[onebg$Status=="BrF",c('BirdID')]
  brm<-onebg[onebg$Status=="BrM",c('BirdID')]
  if(length(brf)==0){brf<-NA}
  if(length(brm)==0){brm<-NA}
  brfm<-data.frame(next_breedgroup=i, BrF=brf, BrM=brm)
  bginfo<-rbind(bginfo, brfm)
}


huh<-bginfo%>%group_by(next_breedgroup)%>%
  summarise(count=length(next_breedgroup))
bginfo<-bginfo%>%filter(next_breedgroup %!in% c(3761, 5125))

combinedchanges<-left_join(combinedchanges, bginfo, by="next_breedgroup")

nondupchanges<-left_join(nondupchanges, bginfo, by="next_breedgroup")


Rindivcc<-combinedchanges[,c('BirdID', "BrF", "BrM")]
Rindivndc<-nondupchanges[,c('BirdID', "BrF", "BrM")]

Rindivcc<-na.omit(Rindivcc)
Rindivndc<-na.omit(Rindivndc)
Rindivcc<-unique(Rindivcc)
Rindivndc<-unique(Rindivndc)

#get relatedness 

pedigree<-read.csv('updated pedigree.csv', sep=';')
pedigree<-pedigree[,c('BirdID', "GeneticMother","GeneticFather","GenMumConfidence","GenDadConfidence")]

pedigree<-filter(pedigree, pedigree$GenDadConfidence>=80 & pedigree$GenMumConfidence>=80)

pedigree<-unique(pedigree)
pedigree<-pedigree[,c("BirdID", "GeneticMother","GeneticFather")]



# pedigree<-filter(pedigree, !is.na(pedigree$sex))
# 
# rtable<-coeffTable(pedigree)

#2023? 
# url <- "https://cran.r-project.org/src/contrib/Archive/pedantics/pedantics_1.7.tar.gz"
# pkgFile <- "pedantics_1.7.tar.gz"
# download.file(url = url, destfile = pkgFile)
# install.packages('pedantics_1.7.tar')
# library()

install_github("JulienGAMartin/pedtricks")
library(pedtricks)

names(pedigree)<-c('id','dam','sire')


# fix_ped(pedigree)

pedinfo<-ped_stats(pedigree, includeA = T)

Amatrix<-as.matrix(pedinfo[["Amatrix"]]) 
Amatrix<-as.data.frame(Amatrix)

problematicbirds<-which(Amatrix >1 , arr.ind = T)
which(Amatrix['526',]>0.5)


Amatrix[Amatrix>1]<-1
range(Amatrix)

# Amatrix<-read.csv('Amatrix.csv')

# pedigrees
# Rindivcc[i,c(2)]
# test<-c(i,Rindivcc[i,c(2)])
# length(test)


# rownames(Amatrix)<-colnames(Amatrix)

trial<-data.frame()
for(i in 1:nrow(Rindivcc)){
  c1<-Rindivcc$BirdID[i]
  c2<-Rindivcc$BrF[i]
  c3<-Rindivcc$BrM[i]
  withbrf<-Amatrix[paste(c1),paste(c2)]
  withbrm<-Amatrix[paste(c1),paste(c3)]
  if(length(withbrf)==0){withbrf<-NA}
  if(length(withbrm)==0){withbrm<-NA}
  onerow<-data.frame(BirdID=c1, withbrf=withbrf, withbrm=withbrm, BrF=c2, BrM=c3)
  trial<-rbind(trial,onerow)
}

trial<-unique(trial)
combinedchanges<-left_join(combinedchanges,trial, by=c('BirdID',"BrF","BrM"))
nondupchanges<-left_join(nondupchanges, trial, by=c('BirdID',"BrF","BrM"))



#bodymass 
bodymass<-read.csv('C:/PhD/Data/modelling dfs for habrok/Dataframes 28_5_24/physio_28_5.csv')
bodymass<-bodymass[,c("BirdID","FieldPeriodID","BodyMass","RightTarsus","occasionyear")]

test<-left_join(nondupchanges, bodymass, by=c('BirdID',"FieldPeriodID"))

test<-test%>%
  group_by(BirdID, FieldPeriodID) %>%
  mutate(
    BodyMass = mean(BodyMass, na.rm = TRUE),
    RightTarsus = mean(RightTarsus, na.rm = TRUE)
  )

test<-unique(test)


nondupchanges<-test

test2<-left_join(combinedchanges, bodymass, by=c('BirdID','FieldPeriodID'))

test2<-test2%>%
  group_by(BirdID, FieldPeriodID) %>%
  mutate(
    BodyMass = mean(BodyMass, na.rm = TRUE),
    RightTarsus = mean(RightTarsus, na.rm = TRUE)
  )

test2<-unique(test2)

combinedchanges<-test2

#ok done 

write.csv(combinedchanges, 'combinedchanges.csv')
write.csv(nondupchanges, 'nondupchanges.csv')


library(lme4)

combinedchanges$bodycondi<-combinedchanges$BodyMass/combinedchanges$RightTarsus
nondupchanges$bodycondi<-nondupchanges$BodyMass/nondupchanges$RightTarsus

combinedchanges<-combinedchanges%>%
  mutate(help=case_when(change=="br2help"~1,
                        change!='br2help'~0))

nondupchanges<-nondupchanges%>%
  mutate(help=case_when(change=="br2help"~1,
                        change!='br2help'~0))


combinedchanges<-as.data.frame(combinedchanges)
nondupchanges<-as.data.frame(nondupchanges)


library(GLMMadaptive)
ccjustdeposed$SexEstimate<-as.factor(ccjustdeposed$SexEstimate)
ccjustdeposed$occasionyear.x<-as.factor(ccjustdeposed$occasionyear.x)


combinedchanges<-read.csv('combinedchanges.csv')
nondupchanges<-read.csv('nondupchanges.csv')
ccnonhelp<-filter(combinedchanges,combinedchanges$change!="br2help")
ccsub<-filter(combinedchanges, combinedchanges$change=='br2sub')
ccnonhelp2<-na.omit(ccnonhelp)
cchelp<-filter(combinedchanges,combinedchanges$change=='br2help')

ndcsub<-filter(nondupchanges,nondupchanges$change=="br2sub")
# ndcsub<-na.omit(ndcsub)
ndchelp<-filter(nondupchanges,nondupchanges$change=="br2help")


set.seed(1)
onesample<-sample_n(ccnonhelp, size=35, replace=F)
ccsubsample<-sample_n(ccsub, size=35, replace=F)
ndcsample1<-sample_n(ndcsub, size=35, replace=F)

ndcdf1<-rbind(ndchelp, ndcsample1)
ndcdf1$occasionyear.x<-as.factor(ndcdf1$occasionyear.x)
ndcdf1$SexEstimate<-as.factor(ndcdf1$SexEstimate)

ccdf1<-rbind(cchelp,onesample)
ccdf2<-rbind(cchelp, ccsubsample)

ccdf1$occasionyear.x<-as.factor(ccdf1$occasionyear.x)
ccdf1$SexEstimate<-as.factor(ccdf1$SexEstimate)
ccdf2$occasionyear.x<-as.factor(ccdf2$occasionyear.x)
ccdf2$SexEstimate<-as.factor(ccdf2$SexEstimate)


test<-filter(combinedchanges, !is.na(combinedchanges$bodycondi))

#if a student needs body condition, they will have to find another indicator 
#maybe terminal year 
# ccmod1<-glm(help~withbrf+withbrm+popden+avg_invert+age+popden:avg_invert,
#                   data=ccdf1,family = binomial(link='logit'))
# 
# summary(ccmod1)

ccmod2<-glm(help~withbrf+withbrm+popden+avg_invert+age+popden:avg_invert,
            data=ccdf2,family = binomial(link='logit'))
summary(ccmod2)

# ccmod2data <- ggpredict(ccmod2, terms=c("popden [all]","avg_invert"))
# plot(ccmod2data)

ccmod3<-glm(help~withbrf+withbrm+popden+avg_invert+age,
            data=ccdf2,family = binomial(link='logit'))
summary(ccmod3)

ndcmod1<-glm(help~withbrf+withbrm+popden+avg_invert+age+popden:avg_invert,
             data=ndcdf1,family = binomial(link='logit'))
summary(ndcmod1)

library(DHARMa)
residccmod2<-testResiduals(ccmod2)
residndcmod1<-testResiduals(ndcmod1)

ccsr<-simulateResiduals(ccmod1)
ndcsr<-simulateResiduals(ndcmod1)

cc2sr<-simulateResiduals(ccmod2)

plot(ccsr)
plot(ndcsr)
plot(cc2sr)
plot(ccmod1)


#resids are okay for these models 

plotdf1<-ccdf1[,c('BirdID', "withbrf",'withbrm',"help")]

plotdf2<-ccdf2[,c('BirdID', "withbrf",'withbrm',"help")]

library(sjPlot)
tab_model(ccmod2)

library(data.table)
plotdf1<-as.data.table(plotdf1)
plotdf2<-as.data.table(plotdf2)

plotdf1<-melt(plotdf1, id.vars=c("BirdID",'help'),measure.vars=c('withbrf','withbrm'), variable.name="r_type")
plotdf2<-melt(plotdf2, id.vars=c("BirdID",'help'),measure.vars=c('withbrf','withbrm'), variable.name="r_type")

plotdf1<-as.data.frame(plotdf1)
plotdf2<-as.data.frame(plotdf2)


ggplot(plotdf1, aes(y=help, x=value, colour = r_type))+geom_point()+
  geom_smooth(method='glm', method.args=list(family='binomial'))+xlab('Relatedness')

ggplot(plotdf2, aes(y=help, x=value, colour = r_type))+geom_point()+
  geom_smooth(method='glm', method.args=list(family='binomial'))+xlab('Relatedness')



#data too zero inflated to compare lifetime breeders vs subords vs grandparenthelper
#now i have randomly subsetted equiv rows 
#for masters project or future work, 
#use bootstrapping, model averaging or cross validation
#look up raph's code 
