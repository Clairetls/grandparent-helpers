pedigree<-read.csv('updated pedigree.csv', sep=';')
pedigree<-pedigree[,c('BirdID', "GeneticMother","GeneticFather","GenMumConfidence","GenDadConfidence")]


#filter for genetic confidence but depends on if theres an error after you run, 
#might get rid of individuals that have no parents
pedigree<-filter(pedigree, pedigree$GenDadConfidence>=80 & pedigree$GenMumConfidence>=80)

ped<-pedigree[,c("BirdID","GeneticMother",'GeneticFather')]
names(ped)<-c('ID', "Dam",'Sire')

ped<-fixPedigree(ped)

Rmatrix<-pedigreeStats(ped, includeA=T)
AMatrix <- Rmatrix$Amatrix #Extract A-matrix
write.csv(AMatrix, "AMatrix.csv", row.names = FALSE)

PedStatSum <- pedStatSummary(pedigreeStats(ped, retain = "ancestors", graphicalReport = "n"))
sink("PedStatSum.txt")
print(PedStatSum)
sink()
