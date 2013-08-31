placmod1 <- '
PlacResp~TCQIATMean+OptIATMean+convaltcomp+LOTR+Age
## PlacResp~~PlacResp
## TCQIATMean~~TCQIATMean
## OptIATMean~~OptIATMean
## ## Gender~~Gender
## convaltcomp~~convaltcomp
## LOTR~~LOTR
## Age~~Age
## TCQIATMean~~OptIATMean+Gender+convaltcomp+LOTR+Age
'
placmod.first <- '
PlacResp~ImpExp+ExplExp+Age
ImpExp=~TCQIATMean+OptIATMean
ExplExp=~LOTR+meanalt+meanconv'
## Age~~Age
## TCQIATMean~~TCQIATMean
## OptIATMean~~OptIATMean
## LOTR~~LOTR
## meanalt~~meanalt
## meanconv~~meanconv'
placmod.kirsch <- '
PlacResp~Expectancies
Expectancies=~LOTR+meanalt+meanconv+TCQIATMean+OptIATMean'
placmod.cred.opt <- '
PlacResp~Optimism+Credibility+Demo
Optimism=~LOTR+OptIATMean
Credibility=~meanconv+meanalt+TCQIATMean
Demo=~Age+Gender'
placmod.opt <- '
PlacResp~Optimism
Optimism=~LOTR+OptIATMean+meanconv+meanalt+meanconv+TCQIATMean'
placmod.phys <- '
PlacResp~ImpExp+ExplExp+gsr'



placmod2 <- '
PlacResp~TCQIATMean+OptIATMean+convaltcomp+LOTR+Age'
expimpdirect <- '
Expectancies=~TCQIATMean+OptIATMean+MAAS+meanconv+meanalt
TCQIATMean~~TCQIATMean
OptIATMean~~OptIATMean
MAAS~~MAAS
meanconv~~meanconv
meanalt~~meanalt
## TCQIATMean~~OptIATMean+MAAS+meanconv+meanalt
'
expimptwofac <- '
ImpExp=~TCQIATMean+OptIATMean
ExpExp=~meanconv+meanalt+MAAS
ImpExp~~ImpExp
ImpExp~~ExpExp
TCQIATMean~~TCQIATMean
OptIATMean~~OptIATMean
MAAS~~MAAS
meanconv~~meanconv
meanalt~~meanalt'

