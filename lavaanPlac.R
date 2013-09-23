placmod.cov <- '
PlacResp~TCQIATMean+OptIATMean+convaltcomp+LOTR
PlacResp~~PlacResp
TCQIATMean~~TCQIATMean
OptIATMean~~OptIATMean

LOTR~~LOTR'

placmod.direct <- '
PlacResp~Predisposition
Predisposition=~TCQIATMean+OptIATMean+LOTR+convaltcomp'
## PlacResp~~PlacResp
## TCQIATMean~~TCQIATMean
## OptIATMean~~OptIATMean
## convaltcomp~~convaltcomp
## LOTR~~LOTR'

placmod.first <- '
PlacResp~ImpExp+ExplExp
ImpExp=~TCQIATMean+OptIATMean
ExplExp=~LOTR+meanalt+meanconv
TCQIATMean~~TCQIATMean
OptIATMean~~OptIATMean
meanconv~~meanconv
meanalt~~meanalt
LOTR~~LOTR
LOTR~~meanalt+meanconv
TCQIATMean~~OptIATMean'
placmod.kirsch <- '
PlacResp~Exp
Exp=~LOTR+meanalt+meanconv
Exp~~Exp
LOTR~~LOTR
meanalt~~meanalt
meanconv~~meanconv
PlacResp~~PlacResp
'
## Expectancies=~LOTR+meanalt+meanconv+TCQIATMean+OptIATMean'-
placmod.cred.opt <- '
PlacResp~LOTR+convaltcomp
LOTR~OptIATMean
convaltcomp~TCQIATMean
PlacResp~~PlacResp
LOTR~~LOTR
## TCQIATMean~~TCQIATMean
convaltcomp~~convaltcomp
'
placmod.cred.opt.twofac <- '
PlacResp~Optimism+Credibility
Optimism=~OptIATMean+LOTR
Credibility=~convaltcomp+TCQIATMean
PlacResp~~PlacResp
LOTR~~LOTR
TCQIATMean~~TCQIATMean
convaltcomp~~convaltcomp
'
placmod.opt <- '
PlacResp~LOTR
LOTR~OptIATMean+meanconv+meanalt+TCQIATMean'
placmod.phys <- '
PlacResp~Exp+gsrmean
Exp=~TCQIATMean+OptIATMean+convaltcomp+LOTR
## PlacResp~~PlacResp
LOTR~~LOTR
TCQIATMean~~TCQIATMean
convaltcomp~~convaltcomp
OptIATMean~~OptIATMean
'
placmod.kirsch.twofac <- '
PlacResp~Expectancies
Expectancies=~TCQIATMean+OptIATMean+meanconv+meanalt
TCQIATMean~~TCQIATMean
OptIATMean~~OptIATMean
meanconv~~meanconv
meanalt~~meanalt
'

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

