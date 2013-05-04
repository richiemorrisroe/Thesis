min1<-rbind(phys.2346[1:60,], deparse.level=2)
min2<-rbind(phys.2346[61:120,], deparse.level=2)
min3<-rbind(phys.2346[121:180,], deparse.level=2)
min4<-rbind(phys.2346[181:240,], deparse.level=2)
min5<-rbind(phys.2346[241:300,], deparse.level=2)
min6<-rbind(phys.2346[301:360,], deparse.level=2)
min7<-rbind(phys.2346[361:420,], deparse.level=2)
min8<-rbind(phys.2346[421:480,], deparse.level=2)
min9<-rbind(phys.2346[481:540,], deparse.level=2)
min10<-rbind(phys.2346[541:600,], deparse.level=2)
min11<-rbind(phys.2346[601:660,], deparse.level=2)
min12<-rbind(phys.2346[661:731,], deparse.level=2)
min13<-rbind(phys.2346[721:780,], deparse.level=2)
min14<-rbind(phys.2346[781:840,], deparse.level=2)
min15<-rbind(phys.2346[841:900,], deparse.level=2)
min16<-rbind(phys.2346[901:960,], deparse.level=2)
min17<-rbind(phys.2346[961:1020,], deparse.level=2)
min18<-rbind(phys.2346[1021:1080,], deparse.level=2)
min19<-rbind(phys.2346[1081:1140,], deparse.level=2)
min20<-rbind(phys.2346[1141:1200,], deparse.level=2)
min21<-rbind(phys.2346[1201:1267,], deparse.level=2)



min22<-rbind(phys.1980[1261:1320,], deparse.level=2)
min23<-rbind(phys.1980[1321:1380,], deparse.level=2)
min24<-rbind(phys.1980[1381:1440,], deparse.level=2)
min25<-rbind(phys.1980[1441:1500,], deparse.level=2)
min26<-rbind(phys.1980[1501:1560,], deparse.level=2)
min27<-rbind(phys.1980[1561:1620,], deparse.level=2)
min28<-rbind(phys.1980[1621:1680,], deparse.level=2)
min29<-rbind(phys.1980[1681:1740,], deparse.level=2)
min30<-rbind(phys.1980[1741:1800,], deparse.level=2)
min31<-rbind(phys.1980[1801:1860,], deparse.level=2)
min32<-rbind(phys.1980[1861:1920,], deparse.level=2)
min33<-rbind(phys.1980[1921:1980,], deparse.level=2)
min34<-rbind(phys.1980[1981:2040,], deparse.level=2)
min35<-rbind(phys.1980[2041:2100,], deparse.level=2)
min36<-rbind(phys.1980[2101:2160,], deparse.level=2)
min37<-rbind(phys.1980[2161:2220,], deparse.level=2)
min38<-rbind(phys.1980[2221:2280,], deparse.level=2)
min39<-rbind(phys.1980[2281:2340,], deparse.level=2)
min40<-rbind(phys.1980[2341:2400,], deparse.level=2)
min41<-rbind(phys.1980[2401:2460,], deparse.level=2)
min42<-rbind(phys.1980[2461:2520,], deparse.level=2)
min43<-rbind(phys.1980[2521:2580,], deparse.level=2)
min44<-rbind(phys.1980[2581:2640,], deparse.level=2)
min45<-rbind(phys.1980[2641:2700,], deparse.level=2)
min46<-rbind(phys.1980[2701:2764,], deparse.level=2)


sum.min1<-lapply(min1, sum)
mean.min1<-lapply(min1, mean)
sum.min2<-lapply(min2, sum)
mean.min2<-lapply(min2, mean)
sum.min3<-lapply(min3, sum)
mean.min3<-lapply(min3, mean)
sum.min4<-lapply(min4, sum)
mean.min4<-lapply(min4, mean)
sum.min5<-lapply(min5, sum)
mean.min5<-lapply(min5, mean)
sum.min6<-lapply(min6, sum)
mean.min6<-lapply(min6, mean)
sum.min7<-lapply(min7, sum)
mean.min7<-lapply(min7, mean)
sum.min8<-lapply(min8, sum)
mean.min8<-lapply(min8, mean)
sum.min9<-lapply(min9, sum)
mean.min9<-lapply(min9, mean)
sum.min10<-lapply(min10, sum)
mean.min10<-lapply(min10, mean)
sum.min11<-lapply(min11, sum)
mean.min11<-lapply(min11, mean)
sum.min12<-lapply(min12, sum)
mean.min12<-lapply(min12, mean)
sum.min13<-lapply(min13, sum)
mean.min13<-lapply(min13, mean)
sum.min14<-lapply(min14, sum)
mean.min14<-lapply(min14, mean)
sum.min15<-lapply(min15, sum)
mean.min15<-lapply(min15, mean)
sum.min16<-lapply(min16, sum)
mean.min16<-lapply(min16, mean)
sum.min17<-lapply(min17, sum)
mean.min17<-lapply(min17, mean)
sum.min18<-lapply(min18, sum)
mean.min18<-lapply(min18, mean)
sum.min19<-lapply(min19, sum)
mean.min19<-lapply(min19, mean)
sum.min20<-lapply(min20, sum)
mean.min20<-lapply(min20, mean)
sum.min21<-lapply(min21, sum)
mean.min21<-lapply(min21, mean)


sum.min22<-lapply(min22, sum)
mean.min22<-lapply(min22, mean)
sum.min23<-lapply(min23, sum)
mean.min23<-lapply(min23, mean)
sum.min24<-lapply(min24, sum)
mean.min24<-lapply(min24, mean)
sum.min25<-lapply(min25, sum)
mean.min25<-lapply(min25, mean)
sum.min26<-lapply(min26, sum)
mean.min26<-lapply(min26, mean)
sum.min27<-lapply(min27, sum)
mean.min27<-lapply(min27, mean)
sum.min28<-lapply(min28, sum)
mean.min28<-lapply(min28, mean)
sum.min29<-lapply(min29, sum)
mean.min29<-lapply(min29, mean)
sum.min30<-lapply(min30, sum)
mean.min30<-lapply(min30, mean)
sum.min31<-lapply(min31, sum)
mean.min31<-lapply(min31, mean)
sum.min32<-lapply(min32, sum)
mean.min32<-lapply(min32, mean)
sum.min33<-lapply(min33, sum)
mean.min33<-lapply(min33, mean)
sum.min34<-lapply(min34, sum)
mean.min34<-lapply(min34, mean)
sum.min35<-lapply(min35, sum)
mean.min35<-lapply(min35, mean)
sum.min36<-lapply(min36, sum)
mean.min36<-lapply(min36, mean)
sum.min37<-lapply(min37, sum)
mean.min37<-lapply(min37, mean)
sum.min38<-lapply(min38, sum)
mean.min38<-lapply(min38, mean)
sum.min39<-lapply(min39, sum)
mean.min39<-lapply(min39, mean)
sum.min40<-lapply(min40, sum)
mean.min40<-lapply(min40, mean)
sum.min41<-lapply(min41, sum)
mean.min41<-lapply(min41, mean)
sum.min42<-lapply(min42, sum)
mean.min42<-lapply(min42, mean)
sum.min43<-lapply(min43, sum)
mean.min43<-lapply(min43, mean)
sum.min44<-lapply(min44, sum)
mean.min44<-lapply(min44, mean)
sum.min45<-lapply(min45, sum)
mean.min45<-lapply(min45, mean)
sum.min46<-lapply(min46, sum)
mean.min46<-lapply(min46, mean)



min1.GSR<-mean.min1$GSR
min1.BP<-sum.min1$BP
min1.VAS<-mean.min1$VAS
min1.comp<-cbind(min1.GSR, min1.BP, min1.VAS)
min2.GSR<-mean.min2$GSR
min2.BP<-sum.min2$BP
min2.VAS<-mean.min2$VAS
min2.comp<-cbind(min2.GSR, min2.BP, min2.VAS)
min3.GSR<-mean.min3$GSR
min3.BP<-sum.min3$BP
min3.VAS<-mean.min3$VAS
min3.comp<-cbind(min3.GSR, min3.BP, min3.VAS)
min4.GSR<-mean.min4$GSR
min4.BP<-sum.min4$BP
min4.VAS<-mean.min4$VAS
min4.comp<-cbind(min4.GSR, min4.BP, min4.VAS)
min5.GSR<-mean.min5$GSR
min5.BP<-sum.min5$BP
min5.VAS<-mean.min5$VAS
min5.comp<-cbind(min5.GSR, min5.BP, min5.VAS)
min6.GSR<-mean.min6$GSR
min6.BP<-sum.min6$BP
min6.VAS<-mean.min6$VAS
min6.comp<-cbind(min6.GSR, min6.BP, min6.VAS)
min7.GSR<-mean.min7$GSR
min7.BP<-sum.min7$BP
min7.VAS<-mean.min7$VAS
min7.comp<-cbind(min7.GSR, min7.BP, min7.VAS)
min8.GSR<-mean.min8$GSR
min8.BP<-sum.min8$BP
min8.VAS<-mean.min8$VAS
min8.comp<-cbind(min8.GSR, min8.BP, min8.VAS)
min9.GSR<-mean.min9$GSR
min9.BP<-sum.min9$BP
min9.VAS<-mean.min9$VAS
min9.comp<-cbind(min9.GSR, min9.BP, min9.VAS)
min10.GSR<-mean.min10$GSR
min10.BP<-sum.min10$BP
min10.VAS<-mean.min10$VAS
min10.comp<-cbind(min10.GSR, min10.BP, min10.VAS)
min11.GSR<-mean.min11$GSR
min11.BP<-sum.min11$BP
min11.VAS<-mean.min11$VAS
min11.comp<-cbind(min11.GSR, min11.BP, min11.VAS)
min12.GSR<-mean.min12$GSR
min12.BP<-sum.min12$BP
min12.VAS<-mean.min12$VAS
min12.comp<-cbind(min12.GSR, min12.BP, min12.VAS)
min13.GSR<-mean.min13$GSR
min13.BP<-sum.min13$BP
min13.VAS<-mean.min13$VAS
min13.comp<-cbind(min13.GSR, min13.BP, min13.VAS)
min14.GSR<-mean.min14$GSR
min14.BP<-sum.min14$BP
min14.VAS<-mean.min14$VAS
min14.comp<-cbind(min14.GSR, min14.BP, min14.VAS)
min15.GSR<-mean.min15$GSR
min15.BP<-sum.min15$BP
min15.VAS<-mean.min15$VAS
min15.comp<-cbind(min15.GSR, min15.BP, min15.VAS)
min16.GSR<-mean.min16$GSR
min16.BP<-sum.min16$BP
min16.VAS<-mean.min16$VAS
min16.comp<-cbind(min16.GSR, min16.BP, min16.VAS)
min17.GSR<-mean.min17$GSR
min17.BP<-sum.min17$BP
min17.VAS<-mean.min17$VAS
min17.comp<-cbind(min17.GSR, min17.BP, min17.VAS)
min18.GSR<-mean.min18$GSR
min18.BP<-sum.min18$BP
min18.VAS<-mean.min18$VAS
min18.comp<-cbind(min18.GSR, min18.BP, min18.VAS)
min19.GSR<-mean.min19$GSR
min19.BP<-sum.min19$BP
min19.VAS<-mean.min19$VAS
min19.comp<-cbind(min19.GSR, min19.BP, min19.VAS)
min20.GSR<-mean.min20$GSR
min20.BP<-sum.min20$BP
min20.VAS<-mean.min20$VAS
min20.comp<-cbind(min20.GSR, min20.BP, min20.VAS)
min21.GSR<-mean.min21$GSR
min21.BP<-sum.min21$BP
min21.VAS<-mean.min21$VAS
min21.comp<-cbind(min21.GSR, min21.BP, min21.VAS)

min22.GSR<-mean.min22$GSR
min22.BP<-sum.min22$BP
min22.VAS<-mean.min22$VAS
min22.comp<-cbind(min22.GSR, min22.BP, min22.VAS)
min23.GSR<-mean.min23$GSR
min23.BP<-sum.min23$BP
min23.VAS<-mean.min23$VAS
min23.comp<-cbind(min23.GSR, min23.BP, min23.VAS)
min24.GSR<-mean.min24$GSR
min24.BP<-sum.min24$BP
min24.VAS<-mean.min24$VAS
min24.comp<-cbind(min24.GSR, min24.BP, min24.VAS)
min25.GSR<-mean.min25$GSR
min25.BP<-sum.min25$BP
min25.VAS<-mean.min25$VAS
min25.comp<-cbind(min25.GSR, min25.BP, min25.VAS)
min26.GSR<-mean.min26$GSR
min26.BP<-sum.min26$BP
min26.VAS<-mean.min26$VAS
min26.comp<-cbind(min26.GSR, min26.BP, min26.VAS)
min27.GSR<-mean.min27$GSR
min27.BP<-sum.min27$BP
min27.VAS<-mean.min27$VAS
min27.comp<-cbind(min27.GSR, min27.BP, min27.VAS)
min28.GSR<-mean.min28$GSR
min28.BP<-sum.min28$BP
min28.VAS<-mean.min28$VAS
min28.comp<-cbind(min28.GSR, min28.BP, min28.VAS)
min29.GSR<-mean.min29$GSR
min29.BP<-sum.min29$BP
min29.VAS<-mean.min29$VAS
min29.comp<-cbind(min29.GSR, min29.BP, min29.VAS)
min30.GSR<-mean.min30$GSR
min30.BP<-sum.min30$BP
min30.VAS<-mean.min30$VAS
min30.comp<-cbind(min30.GSR, min30.BP, min30.VAS)
min31.GSR<-mean.min31$GSR
min31.BP<-sum.min31$BP
min31.VAS<-mean.min31$VAS
min31.comp<-cbind(min31.GSR, min31.BP, min31.VAS)
min32.GSR<-mean.min32$GSR
min32.BP<-sum.min32$BP
min32.VAS<-mean.min32$VAS
min32.comp<-cbind(min32.GSR, min32.BP, min32.VAS)
min33.GSR<-mean.min33$GSR
min33.BP<-sum.min33$BP
min33.VAS<-mean.min33$VAS
min33.comp<-cbind(min33.GSR, min33.BP, min33.VAS)
min34.GSR<-mean.min34$GSR
min34.BP<-sum.min34$BP
min34.VAS<-mean.min34$VAS
min34.comp<-cbind(min34.GSR, min34.BP, min34.VAS)
min35.GSR<-mean.min35$GSR
min35.BP<-sum.min35$BP
min35.VAS<-mean.min35$VAS
min35.comp<-cbind(min35.GSR, min35.BP, min35.VAS)
min36.GSR<-mean.min36$GSR
min36.BP<-sum.min36$BP
min36.VAS<-mean.min36$VAS
min36.comp<-cbind(min36.GSR, min36.BP, min36.VAS)
min37.GSR<-mean.min37$GSR
min37.BP<-sum.min37$BP
min37.VAS<-mean.min37$VAS
min37.comp<-cbind(min37.GSR, min37.BP, min37.VAS)
min38.GSR<-mean.min38$GSR
min38.BP<-sum.min38$BP
min38.VAS<-mean.min38$VAS
min38.comp<-cbind(min38.GSR, min38.BP, min38.VAS)
min39.GSR<-mean.min39$GSR
min39.BP<-sum.min39$BP
min39.VAS<-mean.min39$VAS
min39.comp<-cbind(min39.GSR, min39.BP, min39.VAS)
min40.GSR<-mean.min40$GSR
min40.BP<-sum.min40$BP
min40.VAS<-mean.min40$VAS
min40.comp<-cbind(min40.GSR, min40.BP, min40.VAS)
min41.GSR<-mean.min41$GSR
min41.BP<-sum.min41$BP
min41.VAS<-mean.min41$VAS
min41.comp<-cbind(min41.GSR, min41.BP, min41.VAS)
min42.GSR<-mean.min42$GSR
min42.BP<-sum.min42$BP
min42.VAS<-mean.min42$VAS
min42.comp<-cbind(min42.GSR, min42.BP, min42.VAS)
min43.GSR<-mean.min43$GSR
min43.BP<-sum.min43$BP
min43.VAS<-mean.min43$VAS
min43.comp<-cbind(min43.GSR, min43.BP, min43.VAS)
min44.GSR<-mean.min44$GSR
min44.BP<-sum.min44$BP
min44.VAS<-mean.min44$VAS
min44.comp<-cbind(min44.GSR, min44.BP, min44.VAS)
min45.GSR<-mean.min45$GSR
min45.BP<-sum.min45$BP
min45.VAS<-mean.min45$VAS
min45.comp<-cbind(min45.GSR, min45.BP, min45.VAS)
min46.GSR<-mean.min46$GSR
min46.BP<-sum.min46$BP
min46.VAS<-mean.min46$VAS
min46.comp<-cbind(min46.GSR, min46.BP, min46.VAS)


phys2346.comp<-rbind(min1.comp,min2.comp,min3.comp,min4.comp,min5.comp,
min6.comp,min7.comp,min8.comp,min9.comp,min10.comp,min11.comp,min12.comp,
min13.comp,min14.comp,min15.comp,min16.comp,min17.comp,min18.comp,
min19.comp,min20.comp,min21.comp,deparse.level=2)


min22.comp,min23.comp,min24.comp,
min25.comp,min26.comp,min27.comp,min28.comp,min29.comp,min30.comp,
min31.comp,min32.comp,min33.comp,min34.comp,min35.comp,min36.comp,
min37.comp, min38.comp,min39.comp,min40.comp,min41.comp,min42.comp,min43.comp,
min44.comp,min45.comp, min46.comp, deparse.level=2)

y<-c("8023.min1","8023.min2","8023.min3","8023.min4","8023.min5","8023.min6",
"8023.min7","8023.min8","8023.min9","8023.min10","8023.min11","8023.min12",
"8023.min13","8023.min14","8023.min15","8023.min16","8023.min17","8023.min18",
"8023.min19","8023.min20","8023.min21","8023.min22","8023.min23","8023.min24",
"8023.min25","8023.min26","8023.min27")















































