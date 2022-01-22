setwd("C:\\Users\\Vivianne Eilers\\Dropbox\\PhD Vivi\\Underpasses RK\\BR 116") # Lenovo and Hp address

library(data.table)
dtot<- fread("BR116_complete.csv", data.table= FALSE) 
str(dtot)

#Excluding random points from the data set ####
dtotSp<- dtot[dtot$Seq!="RP",]

#Excluding the species I don't want
dtotSp<-dtotSp[dtotSp$Species!="Ave",]
dtotSp<-dtotSp[dtotSp$Species!="Canis_lupus_familiaris",]
dtotSp<-dtotSp[dtotSp$Species!="Felis_catus",]
dtotSp<-dtotSp[dtotSp$Species!="Mammal",]
dtotSp<-dtotSp[dtotSp$Species!="Unidentified",]

# Replace the NA in the habitat by 0#####
dtotSp$Hab_1[is.na(dtotSp$Hab_1)] <- 0
dtotSp$Hab_3[is.na(dtotSp$Hab_3)] <- 0
dtotSp$Hab_4[is.na(dtotSp$Hab_4)] <- 0
dtotSp$Hab_42[is.na(dtotSp$Hab_42)] <- 0
dtotSp$Hab_49[is.na(dtotSp$Hab_49)] <- 0

# Making a boxplot to show the variation in the habitats for the kill points####
boxplot(dtotSp$Hab_1, dtotSp$Hab_3, dtotSp$Hab_4, dtotSp$Hab_42, dtotSp$Hab_49, ylab="Percentage",
        
        xlab="Habitats", names= c("Water", "Agriculture", "Grassland", "Forest", "Degraded field"))

#Finding the variation of land uses
summary(dtotSp$Hab_1) # percentage of water
summary(dtotSp$Hab_3) # percentage of agriculture
summary(dtotSp$Hab_4) # percentage of natural fields
summary(dtotSp$Hab_42) # Percentage of natural vegetation
summary(dtotSp$Hab_49) # percentage of degraded fields

#Finding how many road kills are only in Hab_3
in_hab_3<-dtotSp$fSp2[dtotSp$Hab_1==0 & dtotSp$Hab_3>0 & dtotSp$Hab_4==0 & dtotSp$Hab_42==0 & dtotSp$Hab_49==0]
table(in_hab_3)

#Finding how many road kills are only in Hab_49
in_hab_49<-dtotSp$fSp2[dtotSp$Hab_1==0 & dtotSp$Hab_3==0 & dtotSp$Hab_4==0 & dtotSp$Hab_42==0 & dtotSp$Hab_49>0]
table(in_hab_49)

#Excluding kill points from the data set ####
dtotCP<- dtot[dtot$Seq=="RP",]
str(dtotCP)

# Replace the NA in the habitat by 0#####
dtotCP$Hab_1[is.na(dtotCP$Hab_1)] <- 0
dtotCP$Hab_3[is.na(dtotCP$Hab_3)] <- 0
dtotCP$Hab_4[is.na(dtotCP$Hab_4)] <- 0
dtotCP$Hab_42[is.na(dtotCP$Hab_42)] <- 0
dtotCP$Hab_49[is.na(dtotCP$Hab_49)] <- 0

# Making a boxplot to show the variation in the habitats for the control points####
boxplot(dtotCP$Hab_1, dtotCP$Hab_3, dtotCP$Hab_4, dtotCP$Hab_42, dtotCP$Hab_49, ylab="Percentage",
        
        xlab="Habitats", names= c("Water", "Agriculture", "Grassland", "Forest", "Degraded field"))

# Underpasses dataset
BR116_UP<-fread("BR116_UP.csv", data.table=F, h=T)
str(BR116_UP)

# Replace the NA in the habitat by 0#####
BR116_UP$Hab_1[is.na(BR116_UP$Hab_1)] <- 0
BR116_UP$Hab_3[is.na(BR116_UP$Hab_3)] <- 0
BR116_UP$Hab_4[is.na(BR116_UP$Hab_4)] <- 0
BR116_UP$Hab_42[is.na(BR116_UP$Hab_42)] <- 0
BR116_UP$Hab_49[is.na(BR116_UP$Hab_49)] <- 0

# Making a boxplot to show the variation in the habitats ####
boxplot(BR116_UP$Hab_1, BR116_UP$Hab_3, BR116_UP$Hab_4, BR116_UP$Hab_42, BR116_UP$Hab_49, ylab="Percentage",
        
        xlab="Habitats", names= c("Water", "Agriculture", "Grassland", "Forest", "Degraded field"))

#Binding control points and underpasses data set
CPdat<-dtotCP[,c(3:7)]
#CPdat$type<-"road"

BR116_UP_2<-BR116_UP[, c(2:6)]
#BR116_UP$type<-"UP"

Habdat<-rbind(cbind(stack(CPdat), group="road"), 
              cbind(stack(BR116_UP_2), group="underpass"))

library(ggplot2)

#Split the habitats in different panels
ggplot(Habdat, aes(group, values)) +
  geom_boxplot() +
  facet_wrap(~ind, scales="free_y")

#Violin plot
#Split the habitats in different panels
Hab_names<-as_labeller(c("Hab_1" = "Water", "Hab_3" = "Agriculture", "Hab_4" = "Grassland",
                         "Hab_42" = "Forest", "Hab_49" = "Degraded field"))
ggplot(Habdat, aes(group, values)) +
  geom_violin(draw_quantiles =  c(0.25, 0.5, 0.75)) +
  #scale_y_log10() + 
  facet_wrap(~ind, scales="free_y", labeller = Hab_names) +
  labs(y = "Percentage")


#Creating stretches of 600 meters
SC.600<- seq(0, 217200, by=600) # Lenght of the road is 217200
#Removing the last Stretch because I created it bigger than needed
SC.600<- SC.600[-length(SC.600)]

# Creating points for every stretch ####
# 4 points per stretch for species with <800 kills
c4<- SC.600 + matrix(1, ncol= 1, nrow= length(SC.600)) %*% 
  matrix(seq(0, 600, l=4+1)[-1], nrow= 1)
# 6 points per stretch for species with > 800 kills
c6<- SC.600 + matrix(1, ncol= 1, nrow= length(SC.600)) %*%
  matrix(seq(0, 600, l=6+1)[-1], nrow= 1)

# creating a table with the name of the species####
Sp.1<- table(dtotSp$Species)

# Combining the name of species with the control points created####
Species<-names(Sp.1[Sp.1 < 800])
Didelphis<- names(Sp.1[Sp.1 > 800])

dataSp<- rbind(expand.grid(distance= c4, Species= Species),
               expand.grid(distance= c6, Species= Didelphis))

# Creating a variable When, selecting even rows to before and odd rows to after####
#sort by Species and distance
dataSp <- dataSp[order(dataSp$Species, dataSp$distance),]
dataSp$ID<-1:290324
dataSp$When<-ifelse(dataSp$ID %% 2, "after", "before")

# Importing data frame with control points created in QGIS ####
C50<- fread("L1_Dist_UP_Points_every_50m.csv", data.table= FALSE) 

#Reshaping the data frame to have the percentage of habitat in columns according to the habitat
# Creating a list based on the ID and percentage of each habitat in columns####
C50_new<- tapply(C50$Percentage, list(C50$ID, C50$COD_USO), mean, na.rm=T)

# Converting the list into a data frame####
C50_df<-as.data.frame(C50_new, row.names=TRUE)
C50_df$ID<-1:4344

# Renaming the columns
names(C50_df)[names(C50_df) == "1"] <- "Hab_1"
names(C50_df)[names(C50_df) == "3"] <- "Hab_3"
names(C50_df)[names(C50_df) == "4"] <- "Hab_4"
names(C50_df)[names(C50_df) == "42"] <- "Hab_42"
names(C50_df)[names(C50_df) == "49"] <- "Hab_49"

# Creating a subset of the main data (C50)just with the variables I need:
# 9:12 -> cngmeters, ID, Zone, X_coord, Y_coord
# 15 -> join_Stret (Underpass stretch ID)
# 17 -> join_Zone
# 21 -> distance # change name cngmeters to distance and distance to nearest.un
C50_U<-unique(C50[, c(9:12, 15, 17, 21)])

#Changing variables names
names(C50_U)[names(C50_U) == "distance"] <- "nearest.un"
names(C50_U)[names(C50_U) == "cngmeters"] <- "distance"
names(C50_U)[names(C50_U) == "xcoord"] <- "X_coordina"
names(C50_U)[names(C50_U) == "ycoord"] <- "Y_Coordina"
names(C50_U)[names(C50_U) == "join_Zone"] <- "Zone"

#Creating a variable Roadkill to indicate control point
C50_U$Roadkill<-0

# Merging QGIS control points ####
C50_Hab<-merge(C50_U, C50_df, all.x=T)

# Merging R control points with QGIS control points####
dataSp<-dataSp[,c(1:2, 4)]
dataCK<-merge(dataSp, C50_Hab, by="distance", all.x=T)

# Combining control points and road kill dataset
dtotSp<-dtotSp[ ,c(2:21, 26:27, 29, 35:38, 40)]

library(gtools)
CPRK<-smartbind(dtotSp,dataCK)

# Grouping species ####
Pool2<- names(Sp.1[Sp.1 > 50])

Pool2<-Pool2[Pool2!="Ave"]
Pool2<-Pool2[Pool2!="Canis_lupus_familiaris"]
Pool2<-Pool2[Pool2!="Felis_catus"]
Pool2<-Pool2[Pool2!="Mammal"]
Pool2<-Pool2[Pool2!="Unidentified"]

#Creating a variable to group the pool of species with less than 50 records
CPRK$Sp2<-ifelse(CPRK$Species %in% Pool2, CPRK$Species, "Pool2")

# Creating a new variable for log Hab####
CPRK$log_Hab_1<-log(CPRK$Hab_1+1)
CPRK$log_Hab_3<-log(CPRK$Hab_3+1)
CPRK$log_Hab_4<-log(CPRK$Hab_4+1)
CPRK$log_Hab_42<-log(CPRK$Hab_42+1)
CPRK$log_Hab_49<-log(CPRK$Hab_49+1)

# Creating a new variable for After, coding 1 for after and 0 for before #### 
CPRK$After<-(CPRK$When=="after")*1

# Creating a variable for the log(nearest.und)
CPRK$logNU<-log(CPRK$nearest.un)

#Creating a variable for Near_5km
CPRK$Near_5km<-(CPRK$nearest.un <= 5000)*1
table(CPRK$Near_5km, CPRK$After)

# Creating a variable Stretch for every 600m ####
# Lenght of the road is 217200
CPRK$Stretch_600<-cut(CPRK$distance, breaks = c(seq(0, 217200, by=600)), labels = c(seq(1, 362, by=1)))

# Creating a variable for real animal ####
CPRK$RealAnimal<-(CPRK$Roadkill==1)*1
str(CPRK)

#Variables as factor####
CPRK$fStretch_600<-as.factor(CPRK$Stretch_600)
CPRK$fSpecies<-as.factor(CPRK$Species)
CPRK$fSp2<-as.factor(CPRK$Sp2)

# Creating a variable for the log(BM)
# Replacing NA with 0
CPRK$Adult_body[is.na(CPRK$Adult_body)] <- 0
CPRK$logBM<-log(CPRK$Adult_body+1)

# Creating a variable for the logBM for the real animal 
CPRK$logBM_RA<-CPRK$logBM * CPRK$RealAnimal

# Creating a variable for the interaction between nearest.un and After and Near_5 ####
# I don't need just the DistAfter because it includes all the distances and I need to limit it to 5 km
#DistNear####
CPRK$DistNear<-CPRK$logNU * CPRK$Near_5km # distances within 5 km
#DistWhenNear####
CPRK$DistWhenNear<-CPRK$logNU * CPRK$After * CPRK$Near_5km # distances within 5 km and after the construction of the underpass

# Creating a variable for the interaction between Near_5km and After
CPRK$NearAfter <- CPRK$Near_5km * CPRK$After

# correct control points for fSp2
lines2exclude<- (CPRK$fSp2 == "Pool2") & (CPRK$fSpecies != "Tyto_furcata") & (CPRK$Roadkill == 0)
# Tyto_furcata is just one arbitrarily chosen species of the Pool2
CPRK2<- CPRK[!lines2exclude, ]

table(CPRK2$fSp2, CPRK2$Roadkill)

## Fitting the full model with interactions of species pool with all other variables ####
library(mgcv)

system.time(bam.11.Sp7<- bam(Roadkill ~ 
                               fSp2 +                                       # fixed effect of species
                               log_Hab_1 +                                      # effect of the percentage of water bodies
                               log_Hab_3 +                                      # effect of the percentage of agricultural areas
                               log_Hab_4 +                                      # effect of the percentage of natural fields
                               log_Hab_42 +                                     # effect of the percentage of a mix of forest and fields areas
                               log_Hab_49 +                                     # effect of the percentage of degraded fields
                               NearAfter +                                      # effect of being registered after the construction of underpass and within 5 km
                               Near_5km+                                        # effect of being within 5 km 
                               DistNear +                                       # effect of distance when the point is less than 5 km from the underpass
                               DistWhenNear +                                   # effect of distance when the point was registered after the construction of underpass and is less than 5 km far
                               s(fSp2, bs="re", by=log_Hab_1) +             # effect of the percentage of water bodies varying according to species
                               s(fSp2, bs="re", by=log_Hab_3) +             # effect of the percentage of agricultural areas varying according to species
                               s(fSp2, bs="re", by=log_Hab_4) +             # effect of the percentage of natural fields varying according to species
                               s(fSp2, bs="re", by=log_Hab_42) +            # effect of the percentage of a mix of forest and fields areas varying according to species
                               s(fSp2, bs="re", by=log_Hab_49) +            # effect of the percentage of degraded fields varying according to species
                               s(fSp2, bs="re", by=DistNear) +              # effect of distance within 5 km varying according to species
                               s(fSp2, bs="re", by=NearAfter) +             # effect of being registered after the construction of underpass, but within 5km, varying according to species
                               s(fSp2, bs="re", by=Near_5km) +              # effect of being within 5 km, varying according to species
                               s(fSp2, bs="re", by=DistWhenNear) +          # effect of distance when the point was registered after the construction of underpass, varying according to species
                               s(fStretch_600, bs="re", by=fSp2) +              # effect of spatial trend according to species (segment of the road) 
                               s(distance, by=fSp2, id=1, k=100) ,              # effect of spatial trend according to species (distance from the beggining  of the road) 
                             select=T, data=CPRK2, family=binomial,
                             discrete= T, nthreads= 3))

save(bam.11.Sp7, file= "bam11Sp7_outputs.RData")

load("bam11Sp7_outputs.RData")

summary(bam.11.Sp7)
#Error: cannot allocate vector of size 899.5 Mb
summary(bam.11.Sp7, re.test = F) # the re.test argument to FALSE 
#will skip the tests for random effects (and other terms with zero dimension null space), 
#allowing the summary for the other terms to be computed quickly.

summary.gam(bam.11.Sp7)
par(mfrow=c(2,4))
plot(bam.11.Sp7)

# Extracting the coefficients ####
Coef.bam11Sp7<- coefficients(bam.11.Sp7)
str(Coef.bam11Sp7)

#Taking the square root of the diagonal to get the standard error
Coef.bam11Sp7.SE<- diag(bam.11.Sp7$Vc)^0.5

#For habitat 1 - percentage of water ####
C_Hab_1_Sp<-Coef.bam11Sp7[grep("log_Hab_1", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of log_Hab_1 for each species
C_Hab_1_Fx<-Coef.bam11Sp7[grep("log_Hab_1", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of log_Hab_1 
C_Hab_1_Sp_SE<-Coef.bam11Sp7.SE[grep("log_Hab_1", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of log_Hab_1 for each species
C_Hab_1_Fx_SE<-Coef.bam11Sp7.SE[grep("log_Hab_1", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of log_Hab_1 
C_Hab_1_Sp_CI<-C_Hab_1_Sp_SE*1.96 # Calculating the Confidence intervals for the random effect of Hab_1 for each species
C_Hab_1_Fx_CI<-C_Hab_1_Fx_SE*1.96 # Calculating the Confidence intervals for the fixed effect of Hab_1

#Plotting the matrix for the coefficients for Habitat 1####
par(mfrow=c(1,1), mar= c(4.1, 12, 4.1, 4.1), las=1)

C_Hab_1_Sp_order<-order(C_Hab_1_Sp, decreasing=T)

C_Hab1_range<-range(c(C_Hab_1_Fx + C_Hab_1_Sp[C_Hab_1_Sp_order] - C_Hab_1_Sp_SE[C_Hab_1_Sp_order],
                      C_Hab_1_Fx + C_Hab_1_Sp[C_Hab_1_Sp_order] + C_Hab_1_Sp_SE[C_Hab_1_Sp_order]))
Hab_1.rank_2<-barplot(C_Hab_1_Sp[C_Hab_1_Sp_order], beside=T, horiz=T, xlim=C_Hab1_range, main="Percentage of water")
abline(v=C_Hab_1_Fx, col = "red") # main effect of habitat 1
str(Hab_1.rank_2)

#Adding standard errors
arrows(x0=C_Hab_1_Fx + C_Hab_1_Sp[C_Hab_1_Sp_order] - C_Hab_1_Sp_SE[C_Hab_1_Sp_order], 
       x1=C_Hab_1_Fx + C_Hab_1_Sp[C_Hab_1_Sp_order] + C_Hab_1_Sp_SE[C_Hab_1_Sp_order],
       y0=Hab_1.rank_2, y1=Hab_1.rank_2, length = 0)

#Adding confidence intervals
arrows(x0=C_Hab_1_Fx + C_Hab_1_Sp[C_Hab_1_Sp_order] - C_Hab_1_Sp_CI[C_Hab_1_Sp_order], 
       x1=C_Hab_1_Fx + C_Hab_1_Sp[C_Hab_1_Sp_order] + C_Hab_1_Sp_CI[C_Hab_1_Sp_order],
       y0=Hab_1.rank_2, y1=Hab_1.rank_2, length = 0)

#For habitat 3 - percentage of agriculture ####
C_Hab_3_Sp<-Coef.bam11Sp7[grep("log_Hab_3", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of log_Hab_3 for each species
C_Hab_3_Fx<-Coef.bam11Sp7[grep("log_Hab_3", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of log_Hab_3 
C_Hab_3_Sp_SE<-Coef.bam11Sp7.SE[grep("log_Hab_3", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of log_Hab_3 for each species
C_Hab_3_Fx_SE<-Coef.bam11Sp7.SE[grep("log_Hab_3", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of log_Hab_3 
C_Hab_3_Sp_CI<-C_Hab_3_Sp_SE*1.96 # Calculating the Confidence intervals for the random effect of Hab_3 for each species
C_Hab_3_Fx_CI<-C_Hab_3_Fx_SE*1.96 # Calculating the Confidence intervals for the fixed effect of Hab_3

#Plotting the matrix for the coefficients for Habitat 3####
par(mfrow=c(1,1), mar= c(4.1, 12, 4.1, 4.1), las=1)

C_Hab_3_Sp_order<-order(C_Hab_3_Sp, decreasing=T)

C_Hab3_range<-range(c(C_Hab_3_Fx + C_Hab_3_Sp[C_Hab_3_Sp_order] - C_Hab_3_Sp_SE[C_Hab_3_Sp_order],
                      C_Hab_3_Fx + C_Hab_3_Sp[C_Hab_3_Sp_order] + C_Hab_3_Sp_SE[C_Hab_3_Sp_order]))
Hab_3.rank_2<-barplot(C_Hab_3_Sp[C_Hab_3_Sp_order], beside=T, horiz=T, xlim=C_Hab3_range, main="Percentage of agriculture")
abline(v=C_Hab_3_Fx, col = "red") # main effect of habitat 1
str(Hab_3.rank_2)

#Adding standard errors
arrows(x0=C_Hab_3_Fx + C_Hab_3_Sp[C_Hab_3_Sp_order] - C_Hab_3_Sp_SE[C_Hab_3_Sp_order], 
       x1=C_Hab_3_Fx + C_Hab_3_Sp[C_Hab_3_Sp_order] + C_Hab_3_Sp_SE[C_Hab_3_Sp_order],
       y0=Hab_3.rank_2, y1=Hab_3.rank_2, length = 0)

#Adding confidence intervals
arrows(x0=C_Hab_3_Fx + C_Hab_3_Sp[C_Hab_3_Sp_order] - C_Hab_3_Sp_CI[C_Hab_3_Sp_order], 
       x1=C_Hab_3_Fx + C_Hab_3_Sp[C_Hab_3_Sp_order] + C_Hab_3_Sp_CI[C_Hab_3_Sp_order],
       y0=Hab_3.rank_2, y1=Hab_3.rank_2, length = 0)

#For habitat 4 - percentage of natural fields (grassland) ####
C_Hab_4_Sp<-Coef.bam11Sp7[grep("log_Hab_4", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of log_Hab_4 for each species
C_Hab_4_Fx<-Coef.bam11Sp7[grep("log_Hab_4", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of log_Hab_4 
C_Hab_4_Sp_SE<-Coef.bam11Sp7.SE[grep("log_Hab_4", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of log_Hab_4 for each species
C_Hab_4_Fx_SE<-Coef.bam11Sp7.SE[grep("log_Hab_4", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of log_Hab_4 
C_Hab_4_Sp_CI<-C_Hab_4_Sp_SE*1.96 # Calculating the Confidence intervals for the random effect of Hab_4 for each species
C_Hab_4_Fx_CI<-C_Hab_4_Fx_SE*1.96 # Calculating the Confidence intervals for the fixed effect of Hab_4

par(mfrow=c(1,1), mar= c(4.1, 12, 4.1, 4.1), las=1)

C_Hab_4_Sp_order<-order(C_Hab_4_Sp, decreasing=T)

C_Hab4_range<-range(c(C_Hab_4_Fx + C_Hab_4_Sp[C_Hab_4_Sp_order] - C_Hab_4_Sp_SE[C_Hab_4_Sp_order],
                      C_Hab_4_Fx + C_Hab_4_Sp[C_Hab_4_Sp_order] + C_Hab_4_Sp_SE[C_Hab_4_Sp_order]))
Hab_4.rank_2<-barplot(C_Hab_4_Sp[C_Hab_4_Sp_order], beside=T, horiz=T, xlim=C_Hab4_range, main="Percentage of grassland")
abline(v=C_Hab_4_Fx, col = "red") # main effect of habitat 1
str(Hab_4.rank_2)

#Adding standard errors
arrows(x0=C_Hab_4_Fx + C_Hab_4_Sp[C_Hab_4_Sp_order] - C_Hab_4_Sp_SE[C_Hab_4_Sp_order], 
       x1=C_Hab_4_Fx + C_Hab_4_Sp[C_Hab_4_Sp_order] + C_Hab_4_Sp_SE[C_Hab_4_Sp_order],
       y0=Hab_4.rank_2, y1=Hab_4.rank_2, length = 0)

#Adding confidence intervals
arrows(x0=C_Hab_4_Fx + C_Hab_4_Sp[C_Hab_4_Sp_order] - C_Hab_4_Sp_CI[C_Hab_4_Sp_order], 
       x1=C_Hab_4_Fx + C_Hab_4_Sp[C_Hab_4_Sp_order] + C_Hab_4_Sp_CI[C_Hab_4_Sp_order],
       y0=Hab_4.rank_2, y1=Hab_4.rank_2, length = 0)

#For habitat 42 - percentage of natural vegetation ####
C_Hab_42_Sp<-Coef.bam11Sp7[grep("log_Hab_42", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of log_Hab_42 for each species
C_Hab_42_Fx<-Coef.bam11Sp7[grep("log_Hab_42", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of log_Hab_4 2
C_Hab_42_Sp_SE<-Coef.bam11Sp7.SE[grep("log_Hab_42", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of log_Hab_42 for each species
C_Hab_42_Fx_SE<-Coef.bam11Sp7.SE[grep("log_Hab_42", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of log_Hab_42 
C_Hab_42_Sp_CI<-C_Hab_42_Sp_SE*1.96 # Calculating the Confidence intervals for the random effect of Hab_42 for each species
C_Hab_42_Fx_CI<-C_Hab_42_Fx_SE*1.96 # Calculating the Confidence intervals for the fixed effect of Hab_42

par(mfrow=c(1,1), mar= c(4.1, 12, 4.1, 4.1), las=1)

C_Hab_42_Sp_order<-order(C_Hab_42_Sp, decreasing=T)

C_Hab42_range<-range(c(C_Hab_42_Fx + C_Hab_42_Sp[C_Hab_42_Sp_order] - C_Hab_42_Sp_SE[C_Hab_42_Sp_order],
                       C_Hab_42_Fx + C_Hab_42_Sp[C_Hab_42_Sp_order] + C_Hab_42_Sp_SE[C_Hab_42_Sp_order]))
Hab_42.rank_2<-barplot(C_Hab_42_Sp[C_Hab_42_Sp_order], beside=T, horiz=T, xlim=C_Hab42_range, main="Percentage of forest")
Hab_42.rank_2<-barplot(C_Hab_42_Fx + C_Hab_42_Sp[C_Hab_42_Sp_order], beside=T, horiz=T, xlim=c(-0.3, 0.1), main="Percentage of forest")
abline(v=C_Hab_42_Fx, col = "red") # main effect of habitat 1
str(Hab_42.rank_2)

#Adding standard errors
arrows(x0=C_Hab_42_Fx + C_Hab_42_Sp[C_Hab_42_Sp_order] - C_Hab_42_Sp_SE[C_Hab_42_Sp_order], 
       x1=C_Hab_42_Fx + C_Hab_42_Sp[C_Hab_42_Sp_order] + C_Hab_42_Sp_SE[C_Hab_42_Sp_order],
       y0=Hab_42.rank_2, y1=Hab_42.rank_2, length = 0)

#Adding confidence intervals
arrows(x0=C_Hab_42_Fx + C_Hab_42_Sp[C_Hab_42_Sp_order] - C_Hab_42_Sp_CI[C_Hab_42_Sp_order], 
       x1=C_Hab_42_Fx + C_Hab_42_Sp[C_Hab_42_Sp_order] + C_Hab_42_Sp_CI[C_Hab_42_Sp_order],
       y0=Hab_42.rank_2, y1=Hab_42.rank_2, length = 0)

#For habitat 49 - percentage of degraded fields ####
C_Hab_49_Sp<-Coef.bam11Sp7[grep("log_Hab_49", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of log_Hab_49 for each species
C_Hab_49_Fx<-Coef.bam11Sp7[grep("log_Hab_49", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of log_Hab_49
C_Hab_49_Sp_SE<-Coef.bam11Sp7.SE[grep("log_Hab_49", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of log_Hab_49 for each species
C_Hab_49_Fx_SE<-Coef.bam11Sp7.SE[grep("log_Hab_49", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of log_Hab_49 
C_Hab_49_Sp_CI<-C_Hab_49_Sp_SE*1.96 # Calculating the Confidence intervals for the random effect of Hab_49 for each species
C_Hab_49_Fx_CI<-C_Hab_49_Fx_SE*1.96 # Calculating the Confidence intervals for the fixed effect of Hab_49

par(mfrow=c(1,1), mar= c(4.1, 12, 4.1, 4.1), las=1)

C_Hab_49_Sp_order<-order(C_Hab_49_Sp, decreasing=T)

C_Hab49_range<-range(c(C_Hab_49_Fx + C_Hab_49_Sp[C_Hab_49_Sp_order] - C_Hab_49_Sp_SE[C_Hab_49_Sp_order],
                       C_Hab_49_Fx + C_Hab_49_Sp[C_Hab_49_Sp_order] + C_Hab_49_Sp_SE[C_Hab_49_Sp_order]))
Hab_49.rank_2<-barplot(C_Hab_49_Sp[C_Hab_49_Sp_order], beside=T, horiz=T, xlim=C_Hab49_range, main="Percentage of forest")
Hab_49.rank_2<-barplot(C_Hab_49_Fx + C_Hab_49_Sp[C_Hab_49_Sp_order], beside=T, horiz=T, xlim=c(-0.3, 0.1), main="Percentage of forest")
abline(v=C_Hab_49_Fx, col = "red") # main effect of habitat 1
str(Hab_49.rank_2)

#Adding standard errors
arrows(x0=C_Hab_49_Fx + C_Hab_49_Sp[C_Hab_49_Sp_order] - C_Hab_49_Sp_SE[C_Hab_49_Sp_order], 
       x1=C_Hab_49_Fx + C_Hab_49_Sp[C_Hab_49_Sp_order] + C_Hab_49_Sp_SE[C_Hab_49_Sp_order],
       y0=Hab_49.rank_2, y1=Hab_49.rank_2, length = 0)

#Adding confidence intervals
arrows(x0=C_Hab_49_Fx + C_Hab_49_Sp[C_Hab_49_Sp_order] - C_Hab_49_Sp_CI[C_Hab_49_Sp_order], 
       x1=C_Hab_49_Fx + C_Hab_49_Sp[C_Hab_49_Sp_order] + C_Hab_49_Sp_CI[C_Hab_49_Sp_order],
       y0=Hab_49.rank_2, y1=Hab_49.rank_2, length = 0)

# Plotting the coefficients for the habitats ####
par(mfrow=c(1,1))
H<-c(0.1, 0.3, 0.5, 0.7, 0.9)
Hc<-c(-0.01, -0.04, 0.22, -0.13,-0.03)
sdH<-c(0.03, 0.02, 0.03, 0.02, 0.02)

plot(H, Hc, xlim=c(0,1), ylim=c(-0.3,0.4), pch=19, axes=T, xaxt="n", ylab="Coefficients (log-odds)", xlab="Habitats")
axis(1, at=c(0.1, 0.3, 0.5, 0.7, 0.9), labels=c("Water", "Agriculture", "Grassland", "Forest", "Degraded field"))
arrows(x0=H, y0=Hc-sdH, x1=H, y1=Hc+sdH, code=3, col=c("blue", "green", "red", "yellow", "orange"), lwd=2, angle=90, length=0.1)
abline(h=0, col = "lightgray", lty = 3)

# For the distance near the underpass (After)####
C_DistWhenNear<-Coef.bam11Sp7[grep("DistWhenNear", names(Coef.bam11Sp7))]
C_DistWhenNear_Sp<-Coef.bam11Sp7[grep("DistWhenNear", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of DistWhenNear for each species
C_DistWhenNear_Fx<-Coef.bam11Sp7[grep("DistWhenNear", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of DistWhenNear
C_DistWhenNear_Sp_SE<-Coef.bam11Sp7.SE[grep("DistWhenNear", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of DistWhenNear for each species
C_DistWhenNear_Fx_SE<-Coef.bam11Sp7.SE[grep("DistWhenNear", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of DistWhenNear 
C_DistWhenNear_Sp_CI<-C_DistWhenNear_Sp_SE*1.96 # Calculating the Confidence intervals for the random effect of DistWhenNear for each species
C_DistWhenNear_Fx_CI<-C_DistWhenNear_Fx_SE*1.96 # Calculating the Confidence intervals for the fixed effect of DistWhenNear

par(mfrow=c(1,1), mar= c(4.1, 12, 4.1, 4.1), las=1)

C_DistWhenNear_Sp_order<-order(C_DistWhenNear_Sp, decreasing=T)

C_DistWhenNear_range<-range(c(C_DistWhenNear_Fx + C_DistWhenNear_Sp[C_DistWhenNear_Sp_order] - C_DistWhenNear_Sp_SE[C_DistWhenNear_Sp_order],
                       C_DistWhenNear_Fx + C_DistWhenNear_Sp[C_DistWhenNear_Sp_order] + C_DistWhenNear_Sp_SE[C_DistWhenNear_Sp_order]))
DistWhenNear.rank_2<-barplot(C_DistWhenNear_Sp[C_DistWhenNear_Sp_order], beside=T, horiz=T, xlim=C_DistWhenNear_range, main="Distance After Underpass")
DistWhenNear.rank_2<-barplot(C_DistWhenNear_Fx + C_DistWhenNear_Sp[C_DistWhenNear_Sp_order], beside=T, horiz=T, xlim=c(-0.3, 0.1), main="Distance After Underpass")
abline(v=C_DistWhenNear_Fx, col = "red") # main effect of habitat 1
str(DistWhenNear.rank_2)

#Adding standard errors
arrows(x0=C_DistWhenNear_Fx + C_DistWhenNear_Sp[C_DistWhenNear_Sp_order] - C_DistWhenNear_Sp_SE[C_DistWhenNear_Sp_order], 
       x1=C_DistWhenNear_Fx + C_DistWhenNear_Sp[C_DistWhenNear_Sp_order] + C_DistWhenNear_Sp_SE[C_DistWhenNear_Sp_order],
       y0=DistWhenNear.rank_2, y1=DistWhenNear.rank_2, length = 0)

#Adding confidence intervals
arrows(x0=C_DistWhenNear_Fx + C_DistWhenNear_Sp[C_DistWhenNear_Sp_order] - C_DistWhenNear_Sp_CI[C_DistWhenNear_Sp_order], 
       x1=C_DistWhenNear_Fx + C_DistWhenNear_Sp[C_DistWhenNear_Sp_order] + C_DistWhenNear_Sp_CI[C_DistWhenNear_Sp_order],
       y0=DistWhenNear.rank_2, y1=DistWhenNear.rank_2, length = 0)

# For the distance near the underpass (Before)####
C_DistNear<-Coef.bam11Sp7[grep("DistNear", names(Coef.bam11Sp7))]
C_DistNear_Sp<-Coef.bam11Sp7[grep("DistNear", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of DistNear for each species
C_DistNear_Fx<-Coef.bam11Sp7[grep("DistNear", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of DistNear
C_DistNear_Sp_SE<-Coef.bam11Sp7.SE[grep("DistNear", names(Coef.bam11Sp7))][-1] # It extracts the coefficient for the random effect of DistNear for each species
C_DistNear_Fx_SE<-Coef.bam11Sp7.SE[grep("DistNear", names(Coef.bam11Sp7))][1] # It extracts the coefficient for the fixed effect of DistNear 
C_DistNear_Sp_CI<-C_DistNear_Sp_SE*1.96 # Calculating the Confidence intervals for the random effect of DistNear for each species
C_DistNear_Fx_CI<-C_DistNear_Fx_SE*1.96 # Calculating the Confidence intervals for the fixed effect of DistNear

par(mfrow=c(1,1), mar= c(4.1, 12, 4.1, 4.1), las=1)
C_DistNear_Sp_order<-order(C_DistNear_Sp, decreasing=T)
C_DistNear_range<-range(c(C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order] - C_DistNear_Sp_SE[C_DistNear_Sp_order],
                          C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order] + C_DistNear_Sp_SE[C_DistNear_Sp_order]))
DistNear.rank_2<-barplot(C_DistNear_Sp[C_DistNear_Sp_order], beside=T, horiz=T, xlim=C_DistNear_range, main="Distance Before Underpass")
DistNear.rank_2<-barplot(C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order], beside=T, horiz=T, xlim=c(-0.3, 0.1), main="Distance Before Underpass")
abline(v=C_DistNear_Fx, col = "red") # main effect of habitat 1
str(DistNear.rank_2)

#Adding standard errors
arrows(x0=C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order] - C_DistNear_Sp_SE[C_DistNear_Sp_order], 
       x1=C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order] + C_DistNear_Sp_SE[C_DistNear_Sp_order],
       y0=DistNear.rank_2, y1=DistNear.rank_2, length = 0)

#Adding confidence intervals
arrows(x0=C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order] - C_DistNear_Sp_CI[C_DistNear_Sp_order], 
       x1=C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order] + C_DistNear_Sp_CI[C_DistNear_Sp_order],
       y0=DistNear.rank_2, y1=DistNear.rank_2, length = 0)

#Scatterplot of effect of distance Before and after (1)#####
# Blank plot
x0 <- c(0.2, 0.8)
y0 <- c(-0.05, -0.003)
sd0<-c(0.03, 0.05)
plot(x0, y0, xlim=c(0,1), ylim=c(-1,1), pch=19, axes=FALSE, xlab="Before                                                                                 After",
     ylab="Coefficients")
axis(1, 0:1)
axis(2, -1:1)

# Vertical arrow
arrows(x0=x0, y0=y0-sd0, x1=x0, y1=y0+sd0, code=3, col=c("blue", "red"), lwd=2, angle=90, length=0.1)

#Grouping the effects of underpass and distance####
xU<-c(0.2, 0.3, 0.7, 0.8)
yU<-c(0.8, -0.98, -0.05, -0.003)
sdU<-c(0.2, 0.3, 0.03, 0.05)

par(mfrow=c(1,1), mar= c(4.1, 6.1, 4.1, 4.1), las=1)

plot(xU, yU, xlim=c(0,1), ylim=c(-2,2), pch=19, axes=T, xlab="",
     ylab="Coefficients (log-odds)", xaxt='n')
axis(1, at=c(0.25, 0.75), labels=c("Area within 5 km", "Distance"))
arrows(x0=xU, y0=yU-sdU, x1=xU, y1=yU+sdU, code=3, col=c("blue", "red", "blue", "red"), lwd=3, angle=90, length=0.1)

abline(h=0, col = "lightgray", lty = 3)

legend("bottom", legend=c("Before", "After"),ncol = 2, cex = 1, text.col = c("blue", "red"), bty = "n", text.font=2)

#Comparing habitat for the same species ####

par(mfrow=c(2,2))

#Furnarius rufus ####
Fr_1<--0.05073
Fr_3<--0.04456
Fr_4<-0.220002
Fr_42<--0.0533
Fr_49<--0.05096

Fr_X<-c(0.1, 0.3, 0.5, 0.7, 0.9)
Fr_Y<-c(Fr_1, Fr_3, Fr_4, Fr_42, Fr_49)
sd_H<-c(0.03, 0.02, 0.03, 0.02, 0.02) # I used the standard deviation for habitat

plot(Fr_X, Fr_Y, xlim=c(0,1), ylim=c(-0.2,0.4), pch=19, axes=T, xaxt="n", ylab="Coefficients (log-odds)", xlab="Habitat", main=substitute(paste(italic("Furnarius rufus"))))
axis(1, at=c(0.1, 0.3, 0.5, 0.7, 0.9), labels=c("Water", "Agriculture", "Grassland", "Forest", "Degraded field"))
arrows(x0=Fr_X, y0=Fr_Y-sd_H, x1=Fr_X, y1=Fr_Y+sd_H, code=3, col=c("blue", "green", "red", "yellow", "orange"), lwd=2, angle=90, length=0.1)
abline(h=0, col = "lightgray", lty = 3)

#Zenaida auriculata ####
Za_1<-0.028714
Za_3<-0.014443
Za_4<-0.219993
Za_42<--0.09098
Za_49<--0.08464

Za_X<-c(0.1, 0.3, 0.5, 0.7, 0.9)
Za_Y<-c(Za_1, Za_3, Za_4, Za_42, Za_49)
sd_H<-c(0.03, 0.02, 0.03, 0.02, 0.02) # I used the standard deviation for habitat

plot(Za_X, Za_Y, xlim=c(0,1), ylim=c(-0.2,0.4), pch=19, axes=T, xaxt="n", ylab="Coefficients (log-odds)", xlab="Habitats", main=substitute(paste(italic("Zenaida auriculata"))))
axis(1, at=c(0.1, 0.3, 0.5, 0.7, 0.9), labels=c("Water", "Agriculture", "Grassland", "Forest", "Degraded field"))
arrows(x0=Za_X, y0=Za_Y-sd_H, x1=Za_X, y1=Za_Y+sd_H, code=3, col=c("blue", "green", "red", "yellow", "orange"), lwd=2, angle=90, length=0.1)
abline(h=0, col = "lightgray", lty = 3)

#Trachemys dorbigni####
Td_1<-0.060913
Td_3<--0.0946
Td_4<-0.22
Td_42<--0.18444
Td_49<-0.001161

Td_X<-c(0.1, 0.3, 0.5, 0.7, 0.9)
Td_Y<-c(Td_1, Td_3, Td_4, Td_42, Td_49)
sd_H<-c(0.03, 0.02, 0.03, 0.02, 0.02) # I used the standard deviation for habitat

plot(Td_X, Td_Y, xlim=c(0,1), ylim=c(-0.2,0.4), pch=19, axes=T, xaxt="n", ylab="Coefficients (log-odds)", xlab="Habitats", main=substitute(paste(italic("Trachemys dorbigni"))))
axis(1, at=c(0.1, 0.3, 0.5, 0.7, 0.9), labels=c("Water", "Agriculture", "Grassland", "Forest", "Degraded field"))
arrows(x0=Td_X, y0=Td_Y-sd_H, x1=Td_X, y1=Td_Y+sd_H, code=3, col=c("blue", "green", "red", "yellow", "orange"), lwd=2, angle=90, length=0.1)
abline(h=0, col = "lightgray", lty = 3)

#Didelphis albiventris####
Da_1<--0.09266
Da_3<--0.1008
Da_4<-0.22
Da_42<--0.09065
Da_49<--0.10596

Da_X<-c(0.1, 0.3, 0.5, 0.7, 0.9)
Da_Y<-c(Da_1, Da_3, Da_4, Da_42, Da_49)
sd_H<-c(0.03, 0.02, 0.03, 0.02, 0.02) # I used the standard deviation for habitat

plot(Da_X, Da_Y, xlim=c(0,1), ylim=c(-0.2,0.4), pch=19, axes=T, xaxt="n", ylab="Coefficients (log-odds)", xlab="Habitats", main=substitute(paste(italic("Didelphis albiventris"))))
axis(1, at=c(0.1, 0.3, 0.5, 0.7, 0.9), labels=c("Water", "Agriculture", "Grassland", "Forest", "Degraded field"))
arrows(x0=Da_X, y0=Da_Y-sd_H, x1=Da_X, y1=Da_Y+sd_H, code=3, col=c("blue", "green", "red", "yellow", "orange"), lwd=2, angle=90, length=0.1)
abline(h=0, col = "lightgray", lty = 3)

#Names of species in the rank order
DistNear.SPrank<-c("Furnarius rufus", "Salvator merianae", "Guira guira",
                   "Dasypus novemcinctus", "Pitangus sulphuratus", "Pool2",
                   "Turdus amaurochalinus", "Turdus rufiventris", 
                   "Nothura maculosa", "Cavia aperea", "Conepatus chinga",
                   "Columbina picui", "Lycalopex gymnocercus", 
                   "Euphractus sexcinctus","Cerdocyon thous",
                   "Columbina talpacoti", "Tyrannus savana",
                   "Philodryas patagoniensis", "Chrysomus ruficapillus",
                   "Trachemys dorbigni", "Didelphis albiventris", 
                   "Galictis cuja", "Paroaria coronata", "Zenaida auriculata", 
                   "Passer domesticus",  "Helicops infrataeniatus")

#To plot only the random effect
DistNear.rank_2<-barplot(C_DistNear_Sp[C_DistNear_Sp_order], beside=T, horiz=T, 
                         xlim=c(-0.00001, 0.00001), main="Distance to underpass",
                         names.arg=DistNear.SPrank, font.axis = 3)
abline(v=C_DistNear_Fx, col = "red") # main effect of DistNear
#Adding confidence intervals for random effects
arrows(x0= C_DistNear_Sp[C_DistNear_Sp_order] - C_DistNear_Sp_CI[C_DistNear_Sp_order], 
       x1= C_DistNear_Sp[C_DistNear_Sp_order] + C_DistNear_Sp_CI[C_DistNear_Sp_order],
       y0= DistNear.rank_2, y1=DistNear.rank_2, length = 0)

#To plot the main effect + random effect
DistNear.rank_2<-barplot(C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order], beside=T, horiz=T,
                         xlim=c(-0.06, 0.01), main="Distance to underpass",
                         names.arg=DistNear.SPrank,font.axis = 3)
abline(v=C_DistNear_Fx, col = "red") # main effect of DistNear
#Adding confidence intervals for fixed+random effects
arrows(x0=C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order] - C_DistNear_Sp_CI[C_DistNear_Sp_order], 
       x1=C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order] + C_DistNear_Sp_CI[C_DistNear_Sp_order],
       y0=DistNear.rank_2, y1=DistNear.rank_2, length = 0)

#To plot the random effect and fixed effect separately
DistNear.rank_2<-barplot(C_DistNear_Sp[C_DistNear_Sp_order], beside=T, horiz=T,
                         # xlim=C_DistNear_range, 
                         xlim=c(-0.06, 0.01), main="Distance to underpass",
                         names.arg=DistNear.SPrank,font.axis = 3)
abline(v=C_DistNear_Fx, col = "red") # main effect of DistNear
#Adding confidence intervals for random effects
arrows(x0= C_DistNear_Sp[C_DistNear_Sp_order] - C_DistNear_Sp_CI[C_DistNear_Sp_order], 
       x1= C_DistNear_Sp[C_DistNear_Sp_order] + C_DistNear_Sp_CI[C_DistNear_Sp_order],
       y0=DistNear.rank_2, y1=DistNear.rank_2, length = 0)
#Adding confidence intervals for fixed+random effects
arrows(x0=C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order] - C_DistNear_Sp_CI[C_DistNear_Sp_order], 
       x1=C_DistNear_Fx + C_DistNear_Sp[C_DistNear_Sp_order] + C_DistNear_Sp_CI[C_DistNear_Sp_order],
       y0=DistNear.rank_2, y1=DistNear.rank_2, length = 0)

