# below are the packages used in the rarefaction analyses
library(vegan)
library(ggplot2)
library(directlabels)
library(ggtext)
library(tidyverse)
library(cowplot)
library(glue)
library(iNEXT)
library(devtools)
library(httr)
library("dplyr")
library("ggpubr")

# At first, generate all the rafefaction curves with INEXT package, so load the necessary packages then
# then generate the fern rarefaction curves to see the trends in the upland locality
# first read the data from the archive "tab raw data upland" choose the sheet 

table1 <- read.table("clipboard", header = TRUE)

out <- iNEXT(table1, q=0, conf=0.95, datatype="abundance")

plant_assemblage <- fortify(out, type=1)

head(plant_assemblage)

df.point <- plant_assemblage[which(plant_assemblage$method=="observed"),]
df.line <- plant_assemblage[which(plant_assemblage$method!="observed"),]
df.line$method <- factor(df.line$method, 
                         c("interpolated", "extrapolated"),
                         c("interpolation", "extrapolation"))


ggplot(plant_assemblage, aes(x=x, y=y, colour=site)) + 
  geom_point(aes(shape=site), size=2, data=df.point) +
  geom_line(aes(linetype=method), lwd=1, data=df.line) +
  scale_color_manual(values=c('#d7191c','#fdae61','yellow','black','#2b83ba')) +
  geom_ribbon(aes(ymin=y.lwr, ymax=y.upr,
                  fill=site, colour = NULL), alpha=0.2) +
scale_fill_manual(values=c("#d9d9d9", "#d9d9d9", "#d9d9d9", "#d9d9d9", "#d9d9d9")) +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                    size = 0.1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                     colour = "white")) +
labs(x="Number of individuals", y="Fern diversity") +
  theme(axis.title = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  annotate(geom = "text", x = 46 , y =12.8 , label = "UL",
           size = 1.8,  hjust = "right") +
  annotate(geom = "text", x = 7 , y =7.4 , label = "750 m",
           size = 1.8,  hjust = "right") +
  annotate(geom = "text", x = 20.8 , y =5.1 , label = "790 m",
           size = 1.8,  hjust = "right") +
  annotate(geom = "text", x = 29 , y = 4.2, label = "910 m",
           size = 1.8,  hjust = "right") +
  theme(legend.position = "none")

ggsave("Figure 8.pdf", path = "C:/Users/Romulo/Desktop/Backup/Documents/Doutorado Unisinos/paper 1/Revision received date February 5/Figures", 
       width=8.6, height=7, units = "cm", dpi=600)


# select with crtl + c only the data without headers and column names,, only the 0 and 1 cells...
# this is the plot of DT rarefaction curve with all leaves included


table1 <- read.table("clipboard", header = TRUE)


d <- # create a new object called "d"
  c(nrow(table1), # which begins with the total number of specimens
    as.numeric(colSums(table1[,2:ncol(table1)])) ) # and then contains the number of specimens on which each DT was observed

out <- iNEXT(d, q=0, conf=0.95, datatype="incidence_freq", nboot = 100)#iNextEst 
plotDT <- ggiNEXT(out, type = 2) +  scale_color_manual(values="forestgreen") +
  scale_fill_manual(values="#d9d9d9") +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                   size = 0.1, linetype = "solid"),
             panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                     colour = "white")) +
  theme(axis.title = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(legend.position = "none")
plotDT

ggsave("Figure 9.pdf", path = "C:/Users/Romulo/Desktop/Backup/Documents/Doutorado Unisinos/paper 1/Revision received date February 5/Figures", 
       width=8.6, height=7, units = "cm", dpi=600)

# that's the end of rarefaction of the upland locality of DTs

# now the tests is applied in the data by elevation site

# to calculate and accumulate the data I copy and paste each elvation site by filtering in the excel data sheet, 
# In this way it is needed to generate separated objects in R for each site and plot after this steps 

table903 <- read.table("clipboard", header = FALSE)
table875 <- read.table("clipboard", header = FALSE)
table803 <- read.table("clipboard", header = FALSE)
table749 <- read.table("clipboard", header = FALSE)

d1 <- # create a new object called "d"
  c(nrow(table903), # which begins with the total number of specimens
    as.numeric(colSums(table903[,2:ncol(table903)])) ) # and then contains the number of specimens on which each DT was observed

d2 <- # create a new object called "d"
  c(nrow(table875), # which begins with the total number of specimens
    as.numeric(colSums(table875[,2:ncol(table875)])) ) # and then contains the number of specimens on which each DT was observed

d3 <- # create a new object called "d"
  c(nrow(table803), # which begins with the total number of specimens
    as.numeric(colSums(table803[,2:ncol(table803)])) ) # and then contains the number of specimens on which each DT was observed

d4 <- # create a new object called "d"
  c(nrow(table749), # which begins with the total number of specimens
    as.numeric(colSums(table749[,2:ncol(table749)])) ) # and then contains the number of specimens on which each DT was observed

dsites <- cbind(d1,  d2, d3, d4)

# calculating the richness Indexes and confidence interval with INEXT for each elevation site
outsites <- iNEXT(dsites, q=0, conf=0.95, datatype="incidence_freq", nboot = 100)

plotsites <- ggiNEXT(outsites, type = 2) +  scale_color_manual(values=c('#d7191c','#fdae61','yellow','black')) +
  scale_fill_manual(values=c("#d9d9d9", "#d9d9d9", "#d9d9d9", "#d9d9d9")) +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 0.1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white")) +
  annotate(geom = "text", x = 220 , y =0.77 , label = "903 m",
           size = 1.8,  hjust = "right") +
  annotate(geom = "text", x = 285 , y =0.92 , label = "875 m",
           size = 1.8,  hjust = "right") +
  annotate(geom = "text", x = 88 , y =0.77 , label = "803 m",
           size = 1.8,  hjust = "right") +
  annotate(geom = "text", x = 160 , y = 0.77, label = "749 m",
           size = 1.8,  hjust = "right") +
  theme(axis.title = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(legend.position = "none") +
  geom_segment(aes(x = 50, y = 0.94, xend = 60, yend = 0.8), linetype= 'dashed', size=0.1, colour='black') +
  geom_segment(aes(x = 127, y = 1, xend = 135, yend = 0.8), linetype= 'dashed', size=0.1, colour='black') +
  geom_segment(aes(x = 184, y = 1, xend = 200, yend = 0.8), linetype= 'dashed', size=0.1, colour='black')
  
plotsites
ggsave("Figure 9.pdf", path = "C:/Users/Romulo/Desktop/Backup/Documents/Doutorado Unisinos/paper 1/Revision received date February 5/Figures", 
       width=8.6, height=7, units = "cm", dpi=600)

# The most abundant fern species in leaves are analysed in rarefaction, we provide the two most abundant epiphytic and terrestrial fern species
# The same is applied to the above rarefaction by elevation site, but organizing the species only in the spectra of elevation of 903-749 meters
# don't forget to selected only the cells with 0 and 1 values, and filtering the data by the most abundant fern species

#terrestrial ferns

tableM_oreocharis <- read.table("clipboard", header = FALSE)
tableD_sellowiana <- read.table("clipboard", header = FALSE)

#epiphytic ferns

tableC_austrabrasilianum <- read.table("clipboard", header = FALSE)
tableN_crassifolium <- read.table("clipboard", header = FALSE)

dM <- # create a new object called "d"
  c(nrow(tableM_oreocharis), # which begins with the total number of specimens
    as.numeric(colSums(tableM_oreocharis[,2:ncol(tableM_oreocharis)])) ) # and then contains the number of specimens on which each DT was observed

dD <- # create a new object called "d"
  c(nrow(tableD_sellowiana), # which begins with the total number of specimens
    as.numeric(colSums(tableD_sellowiana[,2:ncol(tableD_sellowiana)])) ) # and then contains the number of specimens on which each DT was observed

dC <- # create a new object called "d"
  c(nrow(tableC_austrabrasilianum), # which begins with the total number of specimens
    as.numeric(colSums(tableC_austrabrasilianum[,2:ncol(tableC_austrabrasilianum)])) ) # and then contains the number of specimens on which each DT was observed

dN <- # create a new object called "d"
  c(nrow(tableN_crassifolium), # which begins with the total number of specimens
    as.numeric(colSums(tableN_crassifolium[,2:ncol(tableN_crassifolium)])) ) # and then contains the number of specimens on which each DT was observed

dspecies <- cbind(dM,  dD, dC, dN)

# calculating the richness and confidence interval with vegan package wing accumulate method for each elevational site
outspecies <- iNEXT(dspecies, q=0, conf=0.95, datatype="incidence_freq", nboot = 100)
warnings()
# plotting the data with ggplot

plotspecies <- ggiNEXT(outspecies, type = 2) +  scale_color_manual(values=c('#a6611a','#dfc27d','#80cdc1','#018571')) +
  scale_fill_manual(values=c("#d9d9d9", "#d9d9d9", "#d9d9d9", "#d9d9d9")) +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 0.1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white")) +
  theme(axis.title = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  theme(legend.position = "none") +
  annotate(geom = "text", x = 122 , y =0.86 , label = "CA", size = 2.5, fontface = "italic",  hjust = "right") +
  annotate(geom = "text", x = 237 , y =0.89, label = "MO", size = 2.5, fontface = "italic",  hjust = "right") +
  annotate(geom = "text", x = 94 , y = 0.86 , label = "NC", size = 2.5, fontface = "italic",  hjust = "right") +
  annotate(geom = "text", x = 57 , y =0.86 , label = "DS", size = 2.5, fontface = "italic",  hjust = "right") +
  geom_segment(aes(x = 51, y = 1.003, xend = 52, yend = 0.88), linetype= 'dashed', size=0.1, colour='black') +
  geom_segment(aes(x = 65, y = 1.003, xend = 87, yend = 0.88), linetype= 'dashed', size=0.1, colour='black') +
  geom_segment(aes(x = 79, y = 1.003, xend = 112, yend = 0.88), linetype= 'dashed', size=0.1, colour='black')
  
plotspecies
 

# now the plot using vegan package for comparison of rarefaction curve of accumulation method
# with rarefaction curves of sample-coverage method used in Schachat
# select with crtl + c only the data without header and column names,, only the 0 and 1 cells...
# for comparison purposes we added the interaction with minimal preservation potential in the matrix to compare with the only DT rarefaction curve

table2 <- read.table("clipboard", header = TRUE)

# to calculate and accumulate the data

plotupland <-specaccum(table2, method = "rarefaction", permutations = 100, conditioned =TRUE, gamma = "jack1",  w = NULL) 

plot(plotupland)

#creating a dataframe for ggplot2 thats reads only data frame format
plotupland1 <- data.frame(Sites=plotupland$sites, Richness=plotupland$richness, SD=plotupland$sd)

#plotting the results
plotupland <- ggplot() +
  geom_point(data=plotupland1, aes(x=Sites, y=Richness), col="forestgreen") +
  geom_line(data=plotupland1, aes(x=Sites, y=Richness), col="forestgreen") +
  geom_ribbon(data=plotupland1,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2) +
  labs(y="damage types", x = "Leaves sampled") +
theme(panel.background = element_rect(fill = "white", colour = "black",
                     size = 0.1, linetype = "solid"),
                panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                    colour = "white")) + 
  theme(axis.title = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) 
plotupland

# that's the end of rarefaction of the upland locality

# now the tests is applied in the data by elevation site

# to calculate and accumulate the data I copy and paste each elvation site by filtering in the excel data sheet, 
# In this way it is needed to generate separated objects in R for each site and plot after this steps 

table903 <- read.table("clipboard", header = FALSE)
table875 <- read.table("clipboard", header = FALSE)
table803 <- read.table("clipboard", header = FALSE)
table749 <- read.table("clipboard", header = FALSE)


# calculating the richness and confidence interval with vegan package wing accumulate method for each elevation site
plotupland903 <-specaccum(table903, method = "rarefaction", permutations = 100, conditioned =TRUE, gamma = "jack1",  w = NULL) 
plotupland875 <-specaccum(table875, method = "rarefaction", permutations = 100, conditioned =TRUE, gamma = "jack1",  w = NULL) 
plotupland803 <-specaccum(table803, method = "rarefaction", permutations = 100, conditioned =TRUE, gamma = "jack1",  w = NULL) 
plotupland749 <-specaccum(table749, method = "rarefaction", permutations = 100, conditioned =TRUE, gamma = "jack1",  w = NULL) 

plot(plotupland903) # check if it worked properly

#creating a dataframe by site for ggplot2 thats reads only data frame format
site903 <- data.frame(Sites=plotupland903$sites, Richness=plotupland903$richness, SD=plotupland903$sd)
site875 <- data.frame(Sites=plotupland875$sites, Richness=plotupland875$richness, SD=plotupland875$sd)
site803 <- data.frame(Sites=plotupland803$sites, Richness=plotupland803$richness, SD=plotupland803$sd)
site749 <- data.frame(Sites=plotupland749$sites, Richness=plotupland749$richness, SD=plotupland749$sd)

# plotting the data with ggplot
# Use the same colors used in the FIgure 9C ('#d7191c','#fdae61','yellow','black')

plotuplandsites <- ggplot() +
  geom_point(data=site903, aes(x=Sites, y=Richness), col='#d7191c') +
  geom_line(data=site903, aes(x=Sites, y=Richness), col='#d7191c') +
  geom_ribbon(data=site903,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2) +
  geom_point(data=site875, aes(x=Sites, y=Richness), col='#fdae61') +
  geom_line(data=site875, aes(x=Sites, y=Richness), col='#fdae61') +
  geom_ribbon(data=site875,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2) +
  geom_point(data=site803, aes(x=Sites, y=Richness), col='yellow') +
  geom_line(data=site803, aes(x=Sites, y=Richness), col='yellow') +
  geom_ribbon(data=site803,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2) +
  geom_point(data=site749, aes(x=Sites, y=Richness), col='black') +
  geom_line(data=site749, aes(x=Sites, y=Richness), col='black') +
  geom_ribbon(data=site749,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2) +
  labs(y="damage types", x = "Leaves sampled") +
  theme(axis.title = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
theme(panel.background = element_rect(fill = "white", colour = "black",
                                      size = 0.1, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white")) +
  annotate(geom = "text", x = 80 , y = 6.7 , label = "903 m",
           size = 1.8,  hjust = "right") +
  annotate(geom = "text", x = 230 , y =6.9 , label = "875 m",
           size = 1.8,  hjust = "right") +
  annotate(geom = "text", x = 45 , y =9.25 , label = "803 m",
           size = 1.8,  hjust = "right") +
  annotate(geom = "text", x = 120 , y = 10.6, label = "749 m",
           size = 1.8,  hjust = "right")
plotuplandsites

# The most abundant fern species in leaves are analysed in rarefaction, we provide the two most abundant epiphytic and terrestrial fern species
# The same is applied to the above rarefaction by elevation site, but organizing the species only in the spectra of elevation of 903-749 meters
# don't forget to selected only the cells with 0 and 1 values, and filtering the data by fern species chosen

#terrestrial ferns

tableM_oreocharis <- read.table("clipboard", header = FALSE)
tableD_sellowiana <- read.table("clipboard", header = FALSE)

#epiphytic ferns

tableC_austrabrasilianum <- read.table("clipboard", header = FALSE)
tableN_crassifolium <- read.table("clipboard", header = FALSE)



# calculating the richness and confidence interval with vegan package wing accumulate method for each elevational site
plotuplandC_austrabrasilianum <-specaccum(tableC_austrabrasilianum, method = "rarefaction", permutations = 100, conditioned =TRUE, gamma = "jack1",  w = NULL) 
plotuplandM_oreocharis <-specaccum(tableM_oreocharis, method = "rarefaction", permutations = 100, conditioned =TRUE, gamma = "jack1",  w = NULL) 
plotuplandN_crassifolium <-specaccum(tableN_crassifolium, method = "rarefaction", permutations = 100, conditioned =TRUE, gamma = "jack1",  w = NULL) 
plotuplandD_sellowiana <-specaccum(tableD_sellowiana, method = "rarefaction", permutations = 100, conditioned =TRUE, gamma = "jack1",  w = NULL) 

plot(plotuplandC_austrabrasilianum)

#creating a dataframe by site for ggplot2 thats reads only data frame format
C_austrabrasilianum <- data.frame(Sites=plotuplandC_austrabrasilianum$sites, Richness=plotuplandC_austrabrasilianum$richness, SD=plotuplandC_austrabrasilianum$sd)
M_oreocharis <- data.frame(Sites=plotuplandM_oreocharis$sites, Richness=plotuplandM_oreocharis$richness, SD=plotuplandM_oreocharis$sd)
N_crassifolium <- data.frame(Sites=plotuplandN_crassifolium$sites, Richness=plotuplandN_crassifolium$richness, SD=plotuplandN_crassifolium$sd)
D_sellowiana <- data.frame(Sites=plotuplandD_sellowiana$sites, Richness=plotuplandD_sellowiana$richness, SD=plotuplandD_sellowiana$sd)


# plotting the data with ggplot
# use the same colors of figure 9E ('#a6611a','#dfc27d','#80cdc1','#018571')
plotuplandspecies <- ggplot() +
  geom_point(data=C_austrabrasilianum , aes(x=Sites, y=Richness), col='#80cdc1') +
  annotate(geom = "text", x = 210 , y =3.3 , label = "CA", size = 2.5, fontface = "italic",  hjust = "right") +
  geom_line(data=C_austrabrasilianum , aes(x=Sites, y=Richness), col='#80cdc1') +
  geom_ribbon(data=C_austrabrasilianum ,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2) +
  geom_point(data=M_oreocharis, aes(x=Sites, y=Richness), col='#a6611a') +
  annotate(geom = "text", x = 78 , y =5.7 , label = "MO", size = 2.5, fontface = "italic",  hjust = "right") +
  geom_line(data=M_oreocharis, aes(x=Sites, y=Richness), col='#a6611a') +
  geom_ribbon(data=M_oreocharis,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2) +
  geom_point(data=N_crassifolium, aes(x=Sites, y=Richness), col='#018571') +
  annotate(geom = "text", x = 57 , y =3.25 , label = "NC", size = 2.5, fontface = "italic",  hjust = "right") +
  geom_line(data=N_crassifolium, aes(x=Sites, y=Richness), col='#018571') +
  geom_ribbon(data=N_crassifolium,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2) +
  geom_point(data=D_sellowiana , aes(x=Sites, y=Richness), col='#dfc27d') +
  annotate(geom = "text", x = 45 , y =2.3 , label = "DS", size = 2.5, fontface = "italic",  hjust = "right") +
  geom_line(data=D_sellowiana , aes(x=Sites, y=Richness), col='#dfc27d') +
  geom_ribbon(data=D_sellowiana ,aes(x=Sites, ymin=(Richness-2*SD),ymax=(Richness+2*SD)),alpha=0.2) +
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 0.1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white")) +
  theme(axis.title = element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(axis.text.y = element_text(size = 8)) +
  labs(y="damage types", x = "Leaves sampled")
plotuplandspecies

# Saving all plots in one plate with package cowplot
# All data is in the archive "Supplemental data - tab raw data upland"


plot_grid(plotDT, plotupland, plotsites, plotuplandsites, plotspecies, plotuplandspecies, nrow=3, ncol=2,
          labels=c("A", "B", "C", "D", "E", "F"))


ggsave("Figure 9.pdf", path = "C:/Users/Romulo/Desktop/Backup/Documents/Doutorado Unisinos/paper 1/Revision received date February 5/Figures", 
       width=17.4, height=22, units = "cm", dpi=600)










# for comparison purposes we added the interaction with minimal preservation potential in the matrix to compare with the only DT rarefaction curve