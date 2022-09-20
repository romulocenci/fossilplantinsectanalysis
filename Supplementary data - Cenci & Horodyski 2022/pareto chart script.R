# Search for the excel archive "Suplemental data - tab raw data upland.xlsx" the sheet FFG abundances in the supplemental data link provided by Palaios, open and select all data included in hte table
# I use the command ctrl + C (Copy) of the data with column names selected, after I press ctrl + V (Paste) in the script and the table1 appears in R

table1 <- read.table("clipboard", header = TRUE)

# load these following packages
# From here, the code was edited, but the basis if from this website ("https://rstudio-pubs-static.s3.amazonaws.com/72023_670962b57f444c04999fd1a0a393e113.html")
suppressPackageStartupMessages(library(dplyr))

# You need transform your data  to obtain % and freq values, after you can generate your plot

paretochart <- arrange(table1, desc(totals)) %>%
  mutate(
    cumsum = cumsum(totals),
    freq = round(totals / sum(totals), 3),
    cum_freq = cumsum(freq)
  )
paretochart
write.csv(paretochart, "paretochart.csv")

## ## R code to generate Pareto Chart (version 2)

## Saving Parameters 
def_par <- par() 

# upland paretochart, I added commands to save your figure
pdf(file="location where you want to save your file", 
     height=3.937, width=3.5433)
# New margins
par(mar=c(6,5,1,5)) 

## plot bars, pc will hold x values for bars
pc = barplot(paretochart$totals,
             width = 1, space = 0.2, border = NA, axes = F,
             ylim = c(0, 1.05 * max(paretochart$totals, na.rm = T)), 
             cex.names = 0.5, 
             names.arg = c("margin feeding", "piercing-and-sucking", "hole feeding", "surface feeding", "mining", "hole feeding", "galling"), 
             las=2, mgp=c(2,.35,0))

## anotate left axis
axis(side = 2, at = c(0, paretochart$totals), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.6)

## frame plot
box( col = "grey62")

## Cumulative Frequency Lines 
px <- paretochart$cum_freq * max(paretochart$totals, na.rm = T)
lines(pc, px, type = "b", cex = 0.6, pch = 19, col="cyan4")

## Annotate Right Axis
axis(side = 4, at = c(0, px), labels = paste(c(0, round(paretochart$cum_freq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.6, col.axis = "cyan4")
dev.off()
## restoring default paramenter
par(def_par)


# that's the end of the FFG pareto chart plot

# For generate the DT pareto chart, the same above is applied... only the margins I edited due to the data is more large and fits better with more width
# Also I edited the names.arg to change the labels of FFG row names to DTs names
# Search for the excel archive "Suplemental data - tab raw data upland.xlsx" the sheet DTs abundances
# First of all, reading the source of data with copy and paste command, COPY ONLY THE 2ND AND 3RD COLUMN
table1 <- read.table("clipboard", header = TRUE)

# You need transform your data again to obtain % and freq values, after you can generate your plot

paretochart <- arrange(table1, desc(totals)) %>%
  mutate(
    cumsum = cumsum(totals),
    freq = round(totals / sum(totals), 3),
    cum_freq = cumsum(freq)
  )
paretochart
write.csv(paretochart, "paretochart.csv")


## ## R code to generate Pareto Chart (version 2)

## Saving Parameters 
def_par <- par() 

# upland paretochart, I added commands to save your figure
pdf(file="location where you want to save your file", 
    height=3.937, width=6.2992)
# New margins
par(mar=c(4,3,2,3)) 

## plot bars, pc will hold x values for bars
pc1 = barplot(paretochart$totals,
             width = 1, space = 0.2, border = NA, axes = F,
             ylim = c(0, 1.05 * max(paretochart$totals, na.rm = T)), 
             cex.names = 0.5, 
             names.arg = c("DT13", "DT47", "DT103", "DT352", "DT31", "MMP", "DT15", "DT14", "DT115", "DT01", "DT123", "DT233", "DT12",
                           "DT191", "DT92", "DT07", "DT68", "DT410"), las=2, mgp=c(2,.35,0))

## anotate left axis
axis(side = 2, at = c(0, paretochart$totals), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.6)

## frame plot
box( col = "grey62")

## Cumulative Frequency Lines 
px <- paretochart$cum_freq * max(paretochart$totals, na.rm = T)
lines(pc1, px, type = "b", cex = 0.6, pch = 19, col="cyan4")

## Annotate Right Axis
axis(side = 4, at = c(0, px), labels = paste(c(0, round(paretochart$cum_freq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.6, col.axis = "cyan4")
dev.off()

# that's the end of the DT pareto chart plot
# Now, we will do the same thing with all interaction found in the locality, including the interaction with minimal preservation potential

# First of all, reading the source of data with copy and paste command, 
# COPY ONLY THE 1ST AND 3ST COLUMN from the archive "Supplemental data - tab raw upland data" the sheet interaction abundances
table1 <- read.table("clipboard", header = TRUE)

# You need transform your data again to obtain % and freq values, after you can generate your plot

paretochart <- arrange(table1, desc(totals)) %>%
  mutate(
    cumsum = cumsum(totals),
    freq = round(totals / sum(totals), 3),
    cum_freq = cumsum(freq)
  )
paretochart
write.csv(paretochart, "paretochart.csv")

## ## R code to generate Pareto Chart (version 2)

## Saving Parameters 
def_par <- par() 

# upland paretochart of all interactions, I added commands to save your figure
tiff(filename="location where you want to save your file", height=10, width=16, units='cm', compression="lzw", res=1000)
# New margins
par(mar=c(4,3,2,3)) 

## plot bars, pc will hold x values for bars
pc = barplot(paretochart$totals,
             width = 1, space = 0.2, border = NA, axes = F,
             ylim = c(0, 1.05 * max(paretochart$totals, na.rm = T)), 
             cex.names = 0.5, 
             names.arg = c("DT13", "DT47", "DT103", "DT352", "DT31", "MMP", "DT15", "DT14", "DT120", "DT06", "DT123", "DT12",
                           "DT191", "DT92", "DT07", "DT68", "DT410"), las=2)

## anotate left axis
axis(side = 2, at = c(0, paretochart$totals), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.6)

## frame plot
box( col = "grey62")

## Cumulative Frequency Lines 
px <- paretochart$cum_freq * max(paretochart$totals, na.rm = T)
lines(pc, px, type = "b", cex = 0.6, pch = 19, col="cyan4")

## Annotate Right Axis
axis(side = 4, at = c(0, px), labels = paste(c(0, round(paretochart$cum_freq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.6, col.axis = "cyan4")
dev.off()

# This last graphic includes all interactions, and includes the interactions with minimal preservation potential either that represents 5% of all interactions,
#however it is a supplemental plot provided only here since only DTs is analysed in the fossil record 
# That's the end of pareto chart graphics
