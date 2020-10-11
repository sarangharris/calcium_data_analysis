setwd("/Users/sarang/Programming/Projects/R/Plotting Calcium data")
source("graphs_from_zscores.R")



# looping through all files in a directory

file.names <- list.files(path = "/Users/sarang/Programming/Projects/R/Plotting Calcium data/GCaMP7_Insula_Restraint_05232019", pattern = "Zscore).csv", recursive = TRUE, full.names = TRUE)


# creating empty df that will become df with all movements
allMovements <- data.frame(
  Time=numeric(),
  movementType=character(),
  date=numeric(),
  subj_num=numeric(),
# type=character(),
  stringsAsFactors=FALSE
)

# will individually store all data 
movementList <- list()

i=1
for(i in 1:length(file.names)) {
  
  file_location <- file.names[i]
  print(file_location)
  # Create column with subj number and date
  subj_num <- subj_num_fx("Subject(.*?)-", file_location)
  
  # Create column with subj number and date
  subj_date <- subj_date_fx("Restraint-(.*?)/", file_location)
  
  # Create column with subj number and date
  movementType <- movement_type_fx("Subject(.*?).csv", "/(.*?)\\(Zscore\\)", file_location)


  # read in data
  movement <- read_alignments(file_location, subj_num, movementType, subj_date)
  
  # add data to list
  movementList[[i]] <- movement 
   
}
  
# bind all data together 
allMovements = do.call(rbind, movementList)

allMovements2 <- allMovements %>% filter(Z_score < 25 & Z_score > -15)

allMovements2$boutID <- paste(allMovements2$subj_num, allMovements2$date, allMovements2$boutNum)

day1Large <- filter(allMovements2, date == 190517, movementType == "WholeBody")


limit <- c(-15,15)

y=boutNum

ggplot(day1Large, aes(x=Time, y=boutID, fill=Z_score))+
  geom_tile()+
  scale_fill_distiller(palette = "RdBu")


testData <- filter(allMovements, date <= 190518, subj_num < 5)

gg <- ggplot(allMovements, aes(x=Time, y=boutNum, fill=Z_score))
gg <- gg + geom_tile(aes(fill = Z_score))
gg <- gg + scale_fill_gradient2(limits = limit, low = muted("blue"), mid = "white", high = muted("red"), midpoint = 0, space = "Lab", guide = "colourbar", aesthetics = "fill")
gg <- gg + facet_grid(rows = vars(date))
gg <- gg + facet_grid(cols = vars(movementType))
gg <- gg + facet_grid(vars(date), vars(movementType))
gg <- gg + facet_grid(. ~movementType)
gg <- gg + facet_grid(. ~date)
gg <- gg + facet_grid(date ~ movementType)
gg <- gg + labs(x=NULL, y=NULL, title="Day 1 Large\n")
gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(axis.text.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(plot.title=element_text(hjust=0.5))
gg <- gg + theme(strip.text=element_blank())
gg <- gg + theme(panel.spacing.x=unit(1, "lines"))
gg <- gg + theme(panel.spacing.y=unit(1, "lines"))
gg <- gg + theme(legend.title=element_text(size=8))
gg <- gg + theme(legend.title.align=1)
gg <- gg + theme(legend.text=element_text(size=8))
gg <- gg + theme(legend.position="bottom")
gg <- gg + theme(legend.key.size=unit(0.5, "cm"))
gg <- gg + theme(legend.key.width=unit(2, "cm"))
gg

  