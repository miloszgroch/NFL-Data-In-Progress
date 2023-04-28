#                                     WEIGHT AND HEIGHT ANALYSIS FOR POSITION
library(readxl)

Dane <- read_excel("C:/Users/groch/Desktop/Programowanie/R/nfl-big-data-bowl-2023/players2.xlsx")
Dane
Dane$weight <- as.numeric(Dane$weight)



#Convert inches to cm
Dane$height_inch <- as.numeric(gsub("-", ".", Dane$height)) * 12 + as.numeric(gsub(".*-", "", Dane$height))
Dane$height_cm <- Dane$height_inch * 2.54

#Creates list of positions
Positions <- unique(Dane$officialPosition)
Positions

#Loop that creates separate Data Frames for each position
for(position in Positions){
  assign(paste0("Dane_", position), Dane[Dane$officialPosition == position,])
}


#Creates vector of positions with mean of weight
Dane_weight <- sapply(Positions, function(x) mean(get(paste0("Dane_", x))$weight))
Dane_weight

#Barplot for weight across different American Football Positions
barplot(Dane_weight, names.arg=Positions, xlab="Positions", ylab="Mean Weight", col = rainbow(length(Dane_weight)))


# For any NFL fan, these results should not come as a surprise, 
# but we can see how relatively easy it is to analyze values 
# of this type with this code.
# From my perspective, it's interesting to see the clear weight difference 
# between QB and positions like WR or CB. The reason may be the likely average difference 
# in height in favor of QB, which will be the subject of further analysis.


#Creates vector of positions with mean of height
Dane_height <- sapply(Positions, function(x) mean(get(paste0("Dane_", x))$height_cm))
Dane_height

#Barplot for height across different American Football Positions
barplot(Dane_height, names.arg=Positions, xlab="Positions", ylab="Mean height", col = rainbow(length(Dane_height)))

#It's worth mentioning that before creating a barplot for height I had to change the height to centimeters
#That part of code was put above the loop that creates separate datasets for positions 

#Let's display two barplots horizontal and next to each other so we can analyze them easier 
par(mfrow=c(1,2), mar=c(5, 7, 4, 2) + 0.1, las=1)
barplot(Dane_weight, names.arg=Positions, xlab="Mean Weight", ylab="Positions", horiz = TRUE,col = rainbow(length(Dane_weight)))
barplot(Dane_height, names.arg=Positions, xlab="Mean height", ylab="Positions", horiz = TRUE,col = rainbow(length(Dane_height)))
#My earlier hypothesis that the higher weight of the quarterbacks than WR and CB may be due to greater height
#was supported by the second barplot. Another interesting thing that I noticed is a clear difference 
#in height in favor of Tackle compared to the other positions of the Offensive Line, i.e. Guard and Center, 
#despite the fact that the average weight is similar. For those more interested in the NFL, 
#however, it is clear that the best players playing as Tackle are more athletic, and their height 
#does not obscure the quaterback's field of view, as they are further to his flanks than the other linemen 
#a second after the snap. Examples of great Tackle include: Trent Williams i Tyron Smith