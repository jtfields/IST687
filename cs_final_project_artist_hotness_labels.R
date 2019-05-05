setwd("C:/Users/ceeju/OneDrive/Syracuse/IST-687 - Applied Data Science (Prof. Santerre)/Final Project")
music <- read.csv(file="music.csv")

str(music)


##Artist Hotness Histogram
library(ggplot2)
ggplot(music, aes(x=artist.hotttnesss))  + geom_histogram(color="black", fill="steelblue", alpha=0.5) + xlab("Artist Hotness") + ggtitle("Histogram: Artist Hotness")



##Function to create descriptive statistics for artist hotness
descriptive_stats <- function(vector) { library(moments) 
  result <- c(Mean=mean(vector),
            Median=median(vector),
            Min = min(vector),
            Max = max(vector),
            SD = sd(vector),
            Quantile = quantile(vector, probs = c(0.25,.50,0.75, 0.95)),
            Skewness = skewness(vector) )
print(result) 
}

descriptive_stats(music$artist.hotttnesss)

##Methodology for assigning artist hotness levels - uses quantiles from descriptitive_statistics function
95% Quantile: 0.6011861 - Hot
75% Quantile: 0.453858  - Warm
50% Quantile: 0.3807423 - Tepid
25% Quantile: 0.3252656 - Cool


##Code for assigning labels based on above quantiles
music$artist.hotness.label <- ifelse(music$artist.hotttnesss >=0.6011861, "Hot",
                                     ifelse(music$artist.hotttnesss >=0.453858 & music$artist.hotttnesss <0.6011861, "Warm",
                                     ifelse(music$artist.hotttnesss >=0.3807423 & music$artist.hotttnesss <0.453858, "Tepid",
                                     ifelse(music$artist.hotttnesss >=0.3252656 & music$artist.hotttnesss <0.3807423, "Cool",
                                     ifelse(music$artist.hotttnesss < 0.3252656, "Frigid","Else")))))

unique(music$artist.hotness.label)
