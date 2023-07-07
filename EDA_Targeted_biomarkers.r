# Exploratory Data Analysis for NUTRISHIELD Study II Q Trap Targeted Urine Sample

# Required library
# install required library
library(devtools)
devtools::install_github("sfirke/janitor")
devtools::install_github("vqv/ggbiplot", force = TRUE)
devtools::install_github("kassambara/ggcorrplot")
# load Required libraries
library(tidyverse) # meta package of all tidyverse packages
library(janitor)
library(ggplot2)
library(caret)
library(FactoMineR)
library(ggfortify)
library(factoextra)
library(reshape2)
library(ggbiplot)
library(ggcorrplot)

# Set Working Directory
setwd("C:/Users/Parth Doshi/Dropbox/Nutrishield_Study_II_Project (ParthD thesis)/R-script/EDA_for_Human_Mother_Milk")

# load Dataset
Targeted.biomarkers.data <- read.csv("C:/Users/Parth Doshi/Dropbox/Nutrishield_Study_II_Project (ParthD thesis)/Study2-Data/NUTRISHIELD_Study_II_QTrap_Targeted_Urine.csv", sep = ";", skip = 3)
str(Targeted.biomarkers.data)


# Data cleaning
# remove unwanted columns,
# Also filter columns the which has more 50% of value missing.

# Data Cleaning
Targeted.biomarkers.data <- Targeted.biomarkers.data %>%
  # Finding specific pattern to select the sample from data set
  select(Sample, contains(c("IU", "MU"))) %>%
  t() %>%
  as.data.frame() %>%
  row_to_names(1) %>%
  # converting variables to double
  mutate_if(names(.) != "Class", as.double) %>%
  rownames_to_column(var = "Samples")

head(Targeted.biomarkers.data)

#find the variable which has very less unique value and has same concentration through the data
unique_counts <- sapply(Targeted.biomarkers.data[2:29], function(x) length(unique(x)))
unique_counts
threshold <- 20
variables_with_few_unique <- names(unique_counts[unique_counts < threshold])
variables_with_few_unique

# Remove the variables which has very less unique values
Targeted.biomarkers.data <- Targeted.biomarkers.data %>%
  select(-`Kaempferol (umol/g creat)`,-`Daidzein (umol/g creat)`,-`Glycitein (umol/g creat)`,-`Genistein (umol/g creat)`) %>%
  as.data.frame()


# Data distribution 
Targeted.biomarkers.data.long <- gather(Targeted.biomarkers.data[3:25])
ggplot(Targeted.biomarkers.data.long, aes(x = value, y = key, fill = key)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) 

# Log transformation of the data
Targeted.biomarkers.data.normalise <- cbind(Targeted.biomarkers.data[1:2], log10(Targeted.biomarkers.data[3:25]))
summary(Targeted.biomarkers.data.normalise)



#Box plot 
Targeted.biomarkers.data.normalise.long <- gather(Targeted.biomarkers.data.normalise[3:25])
x11()
par(mfrow = c(2,2))
ggplot(Targeted.biomarkers.data.normalise.long, aes(x = value, y = key, fill = key)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) 

Targeted.biomarkers.data.normalise <- Targeted.biomarkers.data.normalise %>%
  select(-`Equol (umol/g creat)`,-`Quercetin (umol/g creat)`,-`3-IPA (umol/g creat)`) %>%
  as.data.frame()

Targeted.biomarkers.data2.normalise.long <- gather(Targeted.biomarkers.data.normalise[3:22])
x11()
par(mfrow = c(2,2))
ggplot(Targeted.biomarkers.data2.normalise.long, aes(x = key , y = value, fill = key)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)

#EDA
# Principle component Analysis
Targeted.biomarkers.Data.Normalise.pca <- prcomp(Targeted.biomarkers.data.normalise[3:22], scale. = FALSE)

#Visualization of PCA
fviz_pca_ind(Targeted.biomarkers.Data.Normalise.pca,
             geom = "point",
             habillage = Targeted.biomarkers.data.normalise$Class,
             palette = c("blue", "red","yellow"),
             addEllipses = TRUE,
             ellipse.type="confidence",
             ggtheme = theme_bw(),
             title = "PCA plot for Qtrap targeted biomarkrs")

#Scree plot 
fviz_eig(Targeted.biomarkers.Data.Normalise.pca, 
         addlabels = TRUE, 
         ylim = c(0, 70),
         main="Scree Plot Qtrap targeted Biomarkers")

#Graph for variable
fviz_pca_var(Targeted.biomarkers.Data.Normalise.pca, col.var = "red")
 
# biplot
biplot = ggbiplot(pcobj = Targeted.biomarkers.Data.Normalise.pca,
                  choices = c(1,2),
                  scale = 0,
                  varname.size = 2.5,
                  varname.abbrev = FALSE,  # Abbreviate variable names (TR
                  var.axes = TRUE,      # Remove variable vectors (TRUE)
                  circle = FALSE,        # Add unit variance circle (TRUE
                  ellipse = FALSE, groups = Targeted.biomarkers.data.normalise$Class) # Adding ellipses
x11()
print(biplot)


## Detecting Multicollinearity ##

#calculate  the correlation matrix
Corr.matrix <- as.data.frame(cor(Targeted.biomarkers.data.normalise[3:22]))
Corr.matrix

plot(Corr.matrix)
model = lm(`Phenylpropionylglycine (umol/g creat)` ~ Class , data = Targeted.biomarkers.data.normalise)
summary(model)
# variance inflation Factor (Are any > 5 or 10 )
library(car)
vif(model)
mean(vif(model))

# Split the  data in test an train
train_indices <- createDataPartition(Targeted.biomarkers.data.normalise$Class, p = 0.7, list = FALSE)
training_data <- Targeted.biomarkers.data.normalise[train_indices, -2]
testing_data <- Targeted.biomarkers.data.normalise[-train_indices, -2]










