require(tidyverse)
require(magrittr)
require(readr)
require(stringr)
require(gridExtra)

# Loading and formatting the data: 

sample <- read.csv("T_NOR_all 6/sample sum-Table 1.csv", 
                   header = TRUE,
                   skip = 1)

sample2 <- sample[,grepl("No.|Run.time|Duration", colnames(sample))] %>% as_tibble()

NOR <- read.csv("T_NOR_all 6/NOR sum-Table 1.csv", 
                header = TRUE,
                skip = 1)

NOR2 <- NOR[,grepl("No.|Run.time|Duration", colnames(NOR))] %>% as_tibble()

NOR2 <- NOR2 %>% 
       filter (NOR$No. %in% intersect(NOR2$No., sample$No.)) %>% #Sieve out individuals that did not participate in both conditions
       arrange(No.) %>%
       rename(
         BlockDuration = Duration..s.,
         BallDuration = Duration..s..1,
         ArenaDuration = Duration..s..2
       )

sample2 <- sample2 %>% 
           arrange(No.) %>%
           rename(
             BlockDuration = Duration..s.,
             BallDuration = Duration..s..1,
             ArenaDuration = Duration..s..2
            )

#Get animal strains:

animalList <- read.csv("180223_T_aSyn3_animal_data/Animal List-Table 1.csv") %>%
              as_tibble()

animalList <- animalList[,grepl("ID|strain", colnames(animalList))] %>% head(-3)

#Use regular expressions to extract the individual number to match with the notation of the other file:

animalList$animal.ID..Ear.. <- regmatches(animalList$animal.ID..Ear.., gregexpr("(?<=\\().*?(?=\\))", animalList$animal.ID..Ear.., perl=T)) %>% 
                               unlist() 

animalList$animal.ID..Ear.. %<>% as.integer

animalList$strain.%<>% as.character

animalList2 <- animalList %>% 
               filter(animalList2$animal.ID..Ear.. %in% intersect(animalList2$animal.ID..Ear.., sample$No.)) %>%
               arrange(animal.ID..Ear..)

#Binding it all together:

sample2 <- bind_cols(sample2, animalList2[,2])
NOR2 <- bind_cols(NOR2, animalList2[,2])


#===================================================================================================================


#Plotting results:

#Scatterplot Ballduration/Blockduration in the two conditions:

plot1 <- ggplot(data = NOR2, mapping = aes(x = BlockDuration, y = BallDuration, colour = strain.)) +
  geom_point() +
  ggtitle("NOR data") +
  geom_smooth(method = "lm") +
  labs(colour = "Strain")

plot2 <- ggplot(data = sample2, mapping = aes(x = BlockDuration, y = BallDuration, colour = strain.)) +
  geom_point() +
  ggtitle("Sample data") +
  geom_smooth(method = "lm") +
  labs(colour = "Strain")

#Percentage of Time Ball explored against total exploration time 
#(again, in both conditions) per individual:

plot3 <- ggplot(data = NOR2, mapping = aes(x = No., y = BallDuration / ArenaDuration, colour = strain.)) +
        geom_point() +
        geom_line() +
        labs(x = "Individual", colour = "Strain") 

plot4 <- ggplot(data = sample2, mapping = aes(x = No., y = BallDuration / ArenaDuration, colour = strain.)) +
  geom_point() +
  geom_line() +
  labs(x = "Individual", colour = "Strain")

#Difference between fractions Ballexplored / total time explored across the two conditions:

plot5 <- ggplot(data = NOR2, mapping = aes(x = No., y = - BallDuration / ArenaDuration - sample2$BallDuration / sample2$ArenaDuration)) +
  geom_point(colour = "violet") +
  geom_line(colour = "violet")

grid.arrange(
  plot1, 
  plot2,
  plot3,
  plot4,
  nrow = 2,
  ncol = 2,
  top = "Nor experimental data")

 #==========================================================================

#Testing:

#Difference score & Wilcox test (from Besheer & Bevins):

#Test also for different strains (later):

x <- sample2 %>% group_split(strain.)
y <- NOR2 %>% group_split(strain.)

#[[1]] = C57Bl/6J, [[2]] = Fra2 L62-s1
  
diff <- function(data){data$BallDuration - data$BlockDuration}
  
wStrain1 <- wilcox.test(diff(x[[1]]), diff(y[[1]]), paired = TRUE)
wStrain2 <- wilcox.test(diff(x[[2]]), diff(y[[2]]), paired = TRUE)

print(wStrain1)
print(wStrain2)


#Answer: the Wilcoxon test shows that, for both strains, there is a significant difference
#between how much time animals spend exploring novel (as opposed to familiar) objects
#in the test condition. However, the test also reveals that there may be quite a difference
#between the two strains (this is also indicated by B&B). I'll run a Kruskal-test to
#check this.

kSample <- kruskal.test((BallDuration - BlockDuration) ~ strain., data = sample2)
kNOR <- kruskal.test((BallDuration - BlockDuration) ~ strain., data = NOR2)


print(kSample)
print(kNOR)

#However, the test does not reveal significant differences between the strains in either condition














  
  
  
 



              




             
             
    
           
























                
                





























      
