######################### TASK 1 #########################

library(tidyverse)

#read.csv or read_csv will both do the same thing.
#read_csv will do it quicker and assume there is
#a header and will work well with tidy verse

filehdi <- "Human-development-index.csv"

#WHAT IS THE DIFFERENCE BETWEEN THESE?!?!?!??!?!?!?!?!?!!?
# hdi <- read_csv("Human-development-index.csv")
# hdi

hdi2 <- read_csv(filehdi) %>% 
  janitor::clean_names()

#Tidy data
hdi3 <- hdi2 %>% 
  pivot_longer(names_to = "year", 
               values_to = "value",
               cols = -c(hdi_rank_2018, country))
#this has taken my data from 212 obvs to 6360 obvs
#this has re-arranged the data such that there are now only 4 columns rather than 32.
#the data is said to be tidy

#! is not

hdi_no_na <- filter(hdi3, !is.na(value))

#to get the mean value by country
hdi_summary <- hdi_no_na %>% 
  group_by(country) %>% 
  summarise(mean_value = mean(value))

#We can add summary columns by adding the code needed to create them in to the summarise() function.
#So for example, to add the number of indices available to each column we use:
hdi_summary <- hdi_no_na %>% 
  group_by(country) %>% 
  summarise(mean_value = mean(value),
            n = length(value))
#this just added an "n" column to the summary to show how many values has gone into the mean that has been generated

#Add columns for the standard deviation and the standard error (S.E.=s.d.n√) to the hdi_summary
hdi_summary <- hdi_no_na %>% 
  group_by(country) %>% 
  summarise(mean_value = mean(value),
            n          = length(value),
            std_value  = sd(value),
            se_value   = std_value/sqrt(n))

#We could filter the summary to get just the ten countries with the lowest mean HDI:
hdi_summary_low <- hdi_summary %>% 
  filter(rank(mean_value) < 11)

hdi_summary_low

#then create a plot
hdi_summary_low %>% 
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_value)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_value - se_value,
                    ymax = mean_value + se_value)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()
            
install.packages("plottrix")


#all of the above code can be made into one
#this would mean there are no intermediates
#so all the way from reading the data in to the graph in one chunk

read_csv("Human-development-index.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(names_to = "year", 
               values_to = "value",
               cols = -c(hdi_rank_2018, country)) %>%
  filter(!is.na(value)) %>% 
  group_by(country) %>% 
  summarise(mean_value = mean(value),
            n = length(value),
            std_value  = sd(value),
            se_value   = std_value/sqrt(n)) %>% 
  filter(rank(mean_value) < 11) %>% 
  ggplot() +
  geom_point(aes(x = country,
                 y = mean_value)) +
  geom_errorbar(aes(x = country,
                    ymin = mean_value - se_value,
                    ymax = mean_value + se_value)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()




######################### TASK 2 #########################

file <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44025h2011.txt.gz&dir=data/historical/stdmet/"
readLines(file, n = 4)

#The first line gives the column name, the second line gives units and
#the data themselves begin on line 3
#we therefore want to skip the first 2 lines of the data file
#We can read them in with:
buoy44025 <- read_table(file, 
                        col_names = FALSE,
                        skip = 2)
#we used read table as its a table and we didnt use readtable2 as this assumes it has column names
#which it doesnt

?scan
#the scan function is used like so:
#scan("file.csv",what="character",skip=1,quiet=TRUE)
#where "file.csv" is the file you are reading in
#what is the type of data to be read
#skip is how many lines of text to skip over
#quiet tells you how many items its read if true, will miss out this info if false



#by using "file, this uses the copy of the data which hasnt had the first 2 lines removed
#on the first 2 lines there are #'s, we want to remove the hashtags

#read in the variable names from the first line, 
#removing the hash
measure <- scan(file,
                nlines = 1,
                what = character()) %>% 
  str_remove("#")
#this is because the first variable name is
#YY

#read in the units from the second line, 
#removing the hash and replacing the 
#/ with _per_ as / is a special character
units <- scan(file, 
              skip = 1,
              nlines = 1, 
              what = character()) %>% 
  str_remove("#") %>% 
  str_replace("/", "_per_")
#skip =1, skips the first row to 
#do the second row.
# replace the / with _per_ 
#this stops the WSPD (etc etc) variable
#units being m/s and to m_per_s

#paste the variable name and its units
#together for the column names
names(buoy44025) <- paste(measure, units, sep = "_")
#we've added in our units and measure columns
#and separate them by a _
#so we get YY_yr (which means years in years etc)







######################### TASK 3 #########################
#Read in the file using read_csv() from the tidyverse's readr package.
# define file name
filesol <- "data-raw/Y101_Y102_Y201_Y202_Y101-5.csv"
# skip first two lines
sol <- read_csv(filesol, skip = 2) %>% 
  janitor::clean_names()
# Filtering rows
# This dataset includes bovine serum proteins from the medium on which
# the cells were grown which need to be filtered out.
# We also filter out proteins for which fewer than 2 peptides were detected
# since we can not be confident about their identity. This is common practice
# for such proteomic data.

#filter the data to keep toes of human proteins identified by more than one peptide/delete those where less than two are deleted
sol <- sol %>% 
  filter(str_detect(description,
                    "OS=Homo sapiens")) %>% 
  filter(x1pep == "x")
#this has just filter out the bovine proteins and those proteins identified with fewer than 2 peptides


#() is characters, we are looking fri characters
#^ means not
#\s means white space
#\ means dont take the next bit literally

#extract the genename from the description and put into a column
sol <- sol %>%
  mutate(genename =  str_extract(description,"GN=[^\\s]+") %>% 
           str_replace("GN=", ""))
#extract top protein identifier and put into a column called protid
sol <- sol %>%
  mutate(protid =  str_extract(accession, ".::[^|;]+") %>% 
           str_replace(".::", ""))

filesol <- "data-raw/Y101_Y102_Y201_Y202_Y101-5.csv"
# skip first two lines
sol <- read_csv(filesol, skip = 2) %>% 
  janitor::clean_names()

sol2 <- sol %>% 
  pivot_longer(names_to = "lineage_rep", 
               values_to = "abndance",
               cols = -c(accession, peptide_count, unique_peptides, confidence_score, anova_p, q_value, max_fold_change, power, highest_mean_condition, lowest_mean_condition, mass, description, x1pep))