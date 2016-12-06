#Script for harvesting data on faculty and students at University of Copenhagen
#To provide basis for test for representativity in our user satisfaction surveys.
#just the latest - but can easily be modified for 2015 and 2014.
#Before that: No gendered data.
#
#Necessary libraries
library(RCurl)
library(XML)
library(rvest)
library(pdftools) #Yeah, pdftables would probably be easier to work with.
require(stringr)


#Where are the data:
faculty_numbers_url <- 'http://tal.ku.dk/c_personale/'
stud_numbers_url <- 'http://us.ku.dk/studiestatistik/studiestatistikker/bestand/KU_BESTAND_2016.pdf'

#First current data for faculty (and other staff)

#Read the page with numbers on staff
pers_numbers_raw <- read_html(faculty_numbers_url)

#Get the relevant strings from the file
pers_num <- html_nodes(pers_numbers_raw,"div") %>%
  html_nodes("table") %>%
  html_nodes("strong") %>%
  html_text

#and now the precise strings:
VIP <- pers_num[2]
DVIP <- pers_num[4]
TAP <- pers_num[6]

#get rid of a punctuation mark
VIP <- gsub('\\.','',VIP)
DVIP <- gsub('\\.','',DVIP)
TAP <- gsub('\\.','',TAP)

#Convert from string to numeric
VIP <- as.numeric(VIP)
DVIP <- as.numeric(DVIP)
TAP <- as.numeric(TAP)

#Collect all three numbers in a dataframe
staff_data <- data.frame(VIP=VIP, DVIP=DVIP, TAP=TAP)

#save it - and attach a time-stamp
#I have no idea about the update-frequency. But it could be interesting to get a time-series on this
saveRDS(staff_data, file=paste("staff_data ",gsub(":", ".", date()),".RDA", sep=""))

#And now for the students. The university saves the data in a pdf. Bloody annoying.
#But that won't keep us away

#let's start by downloading the file:
download.file(stud_numbers_url, "destfile.pdf", mode="wb")

#Convert it to text:
txt <- pdf_text("destfile.pdf")

class(txt)
#Every line in the table ends with \r\n.
#And I get a vector with five very long strings.
#Lets concatenate them in to one large character thingamajig

lines <- NULL

for(i in 1:length(txt)){
  lines[i] <- strsplit(txt[i], "\r\n")
}

lines <- unlist(lines, recursive=FALSE)

lines <- as.list(linier)

#Nice!

#lets get rid of the first line

lines[1] <- NULL

#Each line begins with a string, then a lot of whitespace, and then three numbers, separated by whitespace
#but saved as characters.

#this function splits the string. It's not beautiful. But it works.

getNumbers <- function(line){
  regexp1 <- "([[:digit:]])" #A regular expression that finds digits. The text is terminated by a digit
  theText <- substr(line,1 , regexpr(pattern = regexp1, line)[1]-1)  #Locating the position of first digit.
  theRest <- substr(line, regexpr(pattern = regexp1, line)[1], nchar(line)) #taking the rest
  theRest <- gsub('\\.','',theRest)  #removing punctuation in the rest
  regexp2 <- "([[:blank:]])+"  #Splitting by blank characters
  some <- strsplit(theRest, regexp2) #splitting the rest by blank characters
  result <- list(theText, some[[1]][1], some[[1]][2], some[[1]][3]) #returning the result.
  return(result)
}

#I will need to be able to recognize if the string contains some words:
#Fakultet (faculty), Bachelor and Kandidat (masters degree)


test_fak <- function(streng){
  regexp = "(Fakultet)"
  result = grepl(pattern=regexp, x=streng)
  return(result)
}

test_bac <- function(streng){
  regexp = "(Bachelor)"
  result = grepl(pattern=regexp, x=streng)
  return(result)
}

test_cand <- function(streng){
  regexp = "(Kandidat)"
  result = grepl(pattern=regexp, x=streng)  
  return(result)
}

#Making a dataframe to keep the results of all this:
theNumbers <- data.frame("Fakultet" = character(), Level = character(), Subj = character(), Female = numeric(), Male = numeric(), Total = numeric(), stringsAsFactors=FALSE)
colnames(theNumbers) <- c("Fakultet", "Level", "Subject", "Female", "Male", "Total")


#the structure is so, that first I get a faculty name, with the relevant total numbers.
#Then I get the level (bachelor/master), with the relevant total numbers
#after that, I get all the individual programmes from the aforementioned faculty, and level.
#I'll get to a new level, but with the same faculty name.
#And then I'll get to a new faculty, with total numbers.
#I'll need to initialize two variables:

level <- "Bachelor"
depart <- "Hele KU"

#And then, these lines do the magic!
for(i in 1:length(lines)){
  if(test_fak(getNumbers(lines[i])[1])){
    depart <- str_trim(getNumbers(lines[i])[1])
  }
  if(test_bac(getNumbers(lines[i])[1])){
    level <- "Bachelor"
  }
  else if(test_cand(getNumbers(lines[i])[1])){
    level <- "Kandidat"
  }
  subj <- str_trim(getNumbers(lines[i])[1])
  hun <- as.numeric(getNumbers(lines[i])[2])
  han <- as.numeric(getNumbers(lines[i])[3])
  tot <- as.numeric(getNumbers(lines[i])[4])
  
  if(identical(subj, depart)) {
    subj <- "Alle"
    level <- "Alle"
  }
  if(identical(level,subj)){
    subj <- "Alle"
  }
  
  theNumbers[nrow(theNumbers)+1,] <- list(depart,level,subj,hun,han,tot)
}

#Hm...

#Nice. That got me pretty close. I need to handle the first three lines.
#It could probably be done by code. But by hand is faster.
theNumbers[1,]$Level <- "Alle"
theNumbers[1,]$Subject <- "Alle"
theNumbers[2,]$Level <- "Bachelor"
theNumbers[2,]$Subject <- "Alle"
theNumbers[3,]$Level <- "Kandidat"
theNumbers[3,]$Subject <- "Alle"

#Almost done! I just need to get rid of the last three lines
theNumbers <- theNumbers[-c(257, 258, 259), ]


View(theNumbers)
#And now I'm actually done!
#Save the data:
saveRDS(theNumbers, file="KU_stud_numbers_2016.RDA")
