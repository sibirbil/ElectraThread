library(quanteda)
library(topicmodels)
library(ggplot2)
library(tidytext)
library(dplyr)
library(stringi)

#### Set the current working directory (uses RStudio API!)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#### Read the file
filename <- "electra_thread_redacted.txt"
text <- readChar(filename, file.info(filename)$size)

# Define stop words
sw <- c(stopwords("english"), "can", "v", "thus", "like", "may", "first",
        "two", "one", "use", "using", "also", "s", "based", "since", 
        "must", "b", "n", "v", "x", "y", "s", "z", "am", "pm", "eca",
        "electra", "january", "february", "march", "april", "may", "june", 
        "july", "august", "september", "october", "november", "december", "dont",
        "still", "see", "good", "even", "thanks", "much", "just", "good", "bad") 

#### Frequencies
dtm <- dfm(text, remove = sw)
freq_all <- textstat_frequency(dtm)
freq_part <- textstat_frequency(dtm, n = 40) # first 40
# Sort by reverse frequency order
freq_part$feature <- with(freq_part, reorder(feature, frequency))

# Plot
ggplot(freq_part, aes(y = feature, x = frequency)) +
  theme_bw() + geom_point(size=3, alpha=0.7, colour="black") + 
  xlab("Frequency") + ylab("") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1, face = "bold")) +
  scale_x_continuous(breaks=seq(0,freq_part[1,]$frequency,500))

# To save 
ggsave("term_frequencies_electra.png", dpi = 900)

#### Comparing against the concepts dictionary
toks <- tokens(text)
toks <- tokens_remove(toks, sw)
concepttoks <- tolower(readLines("concepts_redacted.txt", warn=FALSE))
concepttoks <- concepttoks[concepttoks!=""] # remove empty strings

getfreqt <- function(string, toks){
  sstr <- paste('\\b', string, '\\b', sep="")
  length(grep(sstr, toks))
  }

# Warning! The next step takes time...
res <- lapply(concepttoks, getfreqt, toks)
res <- as.data.frame(do.call(rbind, res))
dftest <- data.frame(concepttoks, res)
colnames(dftest) <- c("String", "Frequency")
dftest_part <- dftest[dftest$Frequency>50,]
dftest_part <- dftest_part[order(dftest_part$Frequency, decreasing = TRUE),]
rownames(dftest_part) <- NULL

# Plot 
# Only 9 concepts
ggplot(dftest_part[1:9,], aes(y = reorder(String, Frequency), x = Frequency)) +
  theme_bw() + geom_point(size=3, alpha=0.7, colour="black") + 
  xlab("Frequency") + ylab("") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1, face = "bold")) +
  scale_x_continuous(breaks=seq(0,dftest_part[1,]$Frequency,200))

# To save 
ggsave("social_science_term_frequencies_electra.png", dpi = 900)
