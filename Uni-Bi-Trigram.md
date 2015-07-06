
title: "text mine"

author: "Neeraj"


```{r}
#reading text file.
text <- readLines("movie_lines.txt",encoding="UTF-8")
#when the data is very large it is usually recommended to read first few lines of the data, to give impression of whether it is right or not.

#performing regular expression task to extract only conversation.
reg<-gsub("L[0-9]* .*u[0-9]* .*m[0-9]* ....... .*\\+*\\$\\+* " , "\\1", text)

# Convert to lower case
low <- tolower(reg)

#tokenizing and unlisting because the strsplit produces the llist of data which is not favorable.
space <- unlist(strsplit(low, "[[:space:]]+"))

#removing punctuations.
punc= gsub("[[:punct:]]", "", space)

# Remove empty tokens
emp <- punc[punc != ""]

# Remove the first element and add a period at the end
tokens2 <- c(emp[-1], ".")

#unigram count for 1st 15 words in decresing order of their frequencies.
uni <- sort(table(emp), decreasing=T)
head(uni, n=100)

#giving probability of unigram as well.
for (b in names(uni)[1:100]) {
  cat(b, uni[b], uni[b]/3176748, "\n", sep="\t")
};


```



*Bigram


```{r}
# Create a sorted table of bigram type frequencies
bigram <- sort(table(paste(emp, tokens2)), decreasing=T)
head(bigram,n=100)

#giving probability of bigram as well.
for (b in names(bigram)[1:100]) {
  cat(b, bigram[b], bigram[b]/3176748, "\n", sep="\t")
};

#Again, remove the first element and add a period at the end
tokens3 <- c(tokens2[-1], ".")

# Create a vector of trigrams using paste
trigrams <- paste(emp, tokens2, tokens3)
```



*Trigram


```{r}
# Create a sorted table of trigram type frequencies
trigram <- sort(table(trigrams), decreasing=T)
head(trigram, n=100)

#giving probability associated with trigram as well.
for (t in names(trigram)[1:100]) {
  cat(t, trigram[t], trigram[t]/3176748, "\n", sep="\t")
}
```
