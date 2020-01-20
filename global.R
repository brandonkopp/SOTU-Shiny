
#Load the Data
#allsotu <- readRDS("./sotushiny/data/SOTUdata.rds")
stop_words <- c("the","an","will","can","also", "that","thats")

#Frequency Charts from Words
strcounts <- function(data,list, only=TRUE) {
  counts <- data.frame(matrix(nrow=1,ncol=7))
  names(counts) = c("term","year","period","name","party","delivery","count")
  
  text <- regexer(list, only)
  for(j in 1:nrow(data)){
    for(i in 1:length(text)){
      x <- str_count(data$speechtext[j], text[i])
      df <- data.frame(simpleCap(list[i]), data$year[j], data$timespan[j],data$president[j],
                       data$Party[j],data$delivery[j], x)
      names(df) = c("term","year","period","name","party","delivery","count")
      counts <- rbind(counts,df)
    }
  }
  counts <- counts[!is.na(counts$term), ]
  return(counts)
}

context <- function(sotu,list, only=TRUE) {
  context <- data.frame(matrix(nrow=1,ncol=3))
  names(context) = c("term","year","sentence")
  
  text <- regexer(list, only)
  
  for(j in 1:nrow(sotu)){
    for(i in 1:length(text)){
      search <- paste0("(?i)((?=[^.\\n]*(",text[i],"))[^.\\n]+\\.?)")
      x <- str_extract_all(sotu$speechtext[j], search)
      if(length(x[[1]]) ==0) x <- NA
      df <- data.frame(simpleCap(list[i]), sotu$year[j], x)
      names(df) = c("term","year","sentence")
      context <- rbind(context,df)
    }
  }
  context <- context[!is.na(context$sentence), ]
  return(context)
}

regexer <- function(text, only=TRUE){
  
  x <- strsplit(text, "\\|")
  terms <- length(text)
  
  for(i in 1:terms){
    words <- length(x[[i]])
    if(str_count(text[i],"\\|") > 0){
      for(j in 1:words){
        l <- substr(x[[i]][[j]], 1, 1)
        m <- paste0("[",toupper(l),tolower(l), "]")
        x[[i]][[j]] <- paste0(m,substr(x[[i]][[j]], 2, nchar(x[[i]][[j]])))
      }
    }else {
      l <- substr(x[[i]], 1, 1)
      m <- paste0("[",toupper(l),tolower(l), "]")
      x[[i]] <- paste0(m,substr(x[[i]], 2, nchar(x[[i]])))      
    }
    x[[i]] <- paste(x[[i]], collapse="|")
  }
  
  x <- unlist(x)
  
  x <- strsplit(x, " ")
  terms <- length(x)
  
  for(i in 1:terms){
    words <- length(x[[i]])
    if(str_count(x[i], "\\s") > 0){
      for(j in 2:words){
        l <- substr(x[[i]][[j]], 1, 1)
        m <- paste0("[",toupper(l),tolower(l), "]")
        x[[i]][[j]] <- paste0(m,substr(x[[i]][[j]], 2, nchar(x[[i]][[j]])))
      }
    }
    x[[i]] <- paste(x[[i]], collapse=" ")
    if(only ==TRUE){ x[[i]] <- paste0("\\b",x[[i]],"\\b")}
  }
  x <- unlist(x)
  
  return(x)
}

simpleCap <- function(x) {
  s <- strsplit(x, "\\|")[[1]]
  s <- paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="|")
  s <- strsplit(s, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

stringsplitter <- function(wordstring){
  x <- strsplit(wordstring, ",")[[1]] %>%
    str_trim(side="both") %>%
    unlist(.)
  return(x)
}

wordsOverTime <- function(df,words,stop=TRUE,colorvar,leg,scale=20,sz=14, forprint=FALSE) {
  
  x <- strcounts(df, words, only=stop)
  
  max_count <- max(x$count)
  
  p <- ggplot(x, aes(as.numeric(year), term, size=count)) +
    geom_point(stat="identity", aes_string(color = colorvar), alpha=.5) +
    scale_x_continuous(breaks = seq(1792,2016,by=4)) +
    scale_size(range=c(0,scale), guide="none") +
    facet_wrap(~period, scales="free_x", ncol = 1) +
    labs(title = "",
         y = "",
         x = "Year",
         subtitle= paste0("Circles scaled by number of uses of the word (Maximum Uses: ",max_count,")"),
         color = leg) +
    theme(plot.title = element_text(size=22, face="bold"),
          plot.subtitle = element_text(size=15),
          plot.caption = element_text(size=10),
          plot.background = element_rect(fill = 'white'),
          panel.border = element_rect(fill=NA, size=1, color="grey30"),
          panel.background = element_blank(),
          panel.grid.major = element_line(size=.3, color="grey50",linetype = 3),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, vjust=1.2, hjust=1),
          axis.text = element_text(size = sz-2,face="bold", color="black"),
          axis.title = element_text(size = sz, face="bold", color = "black"),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_text(size=sz, face="bold"),
          strip.background = element_blank(),
          legend.title = element_text(size=sz, face="bold"),
          legend.text = element_text(size=sz-2),
          legend.key = element_rect(fill = NA),
          legend.position = "bottom") +
    guides(colour = guide_legend(override.aes = list(size=11)))
  
  if(leg == "Party"){
      p <- p + scale_color_manual(values=c("blue","lightsteelblue3","brown","pink",
                                           "green","red","orange2"))
  }else if(leg == "Delivery Method") {
      p <- p + scale_color_manual(values=c("blue","red"))
  }
  
  if(forprint==TRUE){
    p <- p + labs(title = "State of the Union Addresses, Words Through Time",
              caption="Downloaded from: https://brandonkopp.shinyapps.io/sotushiny")
  }
  
  p
}

wordsByPres <- function(df,words,stop=TRUE,pres,sz=16) {
  
  x <- strcounts(df, words, only=stop)
  
  p <- ggplot(x, aes(year, count, group=term, color = term)) +
    geom_line(stat="identity", size=1.5) +
    geom_point(size=3) +
    facet_wrap(~term) +
    labs(title = paste0("Word Usage by ", pres),
         y = "Number of Uses",
         x = "Year",
         color = "Term") +
    scale_y_continuous(breaks= pretty_breaks()) +
    theme(plot.title = element_text(size = sz+4, face="bold", color = "black"),
          plot.background = element_rect(fill = 'white'),
          panel.border = element_rect(fill=NA, size=1, color="grey30"),
          panel.background = element_blank(),
          panel.grid.major = element_line(size=.3, color="grey50",linetype = 3),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = sz-2,face="bold", color="black"),
          axis.text.x = element_text(angle = 45, vjust=1.2, hjust=1),
          axis.title = element_text(size = 16, face="bold", color = "black"),
          axis.ticks = element_blank(),
          axis.line = element_line(size = 1,color="black"),
          strip.text = element_text(size=sz, face="bold"),
          strip.background = element_blank(),
          legend.title = element_text(size=sz, face="bold"),
          legend.text = element_text(size=sz-2),
          legend.position = "none")
  
  p
}

make_tdm <- function(text){
  
  docs <- VCorpus(VectorSource(text)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(tolower)  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, stop_words) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument)
  
  tdm <- TermDocumentMatrix(docs) %>%
    as.matrix()
  
  tdm <- melt(tdm)
  names(tdm) <- c("term","variable","freq")
  tdm <- arrange(tdm, desc(freq))
  
  return(tdm)
}

topncount <- function(tdm, top=15, col="darkolivegreen4"){
  
  tdm <- tdm[1:top,]
  
  x <- tdm 
  
  order <- x %>% group_by(term) %>%
    summarize(sumfreq=sum(freq)) %>%
    arrange(sumfreq)
  
  x$term <- factor(x$term,order$term, ordered=TRUE)
  
  p <- ggplot(x, aes(x=term, freq, fill=as.factor(variable), group=as.factor(variable))) +
    geom_bar(stat="identity", color="white",size=.3) +
    scale_fill_manual(values = c(col)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,max(order$sumfreq)+20)) +
    labs(
      title = "Word Count",
      x = "Word",
      y = "Count",
      fill = ""
    ) +
    theme(plot.title = element_blank(),
          plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.border = element_rect(fill = NA, color = 'white', size = 2),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey79", size=.3, linetype = 3),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 10, color="black", face="bold"),
          axis.title.x = element_text(size = 12, face="bold", color = "black"),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_line(colour = "black", size=1),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(size = 10, color= "black"),
          legend.title = element_text(size = 12,face="bold"),
          legend.position = "none") +
    coord_flip()
  
  p
}

cloud <- function(tdm, words=500, color="RdBu"){
  par(mar = rep(0, 4))
  wordcloud(tdm$term, tdm$freq,max.words = words, min.freq =2, scale=c(4,0.5), random.order = FALSE,
            random.color = FALSE, colors= brewer.pal(8, color))
}

compcloud <- function(x,max=200, col=c("lightskyblue","orange")){
  
  docs <- VCorpus(VectorSource(x$speechtext)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(tolower)  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, stop_words) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument)
  
  tdm <- TermDocumentMatrix(docs) %>%
    as.matrix() 
  colnames(tdm) <- c(x$president[1],x$president[2]) 
  
  par(mar = rep(0, 4))
  comparison.cloud(tdm, random.order=FALSE, scale = c(5,0.4),
                   colors = col, title.bg.colors=c("lightskyblue","orange"),
                   title.size=2.5, max.words=max)
}

commcloud <- function(x,max=200){
  
  docs <- VCorpus(VectorSource(x$speechtext)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(tolower)  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, stop_words) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument)
  
  tdm <- TermDocumentMatrix(docs) %>%
    as.matrix() 
  colnames(tdm) <- c(x$president[1],x$president[2]) 
  
  par(mar = rep(0, 4))
  commonality.cloud(tdm, scale = c(5,0.4), max.words=max,
                    random.order=FALSE, colors= brewer.pal(8, "PRGn"))
}

spsentgraph <- function(speechtext){
  speech.df <- data.table(speech=speechtext)
  
  sentences <- data.table(sentSplit(speech.df, "speech"))
  # Add a sentence counter and remove unnecessary variables
  sentences[, sentence.num := seq(nrow(sentences))]
  sentences[, tot := NULL]
  setcolorder(sentences, c("sentence.num", "speech"))
  
  # Syllables per sentence
  sentences[, syllables := syllable_sum(gsub("[\\\\\\/|.]","x", speech))]
  sentences = sentences[!is.na(syllables)]
  # Add cumulative syllable count and percent complete as proxy for progression
  sentences[, syllables.cumsum := cumsum(syllables)]
  sentences[, pct.complete := syllables.cumsum / sum(sentences$syllables)]
  sentences[, pct.complete.100 := pct.complete * 100]
  
  pol.df <- polarity(sentences$speech)$all
  sentences[, words := pol.df$wc]
  sentences[, pol := pol.df$polarity]
  sentences[pol > 0, dir := 1]
  sentences[pol == 0, dir := 0]
  sentences[pol < 0, dir := -1]
  
  my.theme <- 
    theme(plot.title = element_blank(),
          plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.border = element_rect(fill = NA, color = 'white', size = 2),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey79", size=.3, linetype = 3),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 10, color="black", face="bold"),
          axis.title.x = element_text(size = 12, face="bold", color = "black"),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_line(colour = "black", size=1),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(size = 10, color= "black"),
          legend.title = element_text(size = 12,face="bold"),
          legend.position = "none")
  
  CustomScatterPlot <- function(gg)
    return(gg + geom_point(aes(color=dir)) + # Lighten dots
             stat_smooth(method = 'loess', color="indianred3", fill="lightgray", size=1.4) + 
             xlab("Percent complete (by syllable count)") + 
             ylab("Sentiment (sentence-level polarity)") +
             ggtitle("Sentiment Across Speech") +
             scale_x_continuous(labels = percent) + my.theme)
  
  CustomScatterPlot(ggplot(sentences, aes(pct.complete, pol)))
}

spsentcloud <- function(sentimentwords,maxwords = 250, col = c("darkolivegreen4","indianred3")){
  docs <- VCorpus(VectorSource(sentimentwords)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(tolower)  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument) %>%
    tm_map(removeWords, c("the","an","vice"))
  
  tdm <- TermDocumentMatrix(docs) %>%
    as.matrix()
  colnames(tdm) <- c("Positive","Negative")
  
  par(mar = rep(0, 4))
  comparison.cloud(tdm, random.order=FALSE, scale = c(5,0.4),
                   colors = col, title.bg.colors=c("darkolivegreen4","indianred3"),
                   title.size=2.5, max.words=maxwords)
  
}

change_text <- function(text){
  return(gsub(" ","_",tolower(text)))
}
### WIKIPEDIA FUNCTIONS
get_wiki <- function(searchterm){
  search <- gsub(" ", "%20", searchterm)
  url <- paste0('https://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=',
                search, '&srlimit=10&utf8=&format=json')
  
  jsonresults <- jsonlite::read_json(url)
  x <- list(title = jsonresults$query$search[1][[1]]$title,
            snippet = jsonresults$query$search[1][[1]]$snippet,
            url = paste0("https://en.wikipedia.org/wiki/", gsub(" ","_", jsonresults$query$search[1][[1]]$title)))
  return(x)
}

format_wiki <- function(wiki_list){
  pg <- paste0("<br><br><hr>","<h3>Top Wikipedia Result</h3>",
              "<h4>",wiki_list$title,"</h4>",
               "<p>...",wiki_list$snippet,"...</p>",
               "<a href='",wiki_list$url,"' target='_blank'>",wiki_list$url,"</a>")
  pg
}