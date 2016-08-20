library(rvest)
require(XML)
library(ggplot2)


# only do this the FIRST RUN
# create a placeholder using Mad Max entry from OMDB to start data frame and test dimensions
#test.xml <- xmlParse("http://www.omdbapi.com/?t=mad-max&r=xml&tomatoes=true")
#movie.df <- as.data.frame(t(xmlSApply(test.xml["/root/movie"],xmlAttrs)),
#                      stringsAsFactors=FALSE)

### STEP 1: BUILD THE DATA SET

# Paste the URL for the IMDb list you want and add "?$start=" here
imdb.url1 <- "http://www.imdb.com/list/ls051689886/?start="
imdb.url2 <- "&view=compact"
i <- 1
n <- 1
links.all <- character()

# the meat
PAGECOUNT <- getPageCount(imdb.url1,imdb.url2)
IMDB.IDS <- getImdb(PAGECOUNT, imdb.url1, imdb.url2)
OMDB.DF <- getOmdb(IMDB.IDS)

# store data from each list here using <- OMDB.DF
OMDB.DF.america 
OMDB.DF.adventure 
OMDB.DF.animation
OMDB.DF.biography 
OMDB.DF.crime 
OMDB.DF.documentary
OMDB.DF.drama
OMDB.DF.fantasy
OMDB.DF.history
OMDB.DF.mystery 
OMDB.DF.sport 
OMDB.DF.thriller
OMDB.DF.war <- OMDB.DF


# scrape # of pages
getPageCount <- function(url1, url2) {
      imdb.url3 <- paste(url1,as.character(n),url2, sep="")
      pages <- read_html(imdb.url3) %>%
            html_nodes(".desc") %>%
            html_text()
      temp <- as.numeric(unlist(strsplit(pages, "[^[:digit:]]")))
      solution <- unique(temp[!is.na(temp)])
      pagecount <- solution[2]
      return(pagecount)
}

#get IMDb IDs
getImdb <- function(pagecount, url1, url2) {
      while (i <= pagecount) {
            imdb.url3 <- paste(url1,as.character(n),url2, sep="")
            imdb.html.url <- read_html(imdb.url3)
            n=n+250
            i <- i+1
            links <- imdb.html.url %>% 
                  html_nodes(".title a") %>%
                  html_attr("href")
            links.all <- c(links.all, links)
      }
      imdb.ids <- substr(links.all, 8,16)
      return(imdb.ids)
}

# Parse XML from OMDB using IMDb IDs
getOmdb <- function(imdb.ids) {
      omdb.html.1 <- "http://www.omdbapi.com/?i="
      omdb.html.2 <- "&r=xml&tomatoes=true"
      #rm(movie.df)
      for (x in imdb.ids) {
            print(x)
            omdb.html.url <- paste(omdb.html.1, x,omdb.html.2, sep="")
            omdb.xml <- xmlParse(omdb.html.url)
            omdb.data <- as.data.frame(t(xmlSApply(omdb.xml["/root/movie"],xmlAttrs)),
                                    stringsAsFactors=FALSE)
            if (dim(omdb.data)[2] == 34) { #prevents cbind errors due to missing OMDB entries
                  movie.df <- rbind(movie.df,omdb.data) 
            }
#            print(tail(movie.df[,1],1))
      }
      return(movie.df)
}

## CLEAN DATA FRAME

#remove duplicates
movie.df.final <- movie.df.final[!duplicated(movie.df.final[c("title","year")]),]
movie.df.final <- movie.df.final[!duplicated(movie.df.final[c("title","director")]),]

# remove plot and tomato conensus columns
movie.df.final <- movie.df.final[,-c(10,26)]

#remove rows without tomatoMeter, imdbRating, and metascore
movie.df.final$tomatoMeter <- as.numeric(movie.df.final$tomatoMeter)
movie.df.final <- movie.df.final[!is.na(movie.df.final$tomatoMeter),]
movie.df.final$imdbRating <- as.numeric(movie.df.final$imdbRating)
movie.df.final <- movie.df.final[!is.na(movie.df.final$imdbRating),]
movie.df.final$metascore <- as.numeric(movie.df.final$metascore)
movie.df.final <- movie.df.final[!is.na(movie.df.final$metascore),]

rtbyyear <- aggregate(tomatoMeter ~ year, data=movie.df.final, mean)
imdbbyyear <- aggregate(imdbRating ~ year, data=movie.df.final, mean)
metascorebyyear <- aggregate(metascore ~ year, data=movie.df.final, mean)

#get rid of weird 1979 artifact
rtbyyear <- rtbyyear[-c(35),]
imdbbyyear <- imdbbyyear[-c(35),]
metascorebyyear <- metascorebyyear[-c(35),]


ratings <- cbind(imdbbyyear, rtbyyear$tomatoMeter)
ratings <- cbind(ratings, metascorebyyear$metascore)
ratings$imdbRating <- ratings$imdbRating*10
names(ratings)[3] <- "tomatoMeter"
names(ratings)[4] <- "metascore"
ratings$year <- as.numeric(ratings$year)

# line plot
png("average movie ratings.png", width=800, height=600, units="px") 

ggplot(ratings, aes(x=year, group=1,title="Average Movie ratings, 1962-2016",ylab="Rating")) +
      geom_line(aes(y=tomatoMeter, color="red")) +
      geom_line(aes(y=metascore, color="blue")) +
      geom_line(aes(y=imdbRating, color="yellow")) +
      scale_colour_identity(guide="legend", name='Website', labels=c("Metacritic","RT","IMDb")) +
      ylab("Rating") +
      xlab("Year") +
      geom_vline(xintercept=1996, colour="yellow", linetype="longdash") +
      geom_vline(xintercept=1998, colour="red", linetype="longdash") +
      geom_vline(xintercept=1999, colour="blue", linetype="longdash")

dev.off()
# chart only top movies

rt.top <- movie.df.final[ order(movie.df.final$year, -movie.df.final$tomatoMeter), ]
imdb.top <- movie.df.final[ order(movie.df.final$year, -movie.df.final$imdbRating),]
metascore.top <- movie.df.final[ order(movie.df.final$year, -movie.df.final$metascore),]

rt.splits<-split(rt.top,rt.top$year)
rt.df <-lapply(rt.splits,head,12)
rt.top <- do.call(rbind.data.frame,rt.df)

imdb.splits <- split(imdb.top,imdb.top$year)
imdb.df <- lapply(imdb.splits,head,12)
imdb.top <- do.call(rbind.data.frame,imdb.df)

mc.splits <- split(metascore.top, metascore.top$year)
mc.df <- lapply(mc.splits,head,12)
mc.top <- do.call(rbind.data.frame,mc.df)


rtbyyear.top <- aggregate(tomatoMeter ~ year, data=rt.top, mean)
imdbbyyear.top <- aggregate(imdbRating ~ year, data=imdb.top, mean)
mcbyyear.top <- aggregate(metascore ~ year, data=mc.top, mean)

rtbyyear.top <- rtbyyear.top[-c(35),]
imdbbyyear.top <- imdbbyyear.top[-c(35),]
mcbyyear.top <- mcbyyear.top[-c(35),]

ratings.top <- cbind(imdbbyyear.top, rtbyyear.top$tomatoMeter)
ratings.top <- cbind(ratings.top, mcbyyear.top$metascore)
ratings.top$imdbRating <- ratings.top$imdbRating*10
names(ratings.top)[3] <- "tomatoMeter"
names(ratings.top)[4] <- "metascore"
ratings.top$year <- as.numeric(ratings$year)

png("top 12 movie ratings.png", width=800, height=600, units="px")

ggplot(ratings.top, aes(x=year, group=1,title="Top 12 Movie ratings, 1980-2016",ylab="Rating")) +
      geom_line(aes(y=tomatoMeter, color="red")) +
      geom_line(aes(y=metascore, color="blue")) +
      geom_line(aes(y=imdbRating, color="yellow")) +
      scale_colour_identity(guide="legend", name='Website', labels=c("Metacritic","RT","IMDb")) +
      ylab("Rating") +
      xlab("Year") +
      xlim(1980,2016) +
      geom_vline(xintercept=1996, colour="yellow", linetype="longdash") +
      geom_vline(xintercept=1998, colour="red", linetype="longdash") +
      geom_vline(xintercept=1999, colour="blue", linetype="longdash")

dev.off()
# barplot of # of movies by year
png("number of movies by year.png", width=800, height=600, units="px")

barplot(table(movie.df.final$year), main="Number of movies by year (w/ ratings from all 3 sites)")

dev.off()
