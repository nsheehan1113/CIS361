#Enable Packages

library(tuber)
library(purrr)
library(stringr)
library(dplyr)
library(shiny)
library(rmarkdown)
library(ggplot2)
library(knitr)

#Authorization for Youtube APIs
client_id <- "871403720599-cjqncpljsnghb9las2sr0s87rdtm62ko.apps.googleusercontent.com"
client_secret <-  "UV1qXjK9TmVB9k6MzQLoJsjT"

yt_oauth(app_id = client_id,
         app_secret = client_secret,
         token = '')

#Get playlist Id for Music playlist
NickFBPlaylistId <- str_split(
  string = "https://www.youtube.com/playlist?list=PL1SoOwjbBbc-rh4v7ciKTtIcOyOpT11ov", 
  pattern = "=", 
  n = 2,
  simplify = TRUE)[ , 2]


#Put videos in playlist into data frame
NickFBPlaylist <- get_playlist_items(filter = 
                                              c(playlist_id = NickFBPlaylistId), 
                                              part = "contentDetails",
                                              max_results = 10)
#Save list of video ids
FB_ids <- base::as.vector(NickFBPlaylist$contentDetails.videoId)

#Function for get all stats on vids
get_all_stats <- function(id) {
  get_stats(video_id = id)
} 

#Get all stats for vid ids in that playlist
NickFBStats <- map_df(.x = FB_ids, .f = get_all_stats)

#get all details on vids
NickFBDetails <- lapply(FB_ids, get_video_details)
NickMusicDetails <- lapply(music_ids, get_video_details)
NickGamingDetails <- lapply(gaming_ids, get_video_details)
NickMoviesDetails <- lapply(movies_ids, get_video_details)

#Pull Id and Title from details

FBDetailsDF <- data.frame(id=NA, Title=NA)

for (p in 1:length(NickFBDetails)) {
  id <- NickFBDetails[[p]]$items[[1]]$id
  Title <- NickFBDetails[[p]]$items[[1]]$snippet$title
  
  FBdetail <- data_frame(id = id, Title = Title)
  FBDetailsDF <- rbind(FBdetail, FBDetailsDF)
}

#Merge details to stats data frame

NickMusicAllData <- merge(NickMusicStats,MusicDetailsDF,by = "id")
NickGamingAllData <- merge(NickGamingStats,GamingDetailsDF,by = "id")
NickMoviesAllData <- merge(NickMovieStats,MoviesDetailsDF,by = "id")
NickFBAllData <- merge(NickFBStats,FBDetailsDF,by = "id")

#Graph showing differences in views by main genres

options(scipen=999)

MusicGenreViewCount <- sum(as.numeric(NickMusicAllData$viewCount), na.rm = TRUE)
GamingGenreViewCount <- sum(as.numeric(NickGamingAllData$viewCount), na.rm = TRUE)
MoviesGenreViewCount <- sum(as.numeric(NickMoviesAllData$viewCount), na.rm = TRUE)
FBGenreViewCount <- sum(as.numeric(NickFBAllData$viewCount), na.rm = TRUE)

Genres <- c("Movies/Entertainment","Music","Gaming","Fashion/Beauty")
ViewCounts <- c(MoviesGenreViewCount,MusicGenreViewCount,GamingGenreViewCount,FBGenreViewCount)

BarGraphDF <- data.frame(Genres,ViewCounts)

ggplot(data=BarGraphDF, aes(x=Genres,y=ViewCounts))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=ViewCounts), vjust=1.6, color="white",size=3.5)+
  theme_minimal()

#Generate Bar Plot for Likes and Disklikes for each genre

MusicLikeCount <- sum(as.numeric(NickMusicAllData$likeCount), na.rm=TRUE)
GamingLikeCount <- sum(as.numeric(NickGamingAllData$likeCount), na.rm=TRUE)
MoviesLikeCount <- sum(as.numeric(NickMoviesAllData$likeCount), na.rm=TRUE)
FBLikeCount <- sum(as.numeric(NickFBAllData$likeCount), na.rm=TRUE)

LikeCounts <- c(MoviesLikeCount,MusicLikeCount,GamingLikeCount,FBLikeCount)

LikesBarGraphDF <- data.frame(Genres,LikeCounts)

ggplot(data=LikesBarGraphDF, aes(x=Genres,y=LikeCounts))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=LikeCounts), vjust=1.6, color="white",size=3.5)+
  theme_minimal()

MusicDislikeCount <- sum(as.numeric(NickMusicAllData$dislikeCount), na.rm=TRUE)
GamingDislikeCount <- sum(as.numeric(NickGamingAllData$dislikeCount), na.rm=TRUE)
MoviesDislikeCount <- sum(as.numeric(NickMoviesAllData$dislikeCount), na.rm=TRUE)
FBDislikeCount <- sum(as.numeric(NickFBAllData$dislikeCount), na.rm=TRUE)

DislikeCounts <- c(MoviesDislikeCount,MusicDislikeCount,GamingDislikeCount,FBDislikeCount)

DislikesBarGraphDF <- data.frame(Genres,DislikeCounts)

ggplot(data=DislikesBarGraphDF, aes(x=Genres,y=DislikeCounts))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=DislikeCounts), vjust=1.6, color="white",size=3.5)+
  theme_minimal()

# Bar graph for comments per genre

MusicCommentCount <- sum(as.numeric(NickMusicAllData$commentCount), na.rm=TRUE)
GamingCommentCount <- sum(as.numeric(NickGamingAllData$commentCount), na.rm=TRUE)
MoviesCommentCount <- sum(as.numeric(NickMoviesAllData$commentCount), na.rm=TRUE)
FBCommentCount <- sum(as.numeric(NickFBAllData$commentCount), na.rm=TRUE)

CommentCounts <- c(MoviesCommentCount,MusicCommentCount,GamingCommentCount,FBCommentCount)

CommentsBarGraphDF <- data.frame(Genres,CommentCounts)

ggplot(data=CommentsBarGraphDF, aes(x=Genres,y=CommentCounts))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=CommentCounts), vjust=1.6, color="white",size=3.5)+
  theme_minimal()