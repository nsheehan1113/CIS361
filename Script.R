#Enable Packages

library(tuber)
library(purrr)
library(stringr)
library(dplyr)
library(shiny)
library(rmarkdown)
library(ggplot2)

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