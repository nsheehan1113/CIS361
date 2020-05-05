#Enable Packages

library(tuber)
library(purrr)
library(stringr)
library(dplyr)

#Authorization for Youtube APIs
client_id <- "871403720599-cjqncpljsnghb9las2sr0s87rdtm62ko.apps.googleusercontent.com"
client_secret <-  "UV1qXjK9TmVB9k6MzQLoJsjT"

yt_oauth(app_id = client_id,
         app_secret = client_secret,
         token = '')

#Get playlist Id for Music playlist
NickMusicPlaylistId <- str_split(
  string = "https://www.youtube.com/playlist?list=PL1SoOwjbBbc9a0YugW2mLfaDkjCQrlNlh", 
  pattern = "=", 
  n = 2,
  simplify = TRUE)[ , 2]
NickMusicPlaylist

#Put videos in playlist into data frame
NickMusicPlaylist <- get_playlist_items(filter = 
                                              c(playlist_id = NickMusicPlaylistId), 
                                              part = "contentDetails",
                                              max_results = 10)
#Save list of video ids
music_ids <- base::as.vector(NickMusicPlaylist$contentDetails.videoId)

#Function for get all stats on vids
get_all_stats <- function(id) {
  get_stats(video_id = id)
} 

#Get all stats for vid ids in that playlist
NickMusicStats <- map_df(.x = music_ids, .f = get_all_stats)

#Function for get all details on vids
get_all_details <- function(id) {
  get_video_details(video_id = id, part = "snippet")
}

#Get all details for vid ids in that playlist
NickMusicDetails <- map_df(.x = "ZengOKCUBHo", .f = get_video_details)
