

import spotipy
import json
import webbrowser
import MyCredentials

oauth_object = spotipy.SpotifyOAuth(MyCredentials.clientID, MyCredentials.clientSecret, MyCredentials.redirect_uri)
token_dict = oauth_object.get_access_token()
token = token_dict['access_token']
spotifyObject = spotipy.Spotify(auth=token)
user_name = spotifyObject.current_user()

wod_url = 'spotify:artist:6g0mn3tzAds6aVeUYRsryU'

results = spotifyObject.artist_albums(wod_url, album_type='album')

albums = results['items']
while results['next']:
    results = spotify.next(results)
    albums.extend(results['items'])

for album in albums:
    print(f'name: {album["name"]}, ID: {album["id"]}')

aduid = ""

for album in albums:
    if album['name'] == 'A Deeper Understanding':
        aduid = album['id']

track_results = spotifyObject.album_tracks(aduid)

tracks = track_results['items']

adu_trackids = []

for track in tracks:
    track.pop('available_markets')
    adu_trackids.append(track['id'])

print(adu_trackids)
audiodata = dict()

#for track in adu_trackids[0:1]:
#    audiodata[track] = spotifyObject.audio_analysis(track)
#
#features = ['meta', 'track', 'bars', 'beats', 'sections', 'segments', 'tatums']



#for feature in features:
#    for track, data in audiodata:
