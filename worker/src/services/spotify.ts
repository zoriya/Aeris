import SpotifyWebApi from 'spotify-web-api-js';
import { Pipeline, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { BaseService, reaction, service } from "../models/base-service";

@service(ServiceType.Spotify)
export class Spotify extends BaseService {

	private _spotify;

	constructor(_: Pipeline) {
		super();
		// TODO load credentials
		this._spotify = new SpotifyWebApi();
	}

	private async searchTrack(artistName: string, trackName: string)  {
		return this._spotify.searchTracks(`artist=${artistName}&track=${trackName}`).then(
			function (searchResult) {
				if (searchResult.tracks.total == 0)
					throw new Error(`Spotify API: '${trackName}' by ${artistName}: no such track`);
				return searchResult.tracks.items[0];
			}
		);
	}

	private async searchPlaylist(playlistName: string)  {
		return this._spotify.searchPlaylists(`name=${playlistName}&type=playlist`).then(
			function (searchResult) {
				if (searchResult.playlists.total == 0)
					throw new Error(`Spotify API: '${playlistName}': no such playlist`);
				return searchResult.playlists.items[0];
			}
		);
	}

	@reaction(ReactionType.playTrack, ['artist', 'track'])
	playTrack(params: any) {
		this.searchTrack(params['artist'], params['track']).then(
			function (track) {
				this._spotify.playTrack([track.id]);
			}
		);
	}

	@reaction(ReactionType.addTrackToLibrary, ['artist', 'track'])
	addTrackToLibrary(params: any) {
		this.searchTrack(params['artist'], params['track']).then(
			function (track) {
				this._spotify.addToMySavedTracks([track.id]);
			}
		);
		
	}


	@reaction(ReactionType.addToPlaylist, ['artist', 'track', 'playlist'])
	addToPlaylist(params: any) {
		this.searchPlaylist(params['playlist']).then(
			function (playlist) {
				this.searchTrack(params['artist'], params['track']).then(
					function (track) {
						this._spotify.addToPlaylist(playlist.id, [track.uri]);
					}
				)
			}
		);
	}

}
