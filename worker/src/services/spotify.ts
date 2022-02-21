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

	private async _searchTrack(artistName: string, trackName: string)  {
		let searchResult = await this._spotify.searchTracks(`artist=${artistName}&track=${trackName}`);
		if (searchResult.tracks.total == 0)
			throw new Error(`Spotify API: '${trackName}' by ${artistName}: no such track`);
		return searchResult.tracks.items[0];
	}

	private async _searchPlaylist(playlistName: string)  {
		let searchResult = await this._spotify.searchPlaylists(`name=${playlistName}&type=playlist`);
		if (searchResult.playlists.total == 0)
			throw new Error(`Spotify API: '${playlistName}': no such playlist`);
		return searchResult.playlists.items[0];
	}

	@reaction(ReactionType.PlayTrack, ['artist', 'track'])
	async playTrack(params: any) {
		let track = await this._searchTrack(params['artist'], params['track']);
		this._spotify.play({uris: [track.uri]});
	}

	@reaction(ReactionType.AddTrackToLibrary, ['artist', 'track'])
	async addTrackToLibrary(params: any) {
		let track = await this._searchTrack(params['artist'], params['track']);
		this._spotify.addToMySavedTracks([track.id]);

	}


	@reaction(ReactionType.AddToPlaylist, ['artist', 'track', 'playlist'])
	async addToPlaylist(params: any) {
		let playlist = await this._searchPlaylist( params['playlist']);
		let track = await this._searchTrack(params['artist'], params['track']);
		this._spotify.addTracksToPlaylist(playlist.id, [track.uri]);
	}

}
