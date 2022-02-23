import SpotifyWebApi from 'spotify-web-api-js';
import { action, BaseService, reaction, service } from "../models/base-service";
import { Observable } from 'rxjs';
import { Utils } from '../utils';
import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";

@service(ServiceType.Spotify)
export class Spotify extends BaseService {

	private _spotify;

	constructor(_: Pipeline) {
		super();
		// TODO load credentials
		this._spotify = new SpotifyWebApi();
	}

	@action(PipelineType.OnSpotifyAddToPlaylist, ["playlistId"])
	listenAddToPlaylist(params: any): Observable<PipelineEnv> {
		return Utils.longPulling(async since => {
			let ret = await this._spotify.getPlaylistTracks(params.playlistId);
			return ret.items
				.filter(x => new Date(x.added_at) >= since)
				.map(x => ({
					ID: x.track.id,
					NAME: x.track.name,
				}));
		});
	}

	@action(PipelineType.OnSpotifySaveToLibrary, [])
	listenSaveToLibrary(_: any): Observable<PipelineEnv> {
		return Utils.longPulling(async since => {
			let ret = await this._spotify.getMySavedTracks();
			return ret.items
				.filter(x => new Date(x.added_at) >= since)
				.map(x => ({
					ID: x.track.id,
					NAME: x.track.name,
				}));
		});
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
	async playTrack(params: any): Promise<PipelineEnv> {
		let track = await this._searchTrack(params['artist'], params['track']);
		this._spotify.play({uris: [track.uri]});
		return {...params, 'url': track.uri}
	}

	@reaction(ReactionType.AddTrackToLibrary, ['artist', 'track'])
	async addTrackToLibrary(params: any): Promise<PipelineEnv> {
		let track = await this._searchTrack(params['artist'], params['track']);
		this._spotify.addToMySavedTracks([track.id]);
		return {...params, 'url': track.uri}
	}

	@reaction(ReactionType.AddToPlaylist, ['artist', 'track', 'playlist'])
	async addToPlaylist(params: any): Promise<PipelineEnv> {
		let playlist = await this._searchPlaylist( params['playlist']);
		let track = await this._searchTrack(params['artist'], params['track']);
		this._spotify.addTracksToPlaylist(playlist.id, [track.uri]);
		return {...params, 'url': track.uri}
	}

}
