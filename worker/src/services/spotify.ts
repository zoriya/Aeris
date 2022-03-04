import SpotifyWebApi from 'spotify-web-api-node';
import { action, BaseService, reaction, service } from "../models/base-service";
import { Observable } from 'rxjs';
import { Utils } from '../utils';
import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";

@service(ServiceType.Spotify)
export class Spotify extends BaseService {

	private _spotify;

	constructor(pipeline: Pipeline) {
		super();
		if (!("Spotify" in pipeline.userData))
			throw new Error("User is not authenticated via Spotify");
		this._spotify = new SpotifyWebApi({
			accessToken: pipeline.userData["Spotify"].accessToken,
			refreshToken: pipeline.userData["Spotify"].refreshToken,
			clientId: process.env["SPOTIFY_CLIENT_ID"],
			clientSecret: process.env["SPOTIFY_SECRET"],
		});
	}

	private async _refreshIfNeeded(): Promise<void> {
		
	}

	@action(PipelineType.OnSpotifyAddToPlaylist, ["playlistId"])
	listenAddToPlaylist(params: any): Observable<PipelineEnv> {
		return Utils.longPulling(async since => {
			await this._refreshIfNeeded();
			let ret = await this._spotify.getPlaylistTracks(params.playlistId);
			return ret.body.items
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
			await this._refreshIfNeeded();
			let ret = await this._spotify.getMySavedTracks();
			return ret.body.items
				.filter(x => new Date(x.added_at) >= since)
				.map(x => ({
					ID: x.track.id,
					NAME: x.track.name,
				}));
		});
	}

	private async _searchTrack(artistName: string, trackName: string)  {
		await this._refreshIfNeeded();
		let searchResult = await this._spotify.searchTracks(`artist=${artistName}&track=${trackName}`);
		if (searchResult.body.tracks.total == 0)
			throw new Error(`Spotify API: '${trackName}' by ${artistName}: no such track`);
		return searchResult.body.tracks.items[0];
	}

	private async _searchPlaylist(playlistName: string)  {
		await this._refreshIfNeeded();
		let searchResult = await this._spotify.searchPlaylists(`name=${playlistName}&type=playlist`);
		if (searchResult.body.playlists.total == 0)
			throw new Error(`Spotify API: '${playlistName}': no such playlist`);
		return searchResult.body.playlists.items[0];
	}

	@reaction(ReactionType.PlayTrack, ['artist', 'track'])
	async playTrack(params: any): Promise<PipelineEnv> {
		await this._refreshIfNeeded();
		let track = await this._searchTrack(params['artist'], params['track']);
		await this._spotify.play({uris: [track.uri]});
		return {
			URL: track.uri,
			ARTIST: track.artists?.[0].name,
			TRACK: track.name,
		};
	}

	@reaction(ReactionType.Pause, [])
	async pause(params: any): Promise<PipelineEnv> {
		await this._refreshIfNeeded();
		await this._spotify.pause();
		return {};
	}

	@reaction(ReactionType.AddTrackToLibrary, ['artist', 'track'])
	async addTrackToLibrary(params: any): Promise<PipelineEnv> {
		await this._refreshIfNeeded();
		let track = await this._searchTrack(params['artist'], params['track']);
		await this._spotify.addToMySavedTracks([track.id]);
		return {
			URL: track.uri,
			ARTIST: track.artists?.[0].name,
			TRACK: track.name,
		};
	}

	@reaction(ReactionType.AddToPlaylist, ['artist', 'track', 'playlist'])
	async addToPlaylist(params: any): Promise<PipelineEnv> {
		await this._refreshIfNeeded();
		let playlist = await this._searchPlaylist( params['playlist']);
		let track = await this._searchTrack(params['artist'], params['track']);
		await this._spotify.addTracksToPlaylist(playlist.id, [track.uri]);
		return {
			URL: track.uri,
			ARTIST: track.artists?.[0].name,
			TRACK: track.name,
		};
	}

}
