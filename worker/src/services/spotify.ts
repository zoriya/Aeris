import SpotifyWebApi from 'spotify-web-api-node';
import { action, BaseService, reaction, service } from "../models/base-service";
import { Observable } from 'rxjs';
import { Utils } from '../utils';
import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";

@service(ServiceType.Spotify)
export class Spotify extends BaseService {

	private _pipeline: Pipeline;
	private _spotify;

	constructor(pipeline: Pipeline) {
		super();
		if (!("Spotify" in pipeline.userData))
			throw new Error("User is not authenticated via Spotify");
		this._pipeline = pipeline;
		this._spotify = new SpotifyWebApi({
			accessToken: pipeline.userData["Spotify"].accessToken,
			refreshToken: pipeline.userData["Spotify"].refreshToken,
			clientId: process.env["SPOTIFY_CLIENT_ID"],
			clientSecret: process.env["SPOTIFY_SECRET"],
		});
	}

	private async _refreshIfNeeded(): Promise<void> {
		if (Date.parse(this._pipeline.userData["Spotify"].expiresAt) >= Date.now() + 100_000)
			return;
		const ret = await this._spotify.refreshAccessToken();
		const data = this._pipeline.userData["Spotify"];
		data.accessToken = ret.body.access_token;
		if (ret.body.refresh_token)
			data.refreshToken = ret.body.refresh_token;
		data.expiresAt = new Date(Date.now() + ret.body.expires_in * 1000).toISOString();
		this._spotify.setRefreshToken(data.refreshToken);
		this._spotify.setAccessToken(data.accessToken);
		fetch(`${process.env["WORKER_API_URL"]}/spotify/${this._pipeline.userId}?WORKER_API_KEY=${process.env["WORKER_API_KEY"]}`, {
			method: "POST",
			headers: {
				"Content-Type": "application/json",
			},
			body: JSON.stringify(data),
		});
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

	@reaction(ReactionType.PlayTrack, ['trackUri'])
	async playTrack(params: any): Promise<PipelineEnv> {
		await this._refreshIfNeeded();
		await this._spotify.play({uris: [params.trackUri]});
		return {};
	}

	@reaction(ReactionType.Pause, [])
	async pause(_: any): Promise<PipelineEnv> {
		await this._refreshIfNeeded();
		await this._spotify.pause();
		return {};
	}

	@reaction(ReactionType.AddTrackToLibrary, ['trackUri'])
	async addTrackToLibrary(params: any): Promise<PipelineEnv> {
		await this._refreshIfNeeded();
		await this._spotify.addToMySavedTracks([params.trackUri]);
		return {};
	}

	@reaction(ReactionType.AddToPlaylist, ['trackUri', 'playlistUri'])
	async addToPlaylist(params: any): Promise<PipelineEnv> {
		await this._refreshIfNeeded();
		await this._spotify.addTracksToPlaylist(params.playlistUri, ["spotify:track:" + params.trackUri]);
		return {};
	}

}
