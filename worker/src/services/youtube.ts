import { interval, mergeAll, mergeMap, Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType, ServiceType } from "../models/pipeline";
import { youtube_v3 } from '@googleapis/youtube';
import { BaseService, service } from "../models/base-service";
import { action } from "../models/base-service";

@service(ServiceType.Youtube)
export class Youtube extends BaseService {
	private _youtube: youtube_v3.Youtube;

	constructor(_: Pipeline) {
		super();
		this._youtube = new youtube_v3.Youtube({
			auth: process.env["YOUTUBE_KEY"]
		});
	}

	private _longPulling(call: (since: Date) => Promise<PipelineEnv[]>): Observable<PipelineEnv> {
		const startTime: number = Date.now();
		const delay = 60_000;

		return interval(delay).pipe(
			mergeMap(i => call(new Date(startTime + i * delay))),
			mergeAll(),
		);
	}

	@action(PipelineType.OnYtUpload, ["channel"])
	listenChannel(params: any): Observable<PipelineEnv> {
		return this._longPulling(async (since) => {
			const ret = await this._youtube.activities.list({
				part: ["snippet"],
				channelId: params.channel,
				maxResults: 25,
				publishedAfter: since.toISOString(),
			}, {});
			return ret.data.items.map(x => ({
				TITLE: x.snippet.title,
				DESCRIPTION: x.snippet.description,
				PUBLISHED_AT: x.snippet.publishedAt,
				CHANNEL_TITLE: x.snippet.channelTitle,
			}));

		})
	}

	@action(PipelineType.OnYtLike, [])
	listenLike(_: any): Observable<PipelineEnv> {
		// const channel = await this._youtube.channels.list({
		// 	part: ["contentDetails"],
		// 	mine: true,
		// }, {});
		// const playlistId = channel.data.items[0].contentDetails.relatedPlaylists.likes ?? "LL";
		const playlistId = "LL"; // LL seems to be a magic string for the liked playlist.
		return this._longPulling(async (since) => {
			const ret = await this._youtube.playlistItems.list({
				part: ["snippet"],
				playlistId,
			})
			return ret.data.items
				.filter(x => new Date(x.snippet.publishedAt) >= since)
				.map(x => ({
					TITLE: x.snippet.title,
					DESCRIPTION: x.snippet.description,
					LIKED_AT: x.snippet.publishedAt
				}));

		});
	}
};
