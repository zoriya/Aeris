import { interval, mergeAll, mergeMap, Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType } from "../models/pipeline";
import { youtube_v3 } from '@googleapis/youtube';
import { BaseService } from "../models/base-service";
import { action } from "../models/base-service";

export class Youtube extends BaseService {
	private _youtube: youtube_v3.Youtube;

	constructor(_: Pipeline) {
		super();
		this._youtube = new youtube_v3.Youtube({
			auth: process.env["YOUTUBE_KEY"]
		});
	}

	longPulling(call: (since: Date) => Promise<PipelineEnv[]>): Observable<PipelineEnv> {
		const startTime: number = Date.now();
		const delay = 60_000;

		return interval(delay).pipe(
			mergeMap(i => call(new Date(startTime + i * delay))),
			mergeAll(),
		);
	}

	@action(PipelineType.OnUpload, ["channel"])
	listenChannel(params: any): Observable<PipelineEnv> {
		return this.longPulling(async (since) => {
			const ret = await this._youtube.activities.list({
				part: ["snippet"],
				channelId: params.channel,
				maxResults: 25,
				publishedAfter: since.toISOString(),
			}, {});
			console.log(JSON.stringify(ret.data, undefined, 4));
			return ret.data.items.map(x => ({
				TITLE: x.snippet.title,
				DESCRIPTION: x.snippet.description,
				PUBLISHED_AT: x.snippet.publishedAt,
				CHANNEL_TITLE: x.snippet.channelTitle,
			}));

		})
	}
}
