import { interval, mergeAll, mergeMap, Observable } from "rxjs";
import { PipelineEnv } from "../models/pipeline";
import { youtube_v3 } from '@googleapis/youtube';

export class Youtube {
	static async pullVideos(youtube: youtube_v3.Youtube, channel: string, since: Date): Promise<PipelineEnv[]> {
		const ret = await youtube.activities.list({
			part: ["snippet", "contentDetails"],
			channelId: channel,
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
	}

	static listenChannel(params: any): Observable<PipelineEnv> {
		if (!("channel" in params))
			throw new TypeError("Expected a channel parameter but none were given.");
		const youtube = new youtube_v3.Youtube({
			auth: process.env["YOUTUBE_KEY"]
		});
		const startTime: number = Date.now();
		const delay = 60_000;

		return interval(delay).pipe(
			mergeMap(i => Youtube.pullVideos(youtube, params.channel, new Date(startTime + i * delay))),
			mergeAll(),
		);
	}
}
