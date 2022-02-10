import { interval, mergeMap, Observable } from "rxjs";
import { PipelineEnv } from "../models/pipeline";
import { youtube_v3 } from '@googleapis/youtube';

export class Youtube {
	static async pullVideos(youtube: youtube_v3.Youtube): Promise<PipelineEnv> {
		const ret = await youtube.activities.list({
			part: ["snippet", "contentDetails"],
			channelId: "UC_x5XG1OV2P6uZZ5FSM9Ttw",
			maxResults: 25,
		}, {});
		console.log(JSON.stringify(ret.data, undefined, 4));
		return {};
	}

	static listenChannel(params: any): Observable<PipelineEnv> {
		const youtube = new youtube_v3.Youtube({
			auth: process.env["YOUTUBE_KEY"]
		});

		return interval(1000).pipe(
			mergeMap(() => Youtube.pullVideos(youtube))
		);
	}
}
