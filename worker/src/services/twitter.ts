import { from, Observable } from "rxjs";
import { PipelineEnv } from "../models/pipeline";
// import { ETwitterStreamEvent, TwitterApi } from "twitter-api-v2";

export class Twitter {
	static listenTweet(params: any): Observable<PipelineEnv> {
		return from([{} as PipelineEnv])
		// const client: TwitterApi = new TwitterApi();
		// const stream = client.v2.sampleStream({ autoConnect: false });
		// stream.on(ETwitterStreamEvent.Data, console.log);
		// stream.on(ETwitterStreamEvent.Connected, () => console.log('Stream is started.'));

		// stream.connect({ autoReconnect: true, autoReconnectRetries: Infinity });
	}

	static async reactTweet(params: any): Promise<PipelineEnv> {
		return {}
	}
}
