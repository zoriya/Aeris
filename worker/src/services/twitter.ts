import { exhaustMap, from, fromEventPattern, map, Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType, ServiceType } from "../models/pipeline";
import { ETwitterStreamEvent, TweetStream, TwitterApi } from "twitter-api-v2";
import { action, BaseService, service } from "../models/base-service";

@service(ServiceType.Twitter)
export class Twitter extends BaseService {
	constructor(_: Pipeline) {
		super();
	}

	private static async _createStream(): Promise<TweetStream> {
		const client: TwitterApi = new TwitterApi();
		const stream = await client.v2.sampleStream();
		stream.on(ETwitterStreamEvent.Connected, () => console.log('Stream is started.'));
		stream.on(ETwitterStreamEvent.ConnectionError, err => console.log('Connection error!', err));
		stream.on(ETwitterStreamEvent.ConnectionClosed, () => console.log('Connection has been closed.'));
		return stream;
	}

	@action(PipelineType.OnTweet, [])
	static listenTweet(params: any): Observable<PipelineEnv> {
		return from(Twitter._createStream())
			.pipe(
				exhaustMap((stream: TweetStream) =>
					fromEventPattern(
						handler => stream.on(ETwitterStreamEvent.Data, handler),
						() => stream.close()
					)
				)
			);
	}

	static async reactTweet(params: any): Promise<PipelineEnv> {
		return {}
	}
};
