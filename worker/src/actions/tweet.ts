import { ETwitterStreamEvent, TwitterApi } from "twitter-api-v2";

export class TweetListener {
	async run(): Promise<void> {
		const client: TwitterApi = new TwitterApi();
		const stream = client.v2.sampleStream({ autoConnect: false });
		stream.on(ETwitterStreamEvent.Data, console.log);
		stream.on(ETwitterStreamEvent.Connected, () => console.log('Stream is started.'));

		await stream.connect({ autoReconnect: true, autoReconnectRetries: Infinity });
	}
}
