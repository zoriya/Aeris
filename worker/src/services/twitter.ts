import { exhaustMap, from, fromEventPattern, map, Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { ETwitterStreamEvent, TweetStream, TwitterApi } from "twitter-api-v2";
import { action, BaseService, reaction, service } from "../models/base-service";

@service(ServiceType.Twitter)
export class Twitter extends BaseService {
	constructor(_: Pipeline) {
		super();
	}

	private static _createTwitter() {
		return new TwitterApi(); ///TODO Get API KEY
	}

	private static async _createStream(): Promise<TweetStream> {
		const client: TwitterApi = this._createTwitter();
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

	@reaction(ReactionType.followUser, ['user_name'])
	static async followUser(params: any): Promise<PipelineEnv> {
		let client: TwitterApi = this._createTwitter();
		let user = await client.v2.userByUsername(params['user_name']);
		client.v2.follow((await client.currentUser()).id_str, user.data.id);
		return {
			FOLLOWED_ID: user.data.id,
			FOLLOWED_NAME: user.data.name,
			FOLLOWED_BIO: user.data.description,
			FOLLOWED_URL: user.data.url,
		}
	}

	@reaction(ReactionType.postTweet, ['tweet_content'])
	static async postTweet(params: any): Promise<PipelineEnv> {
		let client: TwitterApi = this._createTwitter();
		let tweet = await client.v2.tweet(params['tweet_content']);
		return {
			TWEET_ID: tweet.data.id,
			TWEET_CONTENT: tweet.data.text,
		}
	}

	@reaction(ReactionType.replyToTweet, ['tweet_id', 'reply_body'])
	static async replyToTweet(params: any): Promise<PipelineEnv> {
		let client: TwitterApi = this._createTwitter();
		let reply = await client.v2.reply(
			params['reply_body'],
			params['tweet_id'],
		  );
		return {
			TWEET_ID: params['tweet_id'],
			REPLY_ID: reply.data.id,
			REPLY_CONTENT: reply.data.text,
		}
	}

	@reaction(ReactionType.likeTweet, ['tweet_id'])
	static async likeTweet(params: any): Promise<PipelineEnv> {
		let client: TwitterApi = this._createTwitter();
		let tweet = (await client.v2.tweets([params['tweet_id']])).data[0];
		return {
			TWEET_ID: tweet.id,
			TWEET_CONTENT: tweet.text,
			TWEET_AUTHOR_ID: tweet.author_id
		}
	}

	@reaction(ReactionType.retweet, ['tweet_id'])
	static async retweet(params: any): Promise<PipelineEnv> {
		let client: TwitterApi = this._createTwitter();
		let tweet = await client.v2.retweet((await client.currentUser()).id_str, params['tweet_id']);
		return {
			TWEET_ID: params['tweet_id']
		}
	}
};
