import { exhaustMap, from, fromEventPattern, map, Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { ETwitterStreamEvent, TweetStream, TwitterApi } from "twitter-api-v2";
import { action, BaseService, reaction, service } from "../models/base-service";

@service(ServiceType.Twitter)
export class Twitter extends BaseService {
	private _twitter: TwitterApi;
	private _pipeline: Pipeline;
	
	constructor(pipeline: Pipeline) {
		super();
		this._pipeline = pipeline;
		this._twitter = new TwitterApi(pipeline.userData["Twitter"].accessToken);
	}

	// private async _createStream(): Promise<TweetStream> {
	// 	const stream = await this._.v2.sampleStream();
	// 	stream.on(ETwitterStreamEvent.Connected, () => console.log('Stream is started.'));
	// 	stream.on(ETwitterStreamEvent.ConnectionError, err => console.log('Connection error!', err));
	// 	stream.on(ETwitterStreamEvent.ConnectionClosed, () => console.log('Connection has been closed.'));
	// 	return stream;
	// }


	// @action(PipelineType.OnTweet, [])
	// listenTweet(params: any): Observable<PipelineEnv> {
	// 	return from(Twitter._createStream())
	// 		.pipe(
	// 			exhaustMap((stream: TweetStream) =>
	// 				fromEventPattern(
	// 					handler => stream.on(ETwitterStreamEvent.Data, handler),
	// 					() => stream.close()
	// 				)
	// 			)
	// 		);
	// }

	@reaction(ReactionType.FollowUser, ['user_name'])
	async followUser(params: any): Promise<PipelineEnv> {
		let user = await this._twitter.v2.userByUsername(params['user_name']);
		const me = (await this._twitter.v2.me()).data.id;
		this._twitter.v2.follow(me, user.data.id);
		return {
			FOLLOWED_ID: user.data.id,
			FOLLOWED_NAME: user.data.name,
			FOLLOWED_BIO: user.data.description,
			FOLLOWED_URL: user.data.url,
		}
	}

	@reaction(ReactionType.PostTweet, ['tweet_content'])
	async postTweet(params: any): Promise<PipelineEnv> {
		let tweet = await this._twitter.v2.tweet(params['tweet_content']);
		return {
			TWEET_ID: tweet.data.id,
			TWEET_CONTENT: tweet.data.text,
		}
	}

	@reaction(ReactionType.ReplyToTweet, ['tweet_id', 'reply_body'])
	async replyToTweet(params: any): Promise<PipelineEnv> {
		let reply = await this._twitter.v2.reply(
			params['reply_body'],
			params['tweet_id'],
		  );
		return {
			TWEET_ID: params['tweet_id'],
			REPLY_ID: reply.data.id,
			REPLY_CONTENT: reply.data.text,
		}
	}

	@reaction(ReactionType.LikeTweet, ['tweet_id'])
	async likeTweet(params: any): Promise<PipelineEnv> {
		const me = (await this._twitter.v2.me()).data.id;
		await this._twitter.v2.like(me, params['tweet_id']);
		let tweet = (await this._twitter.v2.tweets([params['tweet_id']])).data[0];
		return {
			TWEET_ID: tweet.id,
			TWEET_CONTENT: tweet.text,
			TWEET_AUTHOR_ID: tweet.author_id
		}
	}

	@reaction(ReactionType.Retweet, ['tweet_id'])
	async retweet(params: any): Promise<PipelineEnv> {
		let tweet = await this._twitter.v2.retweet((await this._twitter.v2.me()).data.id, params['tweet_id']);
		return {
			TWEET_ID: params['tweet_id']
		}
	}
};
