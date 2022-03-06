import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { action, BaseService, reaction, service } from "../models/base-service";
import { Client, GuildMember, Intents, Message, TextChannel } from "discord.js";
import { filter, fromEvent, map, Observable } from "rxjs";
import snoowrap, {Submission, Subreddit} from 'snoowrap';

@service(ServiceType.Reddit)
export class Reddit extends BaseService {
	private _client: snoowrap;

	constructor(pipeline: Pipeline) {
		super();
		this._client = new snoowrap({
			userAgent: "Aeris",
			accessToken: pipeline.userData["Reddit"].accessToken
		})
	}

	@reaction(ReactionType.JoinSubreddit, ['sub_id'])
	async joinSubreddit(params: any): Promise<PipelineEnv> {
		return this._client.getSubreddit(params['sub_id']).subscribe().then((response) => {
			return {
				SUB_NAME: response.name,
				SUB_ID: response.id
			};
		});
	}

	@reaction(ReactionType.LeaveSubreddit, ['sub_id'])
	async leaveSubreddit(params: any): Promise<PipelineEnv> {
		return this._client.getSubreddit(params['sub_id']).unsubscribe().then((response) => {
			return {
				SUB_NAME: response.name,
				SUB_ID: response.id
			};
		});
	}

	@reaction(ReactionType.PostInSubreddit, ['sub_id', 'title', 'body'])
	async postInSubreddit(params: any): Promise<PipelineEnv> {
		return this._client.getSubreddit(params['sub_id']).submitSelfpost({
			subredditName: params['sub_id'],
			title: params['title'],
			text: params['body']
		}).then((response) => {
			return {
				SUB_NAME: response.subreddit.name,
				SUB_ID: response.subreddit.id,
				POST_ID: response.id
			};
		});
	}

	@reaction(ReactionType.ReplyToPost, ['post_id', 'reply_body'])
	async commentPost(params: any): Promise<PipelineEnv> {
		let fetchedSubmission: Submission = this._client.getSubmission(params['post_id']);

		return fetchedSubmission.reply(params['reply_body']).then((res) => {
			return {
				SUB_NAME: fetchedSubmission.subreddit.name,
				SUB_ID: fetchedSubmission.subreddit.id,
				POST_ID: params['post_id'],
				REPLY_ID: res.id
			};
		});
	}

	@reaction(ReactionType.Upvote, ['post_id'])
	async upvotePost(params: any): Promise<PipelineEnv> {
		return this._client.getSubmission(params['post_id']).upvote().then((res) => {
			return {
				SUB_NAME: res.subreddit.name,
				SUB_ID: res.subreddit.id,
				POST_ID: params['post_id'],
				UPVOTE_COUNT: res.ups
			};
		});
	}

	@reaction(ReactionType.Downvote, ['post_id'])
	async downvotePost(params: any): Promise<PipelineEnv> {
		return this._client.getSubmission(params['post_id']).downvote().then((res) => {
			return {
				SUB_NAME: res.subreddit.name,
				SUB_ID: res.subreddit.id,
				POST_ID: res.id,
				DOWNVOTE_COUNT: res.downs
			};
		});
	}
}
