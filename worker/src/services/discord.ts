import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { action, BaseService, reaction, service } from "../models/base-service";
import { Client, Intents, Message } from "discord.js";
import { fromEvent, map, Observable } from "rxjs";

@service(ServiceType.Github)
export class Discord extends BaseService {
	private _client: Client<boolean>;

	constructor(_: Pipeline) {
		super();
		this._client = new Client({ intents: [Intents.FLAGS.GUILDS] });
	}

	@action(PipelineType.OnDiscordMessage, [])
	listenMessages(_: any): Observable<PipelineEnv> {
		return fromEvent(this._client, "messageCreated")
			.pipe(
				map((x: Message) => ({
					MESSAGE: x.content,
					AUTHOR: x.author.username,
				})),
			);
	}

	@reaction(ReactionType.PostDiscordMessage)
	async postMessage(_:any): Promise<PipelineEnv> {

	}

	@reaction(ReactionType.PostDiscordDM, ['other_id', 'message'])
	async postDM(params: any): Promise<PipelineEnv> {
		let res = await this._client.users.fetch(params['other_id']).then(user => 
			user.send(params['message']));
		return {
			USER_ID: res.author.id,
			USERNAME: res.author.username,
			SENDEE_ID: res.member.user.id,
			SENDEE_USERNAME: res.member.user.username,
			MESSAGE: res.content,
		};
	}

	@reaction(ReactionType.markDiscordMessageAsread)
	async markAsread(_:any): Promise<PipelineEnv> {
	}

	@reaction(ReactionType.joinDiscordServer)
	async joinServer(_:any): Promise<PipelineEnv> {
		
	}

	@reaction(ReactionType.leaveDiscordServer, ['server_id'])
	async leaveServer(params :any): Promise<PipelineEnv> {
		let guild = await this._client.guilds.fetch(params['server_id']);
		guild.leave();
		return {
			SERVER_NAME: guild.name,
			SERVER_ID: guild.id,
			USER_ID: guild.me.user.id,
			USERNAME: guild.me.user.username,
		}
	}

	@reaction(ReactionType.setDiscordStatus, ['status'])
	async setStatus(params:any): Promise<PipelineEnv> {
		let res = await this._client.user.setStatus(params['status']);
		return {
			USER_ID: res.userId,
			USERNAME: res.user.username,
			STATUS: params['status']
		};
	}
}
