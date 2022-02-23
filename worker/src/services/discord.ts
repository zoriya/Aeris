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

	@reaction(ReactionType.PostDiscordDM)
	async postDM(_:any): Promise<PipelineEnv> {
		
	}

	@reaction(ReactionType.markDiscordMessageAsread)
	async markAsread(_:any): Promise<PipelineEnv> {
	}

	@reaction(ReactionType.joinDiscordServer)
	async joinServer(_:any): Promise<PipelineEnv> {
		
	}

	@reaction(ReactionType.leaveDiscordServer)
	async leaveServer(_:any): Promise<PipelineEnv> {
		let guilds = await this._client.guilds.
	}

	@reaction(ReactionType.setDiscordStatus, ['status'])
	async setStatus(params:any): Promise<PipelineEnv> {
		let res = await this._client.user.setStatus(params['status']);
		return {
			'user_id': res.userId,
			'username': res.user.username,
			'status': params['status']
		};
	}
}
