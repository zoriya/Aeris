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
}
