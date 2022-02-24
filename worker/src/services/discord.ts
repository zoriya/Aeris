import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { action, BaseService, reaction, service } from "../models/base-service";
import { Client, GuildMember, Intents, Message, TextChannel } from "discord.js";
import { filter, fromEvent, map, Observable } from "rxjs";

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
					AUTHOR_ID: x.author.id,
					AUTHOR_NAME: x.author.username,
				})),
			);
	}

	@action(PipelineType.OnDiscordMention, [])
	listenMentions(_: any): Observable<PipelineEnv> {
		return fromEvent(this._client, "message")
			.pipe(
				filter((x: Message) => x.mentions.has(this._client.user)),
				map((x: Message) => ({
					MESSAGE: x.content,
					AUTHOR_ID: x.author.id,
					AUTHOR_NAME: x.author.username,
				})),
			);
	}

	@action(PipelineType.OnNewDiscordGuildMember, [])
	listenNewGuildMember(_: any): Observable<PipelineEnv> {
		return fromEvent(this._client, "guildMemberAdd")
			.pipe(
				map((member: GuildMember) => ({
					NEW_MEMBER_ID: member.user.id,
					NEW_MEMBER_NAME: member.user.username,
					SERVER_ID: member.guild.id,
					SERVER_NAME: member.guild.name

				})),
			);
	}

	@action(PipelineType.OnDiscordGuildLeave, [])
	listenGuildLeave(_: any): Observable<PipelineEnv> {
		return fromEvent(this._client, "guildMemberRemove")
			.pipe(
				map((member: GuildMember) => ({
					NEW_MEMBER_ID: member.user.id,
					NEW_MEMBER_NAME: member.user.username,
					SERVER_ID: member.guild.id,
					SERVER_NAME: member.guild.name

				})),
			);
	}

	@reaction(ReactionType.PostDiscordMessage, ['server_id', 'channel_id', 'content'])
	async postMessage(params :any): Promise<PipelineEnv> {
		let guild = await this._client.guilds.fetch(params['server_id']);
		let channel = guild.channels.cache.get(params['channel_id']);
		let message = await (<TextChannel> channel).send(params['content']);
		return {
			USER_ID: this._client.user.username,
			USERNAME: this._client.user.id,
			MESSAGE_CONTENT: message.content,
			CHANNEL_ID: channel.id,
			CHANNEL_NAME: channel.name,
			SERVER_ID: guild.id,
			SERVER_NAME: guild.name,
		};

	}

	@reaction(ReactionType.PostDiscordDM, ['other_id', 'content'])
	async postDM(params: any): Promise<PipelineEnv> {
		let res = await this._client.users.fetch(params['other_id']).then(user => 
			user.send(params['content']));
		return {
			USER_ID: res.author.id,
			USERNAME: res.author.username,
			SENDEE_ID: res.member.user.id,
			SENDEE_USERNAME: res.member.user.username,
			MESSAGE: res.content,
		};
	}

	@reaction(ReactionType.LeaveDiscordServer, ['server_id'])
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

	@reaction(ReactionType.SetDiscordStatus, ['status'])
	async setStatus(params:any): Promise<PipelineEnv> {
		let res = await this._client.user.setStatus(params['status']);
		return {
			USER_ID: res.user.id,
			USERNAME: res.user.username,
			STATUS: params['status']
		};
	}
}
