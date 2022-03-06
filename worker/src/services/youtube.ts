import { Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { youtube_v3 } from '@googleapis/youtube';
import { BaseService, reaction, service } from "../models/base-service";
import { action } from "../models/base-service";
import { Utils } from "../utils";
import { OAuth2Client } from "google-auth-library";

@service(ServiceType.Youtube)
export class Youtube extends BaseService {
	private _youtube: youtube_v3.Youtube;

	constructor(pipeline: Pipeline) {
		super();
		if (!("Google" in pipeline.userData))
			throw new Error("User not authenticated via google");
		const client = new OAuth2Client({
			clientId: process.env["GOOGLE_CLIENT_ID"],
			clientSecret: process.env["GOOGLE_SECRET"],
		});
		client.setCredentials({
			refresh_token: pipeline.userData["Google"].refreshToken,
			access_token: pipeline.userData["Google"].accessToken,
		});
		client.on("tokens", x => {
			fetch(`${process.env["WORKER_API_URL"]}/google/${pipeline.userId}?WORKER_API_KEY=${process.env["WORKER_API_KEY"]}`, {
				method: "POST",
				headers: {
					"Content-Type": "application/json",
				},
				body: JSON.stringify({
					accessToken: x.access_token,
					refreshToken: x.refresh_token,
					expiresAt: x.expiry_date,
				}),
			});
		});
		this._youtube = new youtube_v3.Youtube({
			auth: client,
		});
	}

	@action(PipelineType.OnYtUpload, ["channel_id"])
	listenChannel(params: any): Observable<PipelineEnv> {
		return Utils.longPulling(async (since) => {
			const ret = await this._youtube.activities.list({
				part: ["snippet", "contentDetails"],
				channelId: params.channel_id,
				maxResults: 25,
				publishedAfter: since.toISOString(),
			}, {});
			return ret.data.items
				.filter(x => x.snippet.type === "upload")
				.map(x => ({
					ID: x.contentDetails.upload.videoId,
					CHANNEL_ID: x.snippet.channelId,
					TITLE: x.snippet.title,
					DESCRIPTION: x.snippet.description,
					PUBLISHED_AT: x.snippet.publishedAt,
					CHANNEL_TITLE: x.snippet.channelTitle,
				}));

		})
	}

	@action(PipelineType.OnYtLike, [])
	listenLike(_: any): Observable<PipelineEnv> {
		// const channel = await this._youtube.channels.list({
		// 	part: ["contentDetails"],
		// 	mine: true,
		// }, {});
		// const playlistId = channel.data.items[0].contentDetails.relatedPlaylists.likes ?? "LL";
		const playlistId = "LL"; // LL seems to be a magic string for the liked playlist.
		return this.listenPlaylist({playlistId})
	}

	@action(PipelineType.OnYtPlaylistAdd, ["playlistId"])
	listenPlaylist(params: any): Observable<PipelineEnv> {
		return Utils.longPulling(async (since) => {
			const ret = await this._youtube.playlistItems.list({
				part: ["snippet"],
				playlistId: params.playlistId,
			})
			return ret.data.items
				.filter(x => new Date(x.snippet.publishedAt) >= since)
				.map(x => ({
					ID: x.snippet.resourceId.videoId,
					CHANNEL_ID: x.snippet.videoOwnerChannelId,
					CHANNEL_TITLE: x.snippet.videoOwnerChannelTitle,
					TITLE: x.snippet.title,
					DESCRIPTION: x.snippet.description,
					LIKED_AT: x.snippet.publishedAt,
				}));

		});
	}

	@reaction(ReactionType.YtLike, ["videoId"])
	async reactLike(params: any): Promise<PipelineEnv> {
		await this._youtube.videos.rate({
			id: params.videoId,
			rating: "like",
		});
		return {
			ID: params.videoId,
		};
	}

	@reaction(ReactionType.YtComment, ["videoId", "body"])
	async reactComment(params: any): Promise<PipelineEnv> {
		try {
		let infos = await this._youtube.commentThreads.insert({
			part: ["snippet"],
			requestBody: {
				snippet: {
					videoId: params.videoId,
					topLevelComment: {
						snippet: {
							textOriginal: params.body
						}
					}
				}
			}
		});
		return {
			ID: infos.data.id,
		}
		} catch(e) {
			console.log(`youtube react comment error: ${e}`);
			throw new Error("Impossible to comment on this video. Comments may be disabled.");
		}
	}

	@reaction(ReactionType.YtAddToPlaylist, ["videoId", "playlistId"])
	async reactPlaylist(params: any): Promise<PipelineEnv> {
		await this._youtube.playlistItems.insert({
			part: ["snippet"],
			requestBody: {
				snippet: {
					resourceId: {
						videoId: params.videoId,
						kind: "youtube#video"
					},
					playlistId: params.playlistId,
				},
			}
		}, {});
		return {
			VIDEO_ID: params.videoId,
			PLAYLIST_ID: params.playlistId,
		}
	}
};
