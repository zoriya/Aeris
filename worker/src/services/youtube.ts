import { Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { youtube_v3 } from '@googleapis/youtube';
import { BaseService, reaction, service } from "../models/base-service";
import { action } from "../models/base-service";
import { Utils } from "../utils";

@service(ServiceType.Youtube)
export class Youtube extends BaseService {
	private _youtube: youtube_v3.Youtube;

	constructor(_: Pipeline) {
		super();
		this._youtube = new youtube_v3.Youtube({
			auth: process.env["YOUTUBE_KEY"]
		});
	}

	@action(PipelineType.OnYtUpload, ["channel"])
	listenChannel(params: any): Observable<PipelineEnv> {
		return Utils.longPulling(async (since) => {
			const ret = await this._youtube.activities.list({
				part: ["snippet"],
				channelId: params.channel,
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
	}

	@reaction(ReactionType.YtAddToPlaylist, ["videoId", "playlistId"])
	async reactPlaylist(params: any): Promise<PipelineEnv> {
		await this._youtube.playlistItems.insert({
			requestBody: {
				snippet: {
					resourceId: {
						videoId: params.videoId,
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
