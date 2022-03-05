import { Pipeline, PipelineEnv, ReactionType, ServiceType } from "../models/pipeline";
import { BaseService, reaction, service } from "../models/base-service";
import { GraphQLClient, gql } from 'graphql-request'

@service(ServiceType.Anilist)
export class Anilist extends BaseService {
	private _client: GraphQLClient;

	constructor(pipeline: Pipeline) {
		super();
		this._client = new GraphQLClient("https://graphql.anilist.co/")
			.setHeader("Authorization", `Bearer ${pipeline.userData["Anilist"].accessToken}`);
	}
	
	@reaction(ReactionType.UpdateAbout, ["about"])
	async updateAbout(params: any): Promise<PipelineEnv> {
		await this._client.request(gql`
			mutation($about: String) {
				UpdateUser(about: $about){
					id
				}
			}
		`, {
			about: params.about,
		});
		return {};
	}

	@reaction(ReactionType.ToggleFavourite, ["animeID"])
	async toggleFavourite(params: any): Promise<PipelineEnv> {
		await this._client.request(gql`
			mutation($animeId: Int) {
				ToggleFavourite(animeId: $animeId) {
					anime(page: 1, perPage: 0) {
						nodes {
							id
						}
					}
				}
			}
		`, {
			animeId: params.animeID,
		});
		return {};
	}
}
