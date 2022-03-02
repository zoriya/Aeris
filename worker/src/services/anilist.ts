import { Pipeline, PipelineEnv, ReactionType, ServiceType } from "../models/pipeline";
import { BaseService, reaction, service } from "../models/base-service";
import { GraphQLClient, gql } from 'graphql-request'

@service(ServiceType.Anilist)
export class Anilist extends BaseService {
	private _client: GraphQLClient;

	constructor(_: Pipeline) {
		super();
		this._client = new GraphQLClient("https://graphql.anilist.co/");
	}
	
	@reaction(ReactionType.ToggleFavourite, ["about"])
	async updateAbout(params: any): Promise<PipelineEnv> {
		await this._client.request(gql`
			mutation($about: Int) {
				UpdateUser(about: $about){
				}
			}
		`, {
			about: params.about,
		});
		return {};
	}

	@reaction(ReactionType.ToggleFavourite, ["animeId"])
	async toggleFavourite(params: any): Promise<PipelineEnv> {
		await this._client.request(gql`
			mutation($animeId: Int) {
				ToggleFavourite(animeId: $animeId){
				}
			}
		`, {
			animeId: params.animeId,
		});
		return {};
	}
}
