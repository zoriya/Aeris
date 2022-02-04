import { groupBy, lastValueFrom, map, mergeAll, Observable, switchAll, tap } from "rxjs";
import { Twitter } from "./services/twitter";
import { Pipeline, PipelineEnv, PipelineType } from "./models/pipeline";
import { Runner } from "./runner";

export type ActionListener = (params: any) => Observable<PipelineEnv>;
export type ActionListeners = {[key: string]: ActionListener};

export const listenerFactory: ActionListeners = {
	[PipelineType.Twitter_OnTweet]: Twitter.listenTweet
};

export class Manager {
	private _pipelines: Observable<Pipeline>;

	constructor(pipelines: Observable<Pipeline>) {
		this._pipelines = pipelines
	}

	async run(): Promise<void> {
		await lastValueFrom(this._pipelines
			.pipe(
				groupBy((x: Pipeline) => x.id),
				switchAll(),
				map((x: Pipeline) =>
					listenerFactory[x.type](x.params).pipe(
						map((env: PipelineEnv) => [x, env])
					)
				),
				mergeAll(),
				tap(([x, env]: [Pipeline, PipelineEnv]) => {
					console.log(`Running pipeline ${x.name}`)
					new Runner(x).run(env)
				})
			));
	}
}
