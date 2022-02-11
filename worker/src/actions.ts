import { catchError, groupBy, lastValueFrom, map, mergeMap, NEVER, Observable, switchAll, tap } from "rxjs";
import { Twitter } from "./services/twitter";
import { Pipeline, PipelineEnv, PipelineType } from "./models/pipeline";
import { Runner } from "./runner";
import { Youtube } from "./services/youtube";

export type ActionListener = (params: any) => Observable<PipelineEnv>;
export type ActionListeners = {[key: string]: ActionListener};

export const listenerFactory: ActionListeners = {
	[PipelineType.Twitter_OnTweet]: Twitter.listenTweet,
	[PipelineType.Youtube_OnUpload]: Youtube.listenChannel,
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
				mergeMap((x: Pipeline) =>
					this.createPipeline(x).pipe(
						map((env: PipelineEnv) => [x, env]),
						catchError(err => this.handlePipelineError(x, err)),
					)
				),
				tap(([x, env]: [Pipeline, PipelineEnv]) => {
					console.log(`Running pipeline ${x.name}`)
					console.table(env)
					new Runner(x).run(env)
				}),
			));
	}

	createPipeline(pipeline: Pipeline): Observable<PipelineEnv> {
		try {
			return listenerFactory[pipeline.type](pipeline.params)
		} catch (err) {
			return this.handlePipelineError(pipeline, err);
		}
	}

	handlePipelineError(pipeline: Pipeline, error: Error): Observable<never> {
		console.error(`Unhandled exception while trying to listen for the pipeline ${pipeline.name} (type: ${pipeline.type.toString()}).`, error)
		// TODO call the api to inform of the issue
		return NEVER;
	}
}
