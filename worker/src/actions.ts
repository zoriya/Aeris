import { catchError, groupBy, lastValueFrom, map, mergeMap, NEVER, Observable, switchAll, tap } from "rxjs";
import { Pipeline, PipelineEnv } from "./models/pipeline";
import { servicesFactory } from "./models/services";
import { Runner } from "./runner";


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
			const service = servicesFactory[pipeline.service](pipeline);
			return service.getAction(pipeline.type)(pipeline.params)
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
