import { catchError, filter, groupBy, lastValueFrom, map, mergeMap, NEVER, Observable, switchAll, tap } from "rxjs";
import { BaseService } from "./models/base-service";
import { Pipeline, PipelineEnv, PipelineType } from "./models/pipeline";
import { Runner } from "./runner";


export class Manager {
	private _pipelines: Observable<Pipeline>;

	constructor(pipelines: Observable<Pipeline>) {
		this._pipelines = pipelines
	}

	async run(): Promise<void> {
		await lastValueFrom(this._pipelines
			.pipe(
				tap(x => console.log(`Found pipeline ${JSON.stringify(x)}`)),
				filter(x => x.enabled),
				groupBy((x: Pipeline) => x.id),
				switchAll(),
				mergeMap((x: Pipeline) =>
					this.createPipeline(x).pipe(
						map((env: PipelineEnv) => [x, env]),
						catchError(err => this.handlePipelineError(x, err)),
					)
				),
				tap(async ([x, env]: [Pipeline, PipelineEnv]) => {
					console.log(`Running pipeline ${x.name}`)
					try {
						await new Runner(x).run(env);
						fetch(`${process.env["WORKER_API_URL"]}/trigger/${x.id}?WORKER_API_KEY=${process.env["WORKER_API_KEY"]}`);
					} catch (err) {
						this.handlePipelineError(x, err);
					}
				}),
			));
	}

	createPipeline(pipeline: Pipeline): Observable<PipelineEnv> {
		if (pipeline.type === PipelineType.Never) {
			console.log(`Deleting the pipeline ${pipeline.name}`);
			return NEVER;
		}

		try {
			const service = BaseService.createService(pipeline.service, pipeline);
			console.log(`Creating an observable for the pipeline ${pipeline.name} - ${pipeline.type} (${pipeline.service})`);
			return service.getAction(pipeline.type)(pipeline.params)
		} catch (err) {
			return this.handlePipelineError(pipeline, err);
		}
	}

	handlePipelineError(pipeline: Pipeline, error: Error): Observable<never> {
		console.error(`Unhandled exception while trying to listen for the pipeline ${pipeline.name} (type: ${pipeline.type?.toString()}).`, error)
		fetch(`${process.env["WORKER_API_URL"]}/error/${pipeline.id}?WORKER_API_KEY=${process.env["WORKER_API_KEY"]}`, {
			method: "POST",
			body: JSON.stringify({error}),
		});
		return NEVER;
	}
}
