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
						fetch(`${process.env["API_URL"]}/trigger/${x.id}?API_KEY=${process.env["API_KEY"]}`);
					} catch (err) {
						this.handlePipelineError(x, err);
					}
				}),
			));
	}

	createPipeline(pipeline: Pipeline): Observable<PipelineEnv> {
		if (pipeline.type === PipelineType.Never)
			return NEVER;

		try {
			const service = BaseService.createService(pipeline.service, pipeline);
			return service.getAction(pipeline.type)(pipeline.params)
		} catch (err) {
			return this.handlePipelineError(pipeline, err);
		}
	}

	handlePipelineError(pipeline: Pipeline, error: Error): Observable<never> {
		console.error(`Unhandled exception while trying to listen for the pipeline ${pipeline.name} (type: ${pipeline.type.toString()}).`, error)
		fetch(`${process.env["API_URL"]}/error/${pipeline.id}?API_KEY=${process.env["API_KEY"]}`);
		return NEVER;
	}
}
