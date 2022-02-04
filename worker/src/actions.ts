import { map, mergeAll, Observable } from "rxjs";
import { Pipeline, PipelineEnv } from "./models/pipeline";
import { Runner } from "./runner";

export type ActionListener = (params: any) => Observable<PipelineEnv>;
export type ActionListeners = {[key: string]: ActionListener};

export const listenerFactory: ActionListeners = {

};

export class Manager {
	private _pipelines: Observable<Pipeline>;

	constructor(pipelines: Observable<Pipeline>) {
		this._pipelines = pipelines
	}

	async run(): Promise<void> {
		this._pipelines
			.pipe(
				map((x: Pipeline) =>
					listenerFactory[x.type](x.params).pipe(
						map((env: PipelineEnv) => [x, env])
					)
				),
				mergeAll()
			)
			.subscribe(([x, env]: [Pipeline, PipelineEnv]) => {
				new Runner(x).run(env)
			});
	}
}
