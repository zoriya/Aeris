import { reactionHandlers } from "./reactions";
import { Pipeline, PipelineEnv } from "./models/pipeline"


export class Runner {
	private _pipeline: Pipeline;
	private _history: PipelineEnv[];

	constructor(pipeline: Pipeline) {
		this._pipeline = pipeline;
		this._history = [];
	}

	async run(env: PipelineEnv): Promise<void> {
		this._history.push(env);
		for (let reaction of this._pipeline.reactions) {
			const params = this._processParams(reaction.params);
			const newValues = await reactionHandlers[reaction.type](params);
			env = {...env, ...newValues};
			this._history.push(env);
		}
	}

	private _processParams(params: object): object {
		const ret: any = {};
		for (let [key, value] of Object.entries(params)) {
			let newValue = value;
			if (value instanceof String) {
				newValue = value.replace(/{(\w*)(?:@(\d))?}/, (_, name, index) => {
					if (index)
						return this._history[parseInt(index)][name]
					return this._history[this._history.length - 1][name]
				});
			}
			ret[key] = newValue;
		}
		return ret;
	}
}
