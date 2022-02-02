import { Pipeline, PipelineEnv } from "./models/pipeline"

export type ReactionHandler = (params: any) => Promise<PipelineEnv>;
export type ReactionHandlers = {[key: string]: ReactionHandler}

export class Runner {
	private _pipeline: Pipeline;
	private _history: PipelineEnv[];
	private _handlers: ReactionHandlers;

	constructor(pipeline: Pipeline, handlers: ReactionHandlers) {
		this._pipeline = pipeline;
		this._history = [];
		this._handlers = handlers;
	}

	async run(env: PipelineEnv): Promise<void> {
		this._history.push(env);
		for (let reaction of this._pipeline.reactions) {
			const params = this._processParams(reaction.params);
			const newValues = await this._handlers[reaction.type](params);
			env = {...env, ...newValues};
			this._history.push(env);
		}
	}

	private _processParams(params: object): object {
		const ret = {};
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
