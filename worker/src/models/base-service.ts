import { Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "./pipeline";

export type ServiceCreator = {[key: string]: (pipeline: Pipeline) => BaseService};

class ActionMetadata {
	accessor: string;
	parameters: string[];
}

export function service(type: ServiceType) {
	return (target: new (...args: any[]) => object) => {
		BaseService._serviceFactory[type] = target as any;
	}
};

export function action(type: PipelineType, args: string[]) {
	return function(target: any, property: string) {
		BaseService._actions[target.constructor.name] ??= {};
		BaseService._actions[target.constructor.name][PipelineType[type]] = { accessor: property, parameters: args };
	}
}

export function reaction(type: ReactionType, args: string[]) {
	return function(target: any, property: string) {
		BaseService._reactions[target.constructor.name] ??= {};
		BaseService._reactions[target.constructor.name][ReactionType[type]] = { accessor: property, parameters: args };
	}
}

export class BaseService {
	static _serviceFactory: ServiceCreator = {};
	static _actions: { [service: string]: { [action: string]: ActionMetadata } } = {};
	static _reactions: { [service: string]: { [reaction: string]: ActionMetadata } } = {};

	private _runWithParamsCheck(metadata: ActionMetadata): (params: any) => any {
		return (params: any) => {
			for (let required of metadata.parameters) {
				if (!(required in params))
					throw new TypeError(`Expected the parameter ${required} but it was not found.`);
			}
			// @ts-ignore
			return this[metadata.accessor].apply(this, [params])
		};
	}

	getAction(action: PipelineType): (params: any) => Observable<PipelineEnv> {
		const metadata: ActionMetadata = BaseService._actions[this.constructor.name][PipelineType[action]];
		if (!metadata)
			throw new TypeError(`Invalid action: ${action}`);
		return this._runWithParamsCheck(metadata);
	}

	getReaction(reaction: ReactionType): (params: any) => Promise<PipelineEnv> {
		const metadata: ActionMetadata = BaseService._reactions[this.constructor.name][ReactionType[reaction]];
		if (!metadata)
			throw new TypeError(`Invalid reaction: ${reaction}`);
		return this._runWithParamsCheck(metadata);
	}

	static createService(service: ServiceType, pipeline: Pipeline): BaseService {
		const ctr = BaseService._serviceFactory[service];
		if (!ctr)
			throw new TypeError(`Service not found ${service}`);	
		// @ts-ignore
		return new ctr(pipeline);
	}
};

