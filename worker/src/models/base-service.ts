import { Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType, ReactionType } from "./pipeline";

export type ServiceCreator = {[key: string]: (pipeline: Pipeline) => BaseService};

class ActionMetadata {
	accessor: string;
	parameters: string[];
}

export function action(type: PipelineType, args: string[]) {
	return function(target: any, property: string) {
		BaseService.actions[target.constructor.name] ??= {};
		BaseService.actions[target.constructor.name][PipelineType[type]] = { accessor: property, parameters: args };
	}
}

export class BaseService {
	static actions: { [service: string]: { [action: string]: ActionMetadata } } = {};

	getAction(action: PipelineType): (params: any) => Observable<PipelineEnv> {
		console.log(JSON.stringify(BaseService.actions, undefined, 4))
		let key: string = BaseService.actions[this.constructor.name][PipelineType[action]].accessor;
		// @ts-ignore
		return (params: any) => this[key].apply(this, [params]);
	}

	getReaction(reaction: ReactionType): (params: any) => Promise<PipelineEnv> {
		return undefined;
	}
};

