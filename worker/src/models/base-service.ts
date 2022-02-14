import { Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType, ReactionType } from "./pipeline";

export type ServiceCreator = {[key: string]: (pipeline: Pipeline) => BaseService};

class ActionMetadata {
	accessor: string;
	parameters: string[];
}

export function action(type: PipelineType, args: string[]) {
	return function(target: Object, property: string) {
		const targetService = target.toString(); // TODO find the service type from the class's constructor
		BaseService.actions[targetService][type] = { accessor: property, parameters: args };
	}
}

export class BaseService {
	static actions: { [service: string]: { [action: string]: ActionMetadata } };

	getAction(action: PipelineType): (params: any) => Observable<PipelineEnv> {
		let key: string = BaseService.actions[this.constructor.name][action].accessor;
		// @ts-ignore
		return this[key];
	}

	getReaction(reaction: ReactionType): (params: any) => Promise<PipelineEnv> {
		return undefined;
	}
};

