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
		const metadata: ActionMetadata = BaseService.actions[this.constructor.name][PipelineType[action]];
		return (params: any) => {
			for (let required of metadata.parameters) {
				if (!(required in params))
					throw new TypeError(`Expected the parameter ${required} but it was not found.`);
			}
			// @ts-ignore
			return this[metadata.accessor].apply(this, [params])
		};
	}

	getReaction(reaction: ReactionType): (params: any) => Promise<PipelineEnv> {
		return undefined;
	}
};

