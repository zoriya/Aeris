import { fromEvent, Observable } from "rxjs";
import { Pipeline, PipelineEnv, PipelineType, ServiceType } from "../models/pipeline";
import { action, BaseService, service } from "../models/base-service";
import { EventEmitter } from "events"

@service(ServiceType.Utils)
export class UtilsService extends BaseService {
	public static triggerEvents = new EventEmitter();
	private _pipeline: Pipeline;

	constructor(pipeline: Pipeline) {
		super();
		this._pipeline = pipeline;
	}

	@action(PipelineType.OnTrigger, [])
	manualTrigger(_: any): Observable<PipelineEnv> {
		return fromEvent(UtilsService.triggerEvents, this._pipeline.id.toString());
	}
}
