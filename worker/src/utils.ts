import { interval, mergeAll, mergeMap, Observable } from "rxjs";
import { PipelineEnv } from "./models/pipeline";

export class Utils {
	static longPulling(call: (since: Date) => Promise<PipelineEnv[]>): Observable<PipelineEnv> {
		const startTime: number = Date.now();
		const delay = 60_000;

		return interval(delay).pipe(
			mergeMap(i => call(new Date(startTime + i * delay))),
			mergeAll(),
		);
	}
}
