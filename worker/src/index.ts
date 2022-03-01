import { fromEvent, mergeAll, mergeWith, Observable } from "rxjs";
import { fromFetch } from 'rxjs/fetch';
import { Manager } from "./actions";
import "./services";
import fetch from 'node-fetch';
import AbortController from 'abort-controller';
import { Pipeline, PipelineType } from "./models/pipeline";
import { EventEmitter } from "events"
import express from "express";

// @ts-ignore
global.fetch  = fetch;
global.AbortController = AbortController;

const app = express()
const pipelineEvent = new EventEmitter();
app.put("/workflow/:id", req => {

	fetch(`${process.env["WORKER_API_URL"]}/workflow/${req.params.id}?WORKER_API_KEY=${process.env["WORKER_API_KEY"]}`)
		.then(res => {
			pipelineEvent.emit("event", res.json());
		});
});

app.post("/workflow/:id", req => {
	fetch(`${process.env["WORKER_API_URL"]}/workflow/${req.params.id}?WORKER_API_KEY=${process.env["WORKER_API_KEY"]}`)
		.then(res => {
			pipelineEvent.emit("event", res.json());
		});
});

app.delete("/workflow/:id", req => {
	pipelineEvent.emit("event", {
		id: req.params.id,
		type: PipelineType.Never,
	});
});

app.listen(5000);


const pipelines = fromFetch<Pipeline[]>(`${process.env["WORKER_API_URL"]}/workflows?WORKER_API_KEY=${process.env["WORKER_API_KEY"]}`, {selector: x => x.json()})
		.pipe(
			mergeAll(),
			mergeWith(fromEvent(pipelineEvent, "event")),
		) as Observable<Pipeline>;

const manager: Manager = new Manager(pipelines);
await manager.run()
