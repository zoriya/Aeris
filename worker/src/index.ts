import { fromEvent, mergeAll, mergeWith, Observable } from "rxjs";
import { fromFetch } from 'rxjs/fetch';
import { Manager } from "./actions";
import "./services";
import fetch from 'node-fetch';
import AbortController from 'abort-controller';
import { Pipeline, pipelineFromApi, PipelineType } from "./models/pipeline";
import { EventEmitter } from "events"
import express from "express";
import { UtilsService } from "./services/utils";

// @ts-ignore
global.fetch  = fetch;
global.AbortController = AbortController;

const app = express()
const pipelineEvent = new EventEmitter();
app.put("/workflow/:id", (req, res) => {
	console.log(`edit pipeline ${req.params.id}`);
	fetch(`${process.env["WORKER_API_URL"]}/workflow/${req.params.id}?WORKER_API_KEY=${process.env["WORKER_API_KEY"]}`)
		.then(async res => {
			pipelineEvent.emit("event", pipelineFromApi(await res.json()));
		})
		.catch(console.error);
	res.send()
});

app.post("/workflow/:id", (req, res) => {
	console.log(`new pipeline ${req.params.id}`);
	fetch(`${process.env["WORKER_API_URL"]}/workflow/${req.params.id}?WORKER_API_KEY=${process.env["WORKER_API_KEY"]}`)
		.then(async res => {
			pipelineEvent.emit("event", pipelineFromApi(await res.json()));
		})
		.catch(console.error);
	res.send()
});

app.delete("/workflow/:id", (req, res) => {
	console.log(`delete pipeline ${req.params.id}`);
	pipelineEvent.emit("event", {
		id: req.params.id,
		type: PipelineType.Never,
	});
	res.send()
});

app.put("/action/:id", (req, res) => {
	UtilsService.triggerEvents.emit(req.params.id, {});
	res.send();
});

app.listen(5000);


const pipelines = fromFetch<Pipeline[]>(`${process.env["WORKER_API_URL"]}/workflows?WORKER_API_KEY=${process.env["WORKER_API_KEY"]}`, {selector: async x => (await x.json()).map((y: any) => pipelineFromApi(y))})
		.pipe(
			mergeAll(),
			mergeWith(fromEvent(pipelineEvent, "event")),
		) as Observable<Pipeline>;

const manager: Manager = new Manager(pipelines);
await manager.run()
