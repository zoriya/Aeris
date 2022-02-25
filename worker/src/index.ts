import { fromEvent, merge, Observable } from "rxjs";
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
	fetch(`${process.env["API_URL"]}/workflow/${req.params.id}?API_KEY=${process.env["API_KEY"]}`)
		.then(res => {
			pipelineEvent.emit("event", res.json());
		});
});

app.post("/workflow/:id", req => {
	fetch(`${process.env["API_URL"]}/workflow/${req.params.id}?API_KEY=${process.env["API_KEY"]}`)
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

const pipelines = merge(
	fromFetch<Pipeline>(`${process.env["API_URL"]}/workflows?API_KEY=${process.env["API_KEY"]}`, {selector: x => x.json()}),
	fromEvent(pipelineEvent, "event"),
) as Observable<Pipeline>;

const manager: Manager = new Manager(pipelines);
await manager.run()
