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

app.put("/workflow", (req: Pipeline) => {
	pipelineEvent.emit("event", req);
});

app.post("/workflow", (req: Pipeline) => {
	pipelineEvent.emit("event", req);
});

app.delete("/workflow", (req: Pipeline) => {
	req.type = PipelineType.Never;
	pipelineEvent.emit("event", req);
});

app.listen(5000);

const pipelines = merge(
	fromFetch<Pipeline>(`${process.env["API_URL"]}/workflows?API_KEY=${process.env["API_KEY"]}`, {selector: x => x.json()}),
	fromEvent(pipelineEvent, "event"),
) as Observable<Pipeline>;

const manager: Manager = new Manager(pipelines);
await manager.run()
