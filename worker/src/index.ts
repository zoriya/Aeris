// import express from "express";
// import expressWs, { Application } from "express-ws"
// import WebSocket from "ws"

import { from } from "rxjs";
import { Manager } from "./actions";
import { Pipeline, PipelineType, ReactionType } from "./models/pipeline";

// const app: Application = expressWs(express()).app;
// const port = process.env.PORT || 8999;


// app.ws("/ws-path", (ws: WebSocket) => {
// 	ws.on("message", (message: string) => {
// 		console.log("received: %s", message);
// 		ws.send(`Hello, you sent -> ${message}`);
// 	});

// 	ws.send("Hi there, I am a WebSocket server");
// });

// app.listen(port, () => {
// 	console.log(`Server started on port ${port}`);
// });

const pipelines: Pipeline[] = [
	{
		id: 1,
		userID: 2,
		enabled: true,
		lastTrigger: new Date(),
		triggerCount: 0,
		name: "toto",
		type: PipelineType.Youtube_OnUpload,
		params: {
			channel: "UCq-Fj5jknLsUf-MWSy4_brA"
		},
		reactions: [{
			id: 1,
			type: ReactionType.Twitter_Tweet,
			params: {}
		}]
	}
];
const manager: Manager = new Manager(from(pipelines));
await manager.run()
