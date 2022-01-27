import express from "express";
import expressWs, { Application } from "express-ws"
import WebSocket from "ws"

const app: Application = expressWs(express()).app;
const port = process.env.PORT || 8999;


app.ws("/ws-path", (ws: WebSocket) => {
	ws.on("message", (message: string) => {
		console.log("received: %s", message);
		ws.send(`Hello, you sent -> ${message}`);
	});

	ws.send("Hi there, I am a WebSocket server");
});

app.listen(port, () => {
	console.log(`Server started on port ${port}`);
});
