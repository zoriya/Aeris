import type { PipelineBoxProps } from "./components/Pipelines/PipelineBox";
import { GenericButtonProps } from "./components/GenericButton";
import { Typography, Box, Button } from "@mui/material";
import type { ServiceProps } from "./components/types";
import "./App.css";

import { PipelineTriggersProps } from "./pages/PipelineSetup";

import { useNavigate } from "react-router-dom";
import MoreVertIcon from "@mui/icons-material/MoreVert";
import * as React from "react";

export default function App() {
	let svc: ServiceProps = {
		altText: "youTube",
		imageSrc: "https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg",
	};
	let svc2: ServiceProps = {
		altText: "Spotify",
		imageSrc: "https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg",
	};

	let data: Array<PipelineBoxProps> = [
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: svc,
			service2: svc2,
		},
		{
			title: "Lorem ipsum behm uit's long",
			statusText:
				"Lego Star Wars: The Skywalker Saga is an upcoming Lego-themed action-adventure game developed by Traveller's Tales and published by Warner Bros. Interactive Entertainment. It will be the sixth entry in TT Games' Lego Star Wars series of video games and the successor to Lego Star Wars: The Force",
			service1: svc2,
			service2: svc,
		},
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: svc,
			service2: svc2,
		},
		{
			title: "Lorem ipsum behm uit's long",
			statusText: "Vive la france !",
			service1: svc2,
			service2: svc,
		},
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: svc,
			service2: svc2,
		},
		{
			title: "Lorem ipsum behm uit's long",
			statusText: "Vive la france !",
			service1: svc2,
			service2: svc,
		},
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: svc,
			service2: svc2,
		},
		{
			title: "Lorem ipsum behm uit's long",
			statusText: "Vive la france !",
			service1: svc2,
			service2: svc,
		},
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: svc,
			service2: svc2,
		},
	];

	let actions: Array<GenericButtonProps> = [
		{
			title: "Une vidéo à été rg erg ergr rgrg  publiée",
			service: svc,
			trailingIcon: <MoreVertIcon />,
		},
		{
			title: "Riz aux oignons",
			service: svc2,
			trailingIcon: <MoreVertIcon />,
		},
	];

	const navigate = useNavigate();

	const pushToLogin = () => {
		navigate("/auth");
	};

	return (
		<div className="App">
			<header className="App-header">
				<div id="container">
					<Box
						sx={{
							display: "flex",
							flexDirection: "column",
							alignItems: "center",
						}}>
						<Box component="img" alt="Aeris Logo" src={require("./assets/logo-white.png")} />
						<br />
						<Typography variant="h4" style={{ textAlign: "center", maxWidth: "75%" }}>
							Professional, personnal action-reaction manager
						</Typography>
						<br />
						<Button
							id="toConnect"
							variant="contained"
							color="secondary"
							className="EndStartupBtn"
							onClick={pushToLogin}>
							Connect to Aeris
						</Button>
					</Box>
				</div>
			</header>
		</div>
	);
}
