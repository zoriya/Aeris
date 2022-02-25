import PipelineBoxesLayout from "../components/Pipelines/PipelineBoxesLayout";
<<<<<<< HEAD
import ElectricalServicesIcon from "@mui/icons-material/ElectricalServices";
=======
>>>>>>> c8e433aa3794c3e9bd6d7060131d2bb790fc6fd0
import type { PipelineBoxProps } from "../components/Pipelines/PipelineBox";
import PipelineModal from "../components/Pipelines/PipelineModal";
import PipelineEditPage from "./PipelineEdit/PipelineEditPage";
import AddIcon from "@mui/icons-material/Add";
<<<<<<< HEAD
import AppBar from "@mui/material/AppBar";
=======
>>>>>>> c8e433aa3794c3e9bd6d7060131d2bb790fc6fd0
import React, { useEffect } from "react";
import Box from "@mui/material/Box";
import Fab from "@mui/material/Fab";
import { API_ROUTE } from "../";

import { makeStyles } from "@material-ui/core/styles";
import { useState } from "react";
import { getCookie } from "../utils/utils";
import { requestCreatePipeline, deletePipeline } from "../utils/CRUDPipeline";
import { AppPipelineType, ActionTypeEnum, ReactionTypeEnum, AppAREAType } from "../utils/types";
import ServiceSetupModal from "./ServiceSetup";
import {
	AppServices,
	ServiceActions,
	AppServicesLogos,
	AppListActions,
	AppListReactions,
	AppListPipelines,
} from "../utils/globals";
import AerisAppbar from "../components/AppBar";

const useStyles = makeStyles((theme) => ({
	divHomePage: {
		display: "contents",
	},
}));

enum ModalSelection {
	None,
	PipelineEdit,
	ServiceSetup,
}

const getUserName = async (): Promise<string> => {
	const response = await fetch(API_ROUTE + "/auth/me", {
		method: "GET",
		headers: {
			Accept: "application/json",
			"Content-Type": "application/json",
			Authorization: "Bearer " + getCookie("aeris_jwt"),
		},
	});

	if (response.ok) {
		let json = await response.json();
		return json["userName"];
	}
<<<<<<< HEAD
	console.warn("Can't get username");
=======
	console.error("Can't get username");
>>>>>>> c8e433aa3794c3e9bd6d7060131d2bb790fc6fd0
	return "";
};

export default function HomePage() {
	const classes = useStyles();
	const [username, setUsername] = useState<string>("");
	const [modalMode, setModalMode] = useState<ModalSelection>(ModalSelection.None);
	const [pipelineData, setPipelineData] = useState<AppPipelineType>(AppListPipelines[0]);
	const [handleSavePipeline, setHandleSavePipeline] = useState<any>(() => {});

	const homePagePipeLineSave = async (pD: AppPipelineType, creation: boolean) => {
		if (await requestCreatePipeline(pD, creation)) {
			return setModalMode(ModalSelection.None);
		}
	};

	const data: Array<PipelineBoxProps> = [
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: AppServicesLogos["twitter"],
			service2: AppServicesLogos["twitter"],
			onClickCallback: () => {
				setPipelineData({
					name: "louis",
					action: AppListActions[0],
					reactions: AppListReactions,
					data: {
						enabled: true,
						error: false,
						status: "mdr",
					},
				} as AppPipelineType);
				setHandleSavePipeline((pD: AppPipelineType) => homePagePipeLineSave(pD, false));
				setModalMode(ModalSelection.PipelineEdit);
			},
		},
		{
			title: "Lorem ipsum behm uit's long",
			statusText:
				"Lego Star Wars: The Skywalker Saga is an upcoming Lego-themed action-adventure game developed by Traveller's Tales and published by Warner Bros.",
			service1: AppServicesLogos["gmail"],
			service2: AppServicesLogos["twitter"],
			onClickCallback: () => {
				setPipelineData(AppListPipelines[0]);
				setHandleSavePipeline((pD: AppPipelineType) => homePagePipeLineSave(pD, false));
				setModalMode(ModalSelection.PipelineEdit);
			},
		},
	];

	useEffect(() => {
		async function fetchUsername() {
			setUsername(await getUserName());
		}
		fetchUsername();
	}, []);

	return (
		<div className={classes.divHomePage}>
<<<<<<< HEAD
			<React.Fragment>
				<AppBar position="fixed">
					<Toolbar variant="dense">
						<Box sx={{ flexGrow: 1 }} />
						<IconButton
							onClick={() => {
								setModalMode(ModalSelection.ServiceSetup);
							}}>
							<ElectricalServicesIcon />
						</IconButton>
						<Typography noWrap sx={{ margin: 1 }} variant="h5" align="right">
							{username}
						</Typography>
					</Toolbar>
				</AppBar>
			</React.Fragment>
=======
			<AerisAppbar
				username={username}
				onClickOnServices={() => {
					setModalMode(ModalSelection.ServiceSetup);
				}}
			/>
>>>>>>> c8e433aa3794c3e9bd6d7060131d2bb790fc6fd0
			<PipelineBoxesLayout data={data} />

			<PipelineModal
				isOpen={modalMode === ModalSelection.PipelineEdit}
				handleClose={() => setModalMode(ModalSelection.None)}>
				<PipelineEditPage
					pipelineData={pipelineData}
					handleSave={handleSavePipeline}
					services={AppServices}
					actions={AppListActions}
					reactions={AppListReactions}
					handleDelete={(pD: AppPipelineType) => deletePipeline(pD)}
					handleQuit={() => setModalMode(ModalSelection.None)}
				/>
			</PipelineModal>

			<PipelineModal
				isOpen={modalMode === ModalSelection.ServiceSetup}
				handleClose={() => setModalMode(ModalSelection.None)}>
				<ServiceSetupModal services={AppServices} />
			</PipelineModal>

			<Box
				sx={{
					"& > :not(style)": { m: 1 },
					position: "fixed",
					bottom: "5px",
					right: "5px",
				}}>
				<Fab
					onClick={() => {
						setPipelineData(AppListPipelines[1]);
						setHandleSavePipeline((pD: AppPipelineType) => homePagePipeLineSave(pD, true));
						setModalMode(ModalSelection.PipelineEdit);
					}}
					size="medium"
					color="secondary"
					aria-label="add">
					<AddIcon />
				</Fab>
			</Box>
		</div>
	);
}
