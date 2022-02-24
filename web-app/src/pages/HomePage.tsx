import PipelineBoxesLayout from "../components/Pipelines/PipelineBoxesLayout";
import ElectricalServicesIcon from '@mui/icons-material/ElectricalServices';
import type { PipelineBoxProps } from "../components/Pipelines/PipelineBox";
import PipelineModal from "../components/Pipelines/PipelineModal";
import { GenericButtonProps } from "../components/GenericButton";
import PipelineEditPage from "./PipelineEdit/PipelineEditPage";
import type { ImageProps } from "../components/types";
import AddIcon from "@mui/icons-material/Add";
import AppBar from "@mui/material/AppBar";
import React, { useEffect } from 'react';
import Box from "@mui/material/Box";
import Fab from "@mui/material/Fab";
import { API_ROUTE } from '../';

import { makeStyles } from "@material-ui/core/styles";
import { MoreVert } from "@mui/icons-material";
import { useState } from "react";
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
import Typography from "@mui/material/Typography";
import IconButton from "@mui/material/IconButton";
import Toolbar from "@mui/material/Toolbar";

const useStyles = makeStyles((theme) => ({
	divHomePage: {
		display: "contents",
	},
}));

enum ModalSelection {
	None,
	PipelineEdit,
	ActionSelector,
	ReactionSelector,
	ArgumentSelector,
	ServiceSetup,
}

const getCookieValue = (name: string): string => {
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++) {
		var c = ca[i].trim();
		if (c.indexOf(nameEQ) == 0)
			return c.substring(nameEQ.length,c.length);
	}
	return "";
}

const getUserName = async (): Promise<string> => {
	const response = await fetch(API_ROUTE + "/auth/me", {
		method: 'GET',
		headers: {
			'Accept': 'application/json',
			'Content-Type': 'application/json',
			'Authorization': 'Bearer ' + getCookieValue('aeris_jwt')
		}
	});

	if (response.ok) {
		let json = await response.json();
		return json["userName"];
	}
	console.error("Can't get username");
	return '';
}

export default function HomePage() {
	const classes = useStyles();
	const [username, setUsername] = useState<string>("");
	const [modalMode, setModalMode] = useState<ModalSelection>(ModalSelection.None);
	const [pipelineData, setPipelineData] = useState<AppPipelineType>(AppListPipelines[0]);

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
					reactions: [AppListReactions[0]],
					data: {
						enabled: true,
						error: false,
						status: "mdr",
					},
				} as AppPipelineType);
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
			<React.Fragment>
				<AppBar position='fixed'>
					<Toolbar variant="dense">
						<Box sx={{ flexGrow: 1 }}/>
						<IconButton onClick={() => { setModalMode(ModalSelection.ServiceSetup) }}>
							<ElectricalServicesIcon/>
						</IconButton>
						<Typography noWrap sx={{ margin: 1 }} variant='h5' align='right'>
							{username}
						</Typography>
					</Toolbar>
				</AppBar>
			</React.Fragment>
			<PipelineBoxesLayout data={data} />

			<PipelineModal
				isOpen={modalMode === ModalSelection.PipelineEdit}
				handleClose={() => setModalMode(ModalSelection.None)}>
				<PipelineEditPage
					pipelineData={pipelineData}
					setPipelineData={setPipelineData}
					services={AppServices}
					actions={AppListActions}
					reactions={AppListReactions}
					handleQuit={() => setModalMode(ModalSelection.None)}
				/>
			</PipelineModal>

			<PipelineModal
				isOpen={modalMode === ModalSelection.ServiceSetup}
				handleClose={() => setModalMode(ModalSelection.None)}>
				<ServiceSetupModal services={AppServices}/>
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
