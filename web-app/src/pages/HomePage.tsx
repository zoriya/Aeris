import PipelineBoxesLayout from "../components/Pipelines/PipelineBoxesLayout";
import type { PipelineBoxProps } from "../components/Pipelines/PipelineBox";
import PipelineModal from "../components/Pipelines/PipelineModal";
import PipelineEditPage from "./PipelineEdit/PipelineEditPage";
import AddIcon from "@mui/icons-material/Add";
import React, { useEffect } from "react";
import Box from "@mui/material/Box";
import Fab from "@mui/material/Fab";
import { API_ROUTE } from "../";

import { makeStyles } from "@material-ui/core/styles";
import { useState } from "react";
import { getCookie, deSerializeServices } from "../utils/utils";
import { requestCreatePipeline, deletePipeline } from "../utils/CRUDPipeline";
import { AppPipelineType, ActionTypeEnum, ReactionTypeEnum, AppAREAType } from "../utils/types";
import ServiceSetupModal from "./ServiceSetup";
import {
	AppServices,
	ServiceActions,
	AppServicesLogos,
	AppListPipelines,
	NoAREA
} from "../utils/globals";
import AerisAppbar from "../components/AppBar";
import serviceDump from "../utils/discord.json";

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
	console.error("Can't get username");
	return "";
};

const fetchWorkflows = async (): Promise<any> => {
	const response = await fetch(API_ROUTE + 'workflows', {
		method: 'GET',
		headers: {
			Accept: 'application/json',
			"Content-Type": 'application/json',
			Authorization: 'Bearer ' + getCookie('aeris_jwt')
		}
	});

	if (response.ok) {
		let json = await response.json();
		return json;
	}
	console.error("Can't fetch newer workflows");
	return null;
}

export default function HomePage() {
	const classes = useStyles();
	const [username, setUsername] = useState<string>("");
	const [modalMode, setModalMode] = useState<ModalSelection>(ModalSelection.None);
	const [pipelineData, setPipelineData] = useState<AppPipelineType>(AppListPipelines[0]);
	const [handleSavePipeline, setHandleSavePipeline] = useState<(pD: AppPipelineType) => any>(
		() => (t: AppPipelineType) => {}
	);
	const [pipelineDeletion, setPipelineDeletion] = useState<boolean>(true);
	const [data, setWorkflowsDatas] = useState<Array<PipelineBoxProps>>(() => [
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: AppServicesLogos["twitter"],
			service2: AppServicesLogos["twitter"],
			onClickCallback: () => {
				setPipelineData({
					id: 2,
					name: "louis",
					action: NoAREA,
					reactions: [],
					data: {
						enabled: true,
						error: false,
						status: "mdr",
					},
				} as AppPipelineType);
				setHandleSavePipeline(() => (pD: AppPipelineType) => homePagePipeLineSave(pD, false));
				setModalMode(ModalSelection.PipelineEdit);
				setPipelineDeletion(true);
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
				setHandleSavePipeline(() => (pD: AppPipelineType) => homePagePipeLineSave(pD, false));
				setModalMode(ModalSelection.PipelineEdit);
				setPipelineDeletion(true);
			},
		},
	]);

	const AREAs = deSerializeServices(serviceDump, AppServices);
	console.log(AREAs);

	const homePagePipeLineSave = async (pD: AppPipelineType, creation: boolean) => {
		if (await requestCreatePipeline(pD, creation)) {
			return setModalMode(ModalSelection.None);
		}
	};

	const refreshWorkflows = () => {
		let workflowArray = fetchWorkflows().then((res) => {
			if (res !== null) {
				for (const workflow of res) {
					let newWorkflow = {
						title: workflow['action']['name'],
						statusText: 'Last trigger: 2 days',
						service1: AppServicesLogos[workflow['action']['pType'].replace(/([a-z0-9])([A-Z])/g, '$1 $2').toLowerCase().split(' ')[0]],
						service2: AppServicesLogos['twitter'], //TODO => Fetch service name in reaction[...][rType] for reactions
						onClickCallback: () => {
							setPipelineData({
								id: 12,
								name: 'test api refresh',
								action: {
									type: workflow['action']['pType'],
									params: {
										contents: workflow['action']['pParams']['contents']
									},
									returns: {},
									description: 'Something must have been done.',
									service: AppServices[3]
								},
								reactions: [],
								data: {
									enabled: true,
									error: false,
									status: "mdr",
								}
							} as AppPipelineType);
							setHandleSavePipeline(() => (pD: AppPipelineType) => homePagePipeLineSave(pD, false));
							setModalMode(ModalSelection.PipelineEdit);
							setPipelineDeletion(true);
						}
					};
					setWorkflowsDatas((oldArray) => [...oldArray, newWorkflow]);
				}
			}}
		);
	};

	useEffect(() => {
		getUserName().then((username) => {
			setUsername(username);
		});
	}, []);

	return (
		<div className={classes.divHomePage}>
			<AerisAppbar
				username={username}
				onClickOnServices={() => {
					setModalMode(ModalSelection.ServiceSetup);
				}}
				onClickRefresh={refreshWorkflows}
			/>
			<PipelineBoxesLayout data={data} />

			<PipelineModal
				isOpen={modalMode === ModalSelection.PipelineEdit}
				handleClose={() => setModalMode(ModalSelection.None)}>
				<PipelineEditPage
					disableDeletion={!pipelineDeletion}
					pipelineData={pipelineData}
					handleSave={handleSavePipeline}
					services={AppServices}
					actions={AREAs[0]}
					reactions={AREAs[1]}
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
						setPipelineDeletion(false);
						setPipelineData(AppListPipelines[1]);
						setHandleSavePipeline(() => (pD: AppPipelineType) => homePagePipeLineSave(pD, true));
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
