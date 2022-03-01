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
import { getCookie, deSerializeServices, deSerialisePipeline, fetchWorkflows } from "../utils/utils";
import { requestCreatePipeline, deletePipeline, getAboutJson } from "../utils/CRUDPipeline";
import { AppAREAType, AppPipelineType } from "../utils/types";
import ServiceSetupModal from "./ServiceSetup";
import { AppServices, ServiceActions, AppServicesLogos, AppListPipelines, NoAREA } from "../utils/globals";
import AerisAppbar from "../components/AppBar";
import MenuItem from "@mui/material/MenuItem";

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

export default function HomePage() {
	const classes = useStyles();
	const [AREAs, setAREAs] = useState<Array<Array<AppAREAType>>>([]);
	const [username, setUsername] = useState<string>("");
	const [modalMode, setModalMode] = useState<ModalSelection>(ModalSelection.None);
	const [pipelineData, setPipelineData] = useState<AppPipelineType>(AppListPipelines[0]);
	const [handleSavePipeline, setHandleSavePipeline] = useState<(pD: AppPipelineType) => any>(
		() => (t: AppPipelineType) => {}
	);
	const [pipelineDeletion, setPipelineDeletion] = useState<boolean>(true);
	const [data, setWorkflowsDatas] = useState<Array<PipelineBoxProps>>([]);

	const homePagePipeLineSave = async (pD: AppPipelineType, creation: boolean) => {
		if (await requestCreatePipeline(pD, creation)) {
			return setModalMode(ModalSelection.None);
		}
	};
	useEffect(() => {
		getAboutJson()
			.then((aboutInfoParam) => {
				setAREAs(deSerializeServices(aboutInfoParam?.server?.services ?? [], AppServices));
			})
			.catch((error) => {
				console.warn(error);
				setAREAs([[], []]);
			});
	}, []);

	useEffect(() => {
		fetchWorkflows()
			.then((workflows) => {
				let pipelineBoxes: Array<PipelineBoxProps> = [];
				for (const workflow of workflows) {
					const newWorkflow = deSerialisePipeline(workflow, AREAs);

					pipelineBoxes.push({
						title: newWorkflow.name,
						statusText: "Refresh API Test Workflow",
						service1: newWorkflow.action.service.logo,
						service2: newWorkflow.reactions[0].service.logo,
						onClickCallback: () => {
							setPipelineData(newWorkflow);
							setHandleSavePipeline(() => (pD: AppPipelineType) => homePagePipeLineSave(pD, false));
							setModalMode(ModalSelection.PipelineEdit);
							setPipelineDeletion(true);
						},
					} as PipelineBoxProps);
					setWorkflowsDatas((oldArray) => [...oldArray, ...pipelineBoxes]);
					console.log("newworkflow", newWorkflow);
				}
				console.log(workflows);
				console.log("processed", pipelineBoxes);
			})
			.catch((error) => {
				console.warn(error);
			});
	}, [AREAs]);

	/*const jsonToPipelineData = (data: any): PipelineBoxProps => {
		let pipelineData = {
			title: data["action"]["name"],
			statusText: "Refresh API Test Workflow",
			service1: services["spotify"],
			service2: services["twitter"], //TODO => Fetch service name in reaction[...][rType] for reactions
			onClickCallback: () => {
				//setPipelineData();
				setHandleSavePipeline(() => (pD: AppPipelineType) => homePagePipeLineSave(pD, false));
				setModalMode(ModalSelection.PipelineEdit);
				setPipelineDeletion(true);
			},
		} as PipelineBoxProps;

		return pipelineData;
	};
*/
	const refreshWorkflows = () => {
		/*	let workflowArray = fetchWorkflows().then((res) => {
			if (res !== null) {
				for (const workflow of res) {
					let newWorkflow = jsonToPipelineData(workflow);
					setWorkflowsDatas((oldArray) => [...oldArray, newWorkflow]);
				}
			}
		});*/
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
