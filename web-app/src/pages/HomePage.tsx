import PipelineBoxesLayout from "../components/Pipelines/PipelineBoxesLayout";
import type { PipelineBoxProps } from "../components/Pipelines/PipelineBox";
import PipelineModal from "../components/Pipelines/PipelineModal";
import PipelineEditPage from "./PipelineEdit/PipelineEditPage";
import AddIcon from "@mui/icons-material/Add";
import React, { useEffect } from "react";
import Box from "@mui/material/Box";
import Fab from "@mui/material/Fab";
import { API_ROUTE } from "../utils/globals";

import { makeStyles } from "@material-ui/core/styles";
import { useState } from "react";
import { getCookie, deSerializeServices, deSerialisePipeline, fetchWorkflows } from "../utils/utils";
import { requestCreatePipeline, deletePipeline, getAboutJson } from "../utils/CRUDPipeline";
import { AppAREAType, AppPipelineType } from "../utils/types";
import ServiceSetupModal from "./ServiceSetup";
import { AppServices, AppServicesLogos, NoAREA, NewEmptyPipeline } from "../utils/globals";
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
	const [pipelineData, setPipelineData] = useState<AppPipelineType>(NewEmptyPipeline);
	const [handleSavePipeline, setHandleSavePipeline] = useState<(pD: AppPipelineType) => any>(
		() => (t: AppPipelineType) => {}
	);
	const [pipelineDeletion, setPipelineDeletion] = useState<boolean>(true);
	const [pipelinesData, setPipelinesData] = useState<Array<AppPipelineType>>([]);

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
		refreshWorkflows();
	}, [AREAs]);

	const refreshWorkflows = () => {
		fetchWorkflows()
			.then((workflows) => {
				setPipelinesData(workflows.map((workflow: any) => deSerialisePipeline(workflow, AREAs)));
			})
			.catch((error) => {
				console.warn(error);
			});
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
			<PipelineBoxesLayout
				data={pipelinesData.map(
					(pipelineData) =>
						({
							title: pipelineData.name,
							statusText: pipelineData.reactions.length + " reaction(s)",
							service1: pipelineData.action.service.logo,
							service2: pipelineData.reactions[0].service.logo,
							onClickCallback: () => {
								setPipelineData(pipelineData);
								setHandleSavePipeline(() => (pD: AppPipelineType) => homePagePipeLineSave(pD, false));
								setModalMode(ModalSelection.PipelineEdit);
								setPipelineDeletion(true);
							},
						} as PipelineBoxProps)
				)}
			/>

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
						setPipelineData(NewEmptyPipeline);
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
