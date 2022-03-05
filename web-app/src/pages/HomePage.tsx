import { PipelineSquaresLayout } from "../components/Pipelines/PipelineSquaresLayout";
import PipelineModal from "../components/Pipelines/PipelineModal";
import PipelineEditPage from "./PipelineEdit/PipelineEditPage";
import AddIcon from "@mui/icons-material/Add";
import { useEffect } from "react";
import { Box, Fab } from "@mui/material";

import { makeStyles } from "@material-ui/core/styles";
import { useState } from "react";
import {
	getCookie,
	deSerializeServices,
	deSerialisePipeline,
	fetchWorkflows,
	fetchLinkedServices,
	deepCopy,
	doesPipelineUseService,
} from "../utils/utils";
import { requestCreatePipeline, deletePipeline, getAboutJson } from "../utils/CRUDPipeline";
import { AppAREAType, AppPipelineType, AppServiceType, AlertLevel } from "../utils/types";
import ServiceSetupModal from "./ServiceSetup";
import { AppServices, NewEmptyPipeline, API_ROUTE } from "../utils/globals";
import AerisAppbar from "../components/AppBar";
import { Navigate } from "react-router-dom";

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
	if (!getCookie("aeris_jwt")) return <Navigate to="/auth" replace />;

	const classes = useStyles();
	const [username, setUsername] = useState<string>("");
	const [AREAs, setAREAs] = useState<Array<Array<AppAREAType>>>([[], []]);
	const [modalMode, setModalMode] = useState<ModalSelection>(ModalSelection.None);
	const [pipelineData, setPipelineData] = useState<AppPipelineType>(NewEmptyPipeline);
	const [handleSavePipeline, setHandleSavePipeline] = useState<(pD: AppPipelineType) => any>(
		() => (t: AppPipelineType) => {}
	);
	const [servicesData, setServicesData] = useState<Array<AppServiceType>>(AppServices);
	const [pipelineDeletion, setPipelineDeletion] = useState<boolean>(true);
	const [pipelinesData, setPipelinesData] = useState<Array<AppPipelineType>>([]);

	const homePagePipeLineSave = async (pD: AppPipelineType, creation: boolean) => {
		if (await requestCreatePipeline(pD, creation)) {
			if (creation) setPipelinesData([...pipelinesData, pD]);
			else setPipelinesData(pipelinesData.map((iPd) => (iPd.id !== pD.id ? iPd : pD)));
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
		fetchLinkedServices()
			.then((services) => {
				services = services.map((el) => el.toLowerCase());
				setServicesData(
					servicesData.map((servData) => ({
						...servData,
						linked: services.includes(servData.uid) || servData.uid === "utils",
					}))
				);
			})
			.catch((error) => {
				console.warn(error);
			});
	}, []);

	useEffect(() => {
		refreshWorkflows();
	}, [AREAs, servicesData]);

	const refreshWorkflows = () => {
		if (AREAs[0].length === 0 && AREAs[1].length === 0) return;
		fetchWorkflows()
			.then((workflows) => {
				setPipelinesData(
					workflows.map((workflow: any) => {
						let pD = deSerialisePipeline(workflow, AREAs);
						for (const svc of servicesData) {
							if (!svc.linked && doesPipelineUseService(pD, svc) && svc.uid !== "google") {
								pD.data.alertLevel = AlertLevel.Warning;
								pD.data.status = "vous n'avez pas de compte " + svc.label;
							}
						}
						return pD;
					})
				);
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

			<PipelineSquaresLayout
				data={pipelinesData.map((el) => ({
					pipelineData: el,
					onClick: () => {
						setPipelineData(el);
						setHandleSavePipeline(() => (pD: AppPipelineType) => homePagePipeLineSave(pD, false));
						setModalMode(ModalSelection.PipelineEdit);
						setPipelineDeletion(true);
					},
				}))}
			/>

			<PipelineModal
				isOpen={modalMode === ModalSelection.PipelineEdit}
				handleClose={() => setModalMode(ModalSelection.None)}>
				<PipelineEditPage
					disableDeletion={!pipelineDeletion}
					pipelineData={pipelineData}
					handleSave={handleSavePipeline}
					services={servicesData}
					actions={AREAs[0]}
					reactions={AREAs[1]}
					handleDelete={async (pD: AppPipelineType) => {
						if (await deletePipeline(pD)) {
							setPipelinesData(pipelinesData.filter((ipD) => ipD.id !== pD.id));
							setModalMode(ModalSelection.None);
						}
					}}
					handleQuit={() => setModalMode(ModalSelection.None)}
				/>
			</PipelineModal>

			<PipelineModal
				isOpen={modalMode === ModalSelection.ServiceSetup}
				handleClose={() => setModalMode(ModalSelection.None)}>
				<ServiceSetupModal services={servicesData} setServices={setServicesData} />
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
						setPipelineData(deepCopy(NewEmptyPipeline));
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
