import PipelineBoxesLayout from "../components/Pipelines/PipelineBoxesLayout";
import type { PipelineBoxProps } from "../components/Pipelines/PipelineBox";
import PipelineModal from "../components/Pipelines/PipelineModal";
import { GenericButtonProps } from "../components/GenericButton";
import type { ImageProps } from "../components/types";
import PipelineEditPage from "./PipelineEdit/PipelineEditPage";
import Box from "@mui/material/Box";
import Fab from "@mui/material/Fab";
import AddIcon from "@mui/icons-material/Add";

import { makeStyles } from "@material-ui/core/styles";
import { MoreVert } from "@mui/icons-material";
import { useState } from "react";
import { AppPipelineType, ActionTypeEnum, ReactionTypeEnum, AppActionType, AppReactionType } from "../utils/types";
import ServiceSetupModal from "./ServiceSetup";
import { AppServices, ServiceActions, AppServicesLogos, AppListActions, AppListReactions, AppListPipelines } from "../utils/globals";

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
	ServiceSetup
}

export default function HomePage() {
	const classes = useStyles();
	const [modalMode, setModalMode] = useState<ModalSelection>(ModalSelection.ServiceSetup);
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
						status: 'mdr'
					}
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
		{
			title: "Vous êtes débiles bande de trou du cul",
			statusText: "Last: 2d ago",
			service1: AppServicesLogos["twitter"],
			service2: AppServicesLogos["twitter"],
		},
		{
			title: "Lorem ipsum behm uit's long",
			statusText: "Vive la france !",
			service1: AppServicesLogos["twitter"],
			service2: AppServicesLogos["twitter"],
		},
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: AppServicesLogos["twitter"],
			service2: AppServicesLogos["twitter"],
		},
		{
			title: "Lorem ipsum behm uit's long",
			statusText: "Vive la france !",
			service1: AppServicesLogos["twitter"],
			service2: AppServicesLogos["twitter"],
		},
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: AppServicesLogos["twitter"],
			service2: AppServicesLogos["twitter"],
		},
		{
			title: "Lorem ipsum behm uit's long",
			statusText: "Vive la france !",
			service1: AppServicesLogos["twitter"],
			service2: AppServicesLogos["twitter"],
		},
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: AppServicesLogos["twitter"],
			service2: AppServicesLogos["twitter"],
		},
	];

	let sAcopy = AppListActions;

	sAcopy.map((el) => {
		return el["onClickCallback"] = () => {
			setModalMode(ModalSelection.ArgumentSelector);
		}
	})

	/*

	<PipelineModal isOpen={modalMode === ModalSelection.ActionSelector} handleClose={() => setModalMode(ModalSelection.PipelineEdit)}>
				<PipelineSetupModal name="oui oui" services={AppServices} elements={sAcopy} />
			</PipelineModal>
			<PipelineModal isOpen={modalMode === ModalSelection.ServiceSetup } handleClose={() => {}}>
				<ServiceSetupModal services={AppServices} />
			</PipelineModal>
			<PipelineModal isOpen={modalMode === ModalSelection.ArgumentSelector} handleClose={() => setModalMode(ModalSelection.ActionSelector)}>
				<PipelineNameSetup title="j'aime les frites" actions={ServiceActions["youtube"]} />
			</PipelineModal>

	*/

	return (
		<div className={classes.divHomePage}>
			<PipelineBoxesLayout data={data} />

			<PipelineModal isOpen={modalMode === ModalSelection.PipelineEdit} handleClose={() => setModalMode(ModalSelection.None)}>
				<PipelineEditPage pipelineData={pipelineData} setPipelineData={setPipelineData} />
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
