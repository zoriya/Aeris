import PipelineBoxesLayout from "../components/Pipelines/PipelineBoxesLayout";
import type { PipelineBoxProps } from "../components/Pipelines/PipelineBox";
import PipelineModal from "../components/Pipelines/PipelineModal";
import { GenericButtonProps } from "../components/GenericButton";
import type { ImageProps } from "../components/types";
import PipelineEditPage from "./PipelineEditPage";
import Box from "@mui/material/Box";
import Fab from "@mui/material/Fab";
import AddIcon from "@mui/icons-material/Add";

import { makeStyles } from "@material-ui/core/styles";
import { MoreVert } from "@mui/icons-material";
import { useState } from "react";
import { PipelineEditPageProps } from "./PipelineEditPage";
import PipelineSetupModal from "./PipelineSetup";
import PipelineNameSetup from "../components/Pipelines/PipelineNameSetup";
import ServiceSetupModal from "./ServiceSetup";
import { AppServices, ServiceActions, AppServicesLogos } from "../utils/globals";

const useStyles = makeStyles((theme) => ({
	divHomePage: {
		display: "contents",
	},
}));

export default function HomePage() {
	const classes = useStyles();
	const [isModalOpen, setIsModalOpen] = useState<boolean>(false);
	const [modalData, setModalData] = useState<PipelineEditPageProps>({
		title: "",
		trigger: {
			title: "",
			service: {
				imageSrc: "",
				altText: "",
			},
			trailingIcon: <MoreVert />,
		},
		actions: [
			{
				title: "",
				service: {
					imageSrc: "",
					altText: "",
				},
				trailingIcon: <MoreVert />,
			},
		],
	} as PipelineEditPageProps);

	const data: Array<PipelineBoxProps> = [
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: AppServicesLogos["twitter"],
			service2: AppServicesLogos["twitter"],
			onClickCallback: () => {
				setModalData({
					title: "louis",
					trigger: ServiceActions["youtube"][0],
					trailingIcon: <MoreVert />,
					actions: [ServiceActions["twitter"][0], ServiceActions["spotify"][1]],
				} as PipelineEditPageProps);
				setIsModalOpen(!isModalOpen);
			},
		},
		{
			title: "Lorem ipsum behm uit's long",
			statusText:
				"Lego Star Wars: The Skywalker Saga is an upcoming Lego-themed action-adventure game developed by Traveller's Tales and published by Warner Bros.",
			service1: AppServicesLogos["gmail"],
			service2: AppServicesLogos["twitter"],
			onClickCallback: () => {
				setModalData({
					title: "clickable",
					trigger: {
						title: "Eh oui j'ai été set autrement",
						service: AppServicesLogos["github"],
						trailingIcon: <MoreVert />,
					},
					actions: [
						{
							title: "j'aime l'eau",
							service: AppServicesLogos["twitter"],
							trailingIcon: <AddIcon />,
						},
					],
				} as PipelineEditPageProps);
				setIsModalOpen(!isModalOpen);
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

	return (
		<div className={classes.divHomePage}>
			<PipelineBoxesLayout data={data} />
			<PipelineModal isOpen={isModalOpen} handleClose={() => setIsModalOpen(false)}>
				<PipelineEditPage {...modalData} />
			</PipelineModal>
			<PipelineModal isOpen={true} handleClose={() => {}}>
				<PipelineSetupModal name="oui oui" services={AppServices} elements={ServiceActions} />
			</PipelineModal>
			<PipelineModal isOpen={false} handleClose={() => {}}>
				<ServiceSetupModal />
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
						setModalData({
							title: "Nouvelle pipeline",
							trigger: {
								title: "Ajouter une action",
								service: {
									imageSrc: "https://upload.wikimedia.org/wikipedia/commons/5/55/Question_Mark.svg",
									altText: "Action inconnue",
								},
								trailingIcon: <AddIcon />,
							},
							actions: [],
						} as PipelineEditPageProps);
						setIsModalOpen(!isModalOpen);
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
