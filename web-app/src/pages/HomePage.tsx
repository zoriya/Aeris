import PipelineBoxesLayout from "../components/Pipelines/PipelineBoxesLayout";
import type { PipelineBoxProps } from "../components/Pipelines/PipelineBox";
import PipelineModal from "../components/Pipelines/PipelineModal";
import { GenericButtonProps } from "../components/GenericButton";
import type { ServiceProps } from "../components/types";
import PipelineEditPage from "./PipelineEditPage";
import Box from "@mui/material/Box";
import Fab from "@mui/material/Fab";
import AddIcon from "@mui/icons-material/Add";

import { makeStyles } from "@material-ui/core/styles";
import { MoreVert } from "@mui/icons-material";
import { useState } from "react";
import { PipelineEditPageProps } from "./PipelineEditPage";
import PipelineSetupModal, { PipelineTriggersProps } from "./PipelineSetup";
import PipelineNameSetup from "../components/Pipelines/PipelineNameSetup";

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

	let svc: ServiceProps = {
		altText: "youTube",
		imageSrc:
			"https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg",
	};
	let svc2: ServiceProps = {
		altText: "Spotify",
		imageSrc:
			"https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg",
	};

	let actions: Array<GenericButtonProps> = [
		{
			title: "Une vidéo à été rg erg ergr rgrg  publiée",
			service: svc,
			trailingIcon: <MoreVert />,
		},
		{
			title: "Riz aux oignons",
			service: svc2,
			trailingIcon: <MoreVert />,
		},
	];

	let data: Array<PipelineBoxProps> = [
		{
			title: "My super action",
			statusText: "Last: 2d ago",
			service1: svc,
			service2: svc2,
			onClickCallback: () => {
				setModalData({
					title: "louis",
					trigger: actions[0],
					trailingIcon: <MoreVert />,
					actions: actions,
				} as PipelineEditPageProps);
				setIsModalOpen(!isModalOpen);
			},
		},
		{
			title: "Lorem ipsum behm uit's long",
			statusText:
				"Lego Star Wars: The Skywalker Saga is an upcoming Lego-themed action-adventure game developed by Traveller's Tales and published by Warner Bros.",
			service1: svc2,
			service2: svc,
			onClickCallback: () => {
				setModalData({
					title: "clickable",
					trigger: {
						title: "Eh oui j'ai été set autrement",
						service: {
							imageSrc:
								"https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg",
							altText: "github logo",
						},
						trailingIcon: <MoreVert />,
					},
					actions: [
						{
							title: "j'aime l'eau",
							service: {
								imageSrc:
									"https://upload.wikimedia.org/wikipedia/sco/9/9f/Twitter_bird_logo_2012.svg",
								altText: "twitter logo",
							},
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

	let triggersAvailable: Array<PipelineTriggersProps> = [
		{
			name: "Test",
			triggers: new Array<String>("Lorem Ipsum", "Lorem Ipsum 2"),
		},
		{
			name: "Test 2",
			triggers: new Array<String>("Lorem Ipsum", "Lorem Ipsum 2"),
		},
		{
			name: "Test 3",
			triggers: new Array<String>("Lorem Ipsum", "Lorem Ipsum 2"),
		},
	];

	return (
		<div className={classes.divHomePage}>
			<PipelineBoxesLayout data={data} />
			<PipelineModal
				isOpen={isModalOpen}
				handleClose={() => setIsModalOpen(false)}
			>
				<PipelineEditPage {...modalData} />
			</PipelineModal>

			<Box
				sx={{
					"& > :not(style)": { m: 1 },
					position: "fixed",
					bottom: "5px",
					right: "5px",
				}}
			>
				<Fab
					size="medium"
					color="secondary"
					aria-label="add"
					onClick={() => {
						setModalData({
							title: "Nouvelle pipeline",
							trigger: {
								title: "Ajouter une action",
								service: {
									imageSrc:
										"https://upload.wikimedia.org/wikipedia/commons/5/55/Question_Mark.svg",
									altText: "Action inconnue",
								},
								trailingIcon: <AddIcon />,
							},
							actions: [],
						} as PipelineEditPageProps);
						setIsModalOpen(!isModalOpen);
					}}
				>
					<AddIcon />
				</Fab>
			</Box>
		</div>
	);
}
