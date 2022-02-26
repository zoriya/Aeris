import { useState } from "react";
import { AppAREAType, AppPipelineType } from "../../utils/types";
import {
	Box,
	Switch,
	FormControl,
	Grid,
	Typography,
	FormGroup,
	FormControlLabel,
	Button,
	ButtonGroup,
} from "@mui/material";
import GenericButton, { GenericButtonProps } from "../../components/GenericButton";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import AddBoxIcon from "@mui/icons-material/AddBox";
import DeleteIcon from "@mui/icons-material/Delete";
import SaveIcon from "@mui/icons-material/Save";
import LoadingButton from "@mui/lab/LoadingButton";
import KeyboardArrowDownIcon from "@mui/icons-material/KeyboardArrowDown";
import { PipelineEditMode } from "./PipelineEditPage";
import { getCookie, PipeLineHostToApi } from "../../utils/utils";
import { API_ROUTE } from "../..";
import { ReactionCard } from "../../components/ReactionCard";
import { Keyboard } from "@mui/icons-material";

interface PipelineEditPipelineProps {
	pipelineData: AppPipelineType;
	handleEditAction: (action: AppAREAType) => any;
	handleEditReaction: (reaction: AppAREAType) => any;
	handleDelete: (pD: AppPipelineType) => any;
	handleSave: (pD: AppPipelineType) => any;
	setEditMode: (mode: PipelineEditMode) => any;
	setEditReactionIndex: any;
}

export default function PipelineEditPipeline({
	pipelineData,
	handleDelete,
	handleSave,
	setEditMode,
	setEditReactionIndex,
}: PipelineEditPipelineProps) {
	return (
		<div>
			<div
				style={{
					display: "grid",
					gridTemplateColumns: "25vw 5vw 12vw 13vw",
					gridTemplateRows: "2fr 1fr auto 3fr 1fr",
					gridTemplateAreas: `
							'pipelineTitle  pipelineTitle   pipelineTitle       enabledStatus'
							'actionTitle    .               reactionTitle       reactionTitle'
							'actionData     arrow           reactionData        reactionData'
							'.              .               buttonAddReaction   buttonAddReaction'
							'buttonDelete   .               buttonCancelSave    buttonCancelSave'
						`,
					justifyItems: "center",
					alignItems: "center",
				}}>
				<Typography
					style={{ gridArea: "pipelineTitle", justifySelf: "left" }}
					width="100%"
					variant="h2"
					noWrap
					align="left">
					{pipelineData.name}
				</Typography>

				<FormGroup style={{ gridArea: "enabledStatus" }}>
					<FormControlLabel control={<Switch defaultChecked />} color="secondary" label="Activée" />
				</FormGroup>

				<Typography style={{ gridArea: "actionTitle", justifySelf: "left" }} variant="h5" noWrap align="left">
					Action
				</Typography>

				<Typography style={{ gridArea: "reactionTitle", justifySelf: "left" }} variant="h5" noWrap align="left">
					Réactions
				</Typography>

				<Grid
					container
					gridArea={"actionData"}
					direction="column"
					spacing={2}
					justifyContent="flex-start"
					alignItems="flex-start">
					<Grid item sm={10} md={10} lg={5} xl={4}>
						<GenericButton
							service={pipelineData.action.service.logo}
							title={pipelineData.action.type}
							onClickCallback={() => setEditMode(PipelineEditMode.Action)}
							trailingIcon={<AddBoxIcon />}
						/>
					</Grid>
				</Grid>

				<ArrowForwardIcon sx={{ gridArea: "arrow", height: 38, width: 38 }} />

				<div
					style={{
						width: "100%",
						height: "100%",
						overflow: "auto",
						maxHeight: "30vh",
						gridArea: "reactionData",
						padding: "10px",
					}}>
					<Grid container direction="column" spacing={2} justifyContent="center" alignItems="flex-start">
						{pipelineData.reactions.map((el, index) => (
							<Grid item sm={10} md={10} lg={5} xl={4} key={index}>
								<ReactionCard
									handleEdit={() => {
										setEditMode(PipelineEditMode.Reactions);
										setEditReactionIndex(index);
									}}
									handleDelete={() => pipelineData.reactions.splice(index, 1)}
									reaction={el}
									order={index + 1}
									onClick={() => {}}
								/>
								<GenericButton
									service={el.service.logo}
									title={index + 1 + " - " + el.type}
									onClickCallback={() => {
										setEditMode(PipelineEditMode.Reactions);
										setEditReactionIndex(index);
									}}
									trailingIcon={<DeleteIcon />}
								/>
							</Grid>
						))}
					</Grid>
				</div>

				<LoadingButton
					sx={{ gridArea: "buttonAddReaction" }}
					color="secondary"
					loading={false}
					loadingPosition="start"
					onClick={() => {
						setEditMode(PipelineEditMode.Reactions);
						setEditReactionIndex(pipelineData.reactions.length);
					}}
					startIcon={<AddBoxIcon />}
					variant="contained">
					Ajouter une réaction
				</LoadingButton>

				<LoadingButton
					sx={{ gridArea: "buttonDelete", justifySelf: "left" }}
					variant="contained"
					color="error"
					startIcon={<DeleteIcon />}
					loadingPosition="start"
					onClick={() => handleDelete(pipelineData)}
					loading={false}>
					Supprimer la pipeline
				</LoadingButton>

				<ButtonGroup sx={{ gridArea: "buttonCancelSave", justifySelf: "right" }}>
					<Button
						color="primary"
						startIcon={<SaveIcon />}
						onClick={async () => handleSave(pipelineData)}
						variant="contained">
						Sauvegarder
					</Button>
				</ButtonGroup>
			</div>
		</div>
	);
}
