import { useState } from "react";
import { AppPipelineType } from "../../utils/types";
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
import CancelIcon from "@mui/icons-material/Cancel";
import { PipelineEditMode } from "./PipelineEditPage";
import { getCookie, PipeLineHostToApi } from "../../utils/utils";
import { API_ROUTE } from "../..";

interface PipelineEditPipelineProps {
	pipelineData: AppPipelineType;
	setPipelineData: any;
	setEditMode: any;
	setEditReactionIndex: any;
}

export default function PipelineEditPipeline({
	pipelineData,
	setPipelineData,
	setEditMode,
	setEditReactionIndex,
}: PipelineEditPipelineProps) {
	const requestCreatePipeline = async (pipelineData: AppPipelineType) => {
		const jwt = getCookie("aeris_jwt");

		const rawResponse = await fetch(API_ROUTE + "/workflow/", {
			method: "POST",
			headers: {
				Accept: "application/json",
				"Content-Type": "application/json",
				Authorization: "Bearer " + jwt,
			},
			body: JSON.stringify(PipeLineHostToApi(pipelineData)),
		});
		if (!rawResponse.ok) return false;
		return true;
	};

	return (
		<div>
			<div
				style={{
					display: "grid",
					gridTemplateColumns: "5fr 1fr 5fr",
					gridTemplateRows: "100px 50px auto 100px 100px",
					gridTemplateAreas: `
							'pipelineTitle 	pipelineTitle	enabledStatus'
							'actionTitle 	. 				reactionTitle'
							'actionData 	arrow 			reactionData'
							'. 				. 				buttonAddReaction'
							'buttonDelete	. 				buttonCancelSave'
						`,
					placeItems: "center",
				}}>
				<Typography style={{ gridArea: "pipelineTitle", justifySelf: "left" }} variant="h2" noWrap align="left">
					{pipelineData.name}
				</Typography>

				<FormGroup style={{ gridArea: "enabledStatus", justifySelf:"right" }}>
					<FormControlLabel control={<Switch defaultChecked />} color="secondary" label="Pipeline activée" />
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

				<Grid
					container
					gridArea={"reactionData"}
					direction="column"
					spacing={2}
					justifyContent="flex-start"
					alignItems="flex-start">
					{pipelineData.reactions.map((el, index) => (
						<Grid item sm={10} md={10} lg={5} xl={4} key={index}>
							<GenericButton
								service={el.service.logo}
								title={el.type}
								onClickCallback={() => {
									setEditMode(PipelineEditMode.Reactions);
									setEditReactionIndex(index);
								}}
								trailingIcon={<AddBoxIcon />}
							/>
						</Grid>
					))}
				</Grid>

				<LoadingButton
					sx={{ gridArea: "buttonAddReaction" }}
					color="secondary"
					loading={false}
					loadingPosition="start"
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
					loading={false}>
					Supprimer la pipeline
				</LoadingButton>

				<ButtonGroup sx={{ gridArea: "buttonCancelSave", justifySelf: "right" }}>
					<Button
						color="primary"
						startIcon={<CancelIcon />}
						onClick={() => setEditMode(PipelineEditMode.QuitEdit)}
						variant="contained">
						Annuler
					</Button>
					<Button
						color="secondary"
						startIcon={<SaveIcon />}
						onClick={async () => {
							if (await requestCreatePipeline(pipelineData)) {
								setPipelineData(pipelineData);
							}
						}}
						variant="contained">
						Sauvegarder
					</Button>
				</ButtonGroup>
			</div>
		</div>
	);
}
