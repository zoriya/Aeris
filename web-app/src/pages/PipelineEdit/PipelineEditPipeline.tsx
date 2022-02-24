import { useState } from "react";
import { AppPipelineType } from "../../utils/types";
import { Box, Switch, FormControl, Grid, Typography, FormGroup, FormControlLabel, Button } from "@mui/material";
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
			<Box
				sx={{
					display: "flex",
					flexDirection: "row",
					alignItems: "center",
					justifyContent: "space-between",
				}}>
				<Typography variant="h2" noWrap align="left">
					{pipelineData.name}
				</Typography>
				<FormGroup>
					<FormControlLabel control={<Switch defaultChecked />} color="secondary" label="Pipeline activée" />
				</FormGroup>
			</Box>
			<Box sx={{ display: "flex", flexDirection: "row", alignItems: "center" }}>
				<Grid container direction="column" spacing={2} justifyContent="flex-start" alignItems="flex-start">
					<Grid item sm={10} md={10} lg={5} xl={4}>
						<GenericButton
							service={pipelineData.action.service.logo}
							title={pipelineData.action.type}
							onClickCallback={() => setEditMode(PipelineEditMode.Action)}
							trailingIcon={<AddBoxIcon />}
						/>
					</Grid>
				</Grid>

				<ArrowForwardIcon sx={{ height: 38, width: 38 }} />

				<Grid container direction="column" spacing={2} justifyContent="flex-start" alignItems="flex-start">
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
			</Box>

			<LoadingButton
				color="secondary"
				loading={false}
				loadingPosition="start"
				startIcon={<AddBoxIcon />}
				variant="contained">
				Ajouter une réaction
			</LoadingButton>
			<LoadingButton
				variant="contained"
				color="error"
				startIcon={<DeleteIcon />}
				loadingPosition="start"
				loading={false}>
				Supprimer la pipeline
			</LoadingButton>
			<Button
				color="primary"
				startIcon={<CancelIcon />}
				onClick={() => setEditMode(PipelineEditMode.QuitEdit)}
				variant="contained">
				Annuler
			</Button>
			<LoadingButton
				variant="contained"
				color="secondary"
				startIcon={<SaveIcon />}
				loadingPosition="start"
				onClick={async () => {
					if (await requestCreatePipeline(pipelineData)) {
						setPipelineData(pipelineData);
					}
				}}
				loading={false}>
				Sauvegarder la pipeline
			</LoadingButton>
		</div>
	);
}
