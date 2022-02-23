import { useState } from "react";
import { AppPipelineType } from "../../utils/types";
import { Box, Switch, FormControl, Grid, Typography, FormGroup, FormControlLabel } from "@mui/material"
import GenericButton, { GenericButtonProps } from "../../components/GenericButton";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import AddBoxIcon from "@mui/icons-material/AddBox";
import DeleteIcon from "@mui/icons-material/Delete";
import LoadingButton from "@mui/lab/LoadingButton";
import { PipelineEditMode } from "./PipelineEditPage"

interface PipelineEditPipelineProps {
	pipelineData: AppPipelineType,
	setPipelineData: any,
	setEditMode: any,
	setEditReactionIndex: any
}

export default function PipelineEditPipeline( {pipelineData, setPipelineData, setEditMode, setEditReactionIndex} : PipelineEditPipelineProps) {
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
							trailingIcon={<AddBoxIcon/>} />
					</Grid>
				</Grid>

				<ArrowForwardIcon sx={{ height: 38, width: 38 }} />

				<Grid container direction="column" spacing={2} justifyContent="flex-start" alignItems="flex-start">
					{pipelineData.reactions.map((el, index) => (
						<Grid item sm={10} md={10} lg={5} xl={4} key={index}>
							<GenericButton service={el.service.logo} title={el.type} trailingIcon={<AddBoxIcon/>} />
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
		</div>
	);
}