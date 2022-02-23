import { Box, Typography, FormGroup, FormControlLabel, Switch, Grid } from "@mui/material";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import PipelineActionList from "../components/PipelineActionList";
import AddBoxIcon from "@mui/icons-material/AddBox";
import DeleteIcon from "@mui/icons-material/Delete";
import LoadingButton from "@mui/lab/LoadingButton";
import { styled } from "@mui/material/styles";
import GenericButton from "../components/GenericButton";

import { AppActionType, AppReactionType } from "../utils/types"

import { GenericButtonProps } from "./../components/GenericButton";

export interface PipelineEditPageProps {
	title: string;
	action: AppActionType;
	reactions: Array<AppReactionType>;
}

export default function PipelineEditPage({ title, action, reactions }: PipelineEditPageProps) {
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
					{title}
				</Typography>
				<FormGroup>
					<FormControlLabel control={<Switch defaultChecked />} color="secondary" label="Pipeline activée" />
				</FormGroup>
			</Box>
			<Box sx={{ display: "flex", flexDirection: "row", alignItems: "center" }}>
				<Grid container direction="column" spacing={2} justifyContent="flex-start" alignItems="flex-start">
					<Grid item sm={10} md={10} lg={5} xl={4}>
						<GenericButton service={action.service.logo} title={action.type} trailingIcon={<AddBoxIcon/>} />
					</Grid>
				</Grid>

				<ArrowForwardIcon sx={{ height: 38, width: 38 }} />

				<Grid container direction="column" spacing={2} justifyContent="flex-start" alignItems="flex-start">
					{reactions.map((el, index) => (
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
