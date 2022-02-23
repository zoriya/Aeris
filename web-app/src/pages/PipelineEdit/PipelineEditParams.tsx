import LoadingButton from "@mui/lab/LoadingButton";
import Typography from "@mui/material/Typography";
import { Grid, TextField } from "@mui/material";
import { Save } from "@mui/icons-material";
import Box from "@mui/material/Box";
import { AppPipelineType, AppAREAType } from "../../utils/types";

import { PipelineActionListProps } from "../../components/PipelineActionList";

interface PipelineEditParamsProps {
	pipelineData: AppPipelineType,
	AREA: AppAREAType,
	setParams: any,
	handleQuit: any
} 

export default function PipelineEditParams({ pipelineData, AREA, setParams }: PipelineEditParamsProps) {
	return (
		<div>
			<Box
				sx={{
					display: "flex",
					flexDirection: "row",
					alignItems: "center",
					justifyContent: "space-between",
					marginBottom: "35px",
				}}>
				<Typography variant="h4" noWrap align="left" minWidth={300}>
					'{AREA.type}' Parameters
				</Typography>
			</Box>
			<Box sx={{ display: "flex", flexDirection: "column", alignItems: "center" }}>
				<TextField label="Parameter 1 name" placeholder="Parameter 1 name" variant="standard" />
				<TextField
					sx={{ marginTop: "20px" }}
					label="Parameter 2 name"
					placeholder="Parameter 2 name"
					variant="standard"
				/>
				<LoadingButton
					sx={{ marginTop: "30px" }}
					color="secondary"
					loading={false}
					loadingPosition="start"
					startIcon={<Save />}
					onClick={() => setParams(AREA)}
					variant="contained">
					Save
				</LoadingButton>
			</Box>
		</div>
	);
}
