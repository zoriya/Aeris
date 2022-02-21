import LoadingButton from "@mui/lab/LoadingButton";
import Typography from "@mui/material/Typography";
import { Grid, TextField } from "@mui/material";
import { Save } from "@mui/icons-material";
import Box from "@mui/material/Box";

import { PipelineActionListProps } from "../PipelineActionList";

//TODO Need to add dynamic number of parameter and a map to create TextFields dynamicly
export default function PipelineNameSetup({ title }: PipelineActionListProps) {
	//TODO at line 23, take a look to make Typography size dynamic in order to avoir textWrap when modal is too tiny
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
					'{title}' Parameters
				</Typography>
			</Box>
			<Box sx={{ display: "flex", flexDirection: "column", alignItems: "center" }}>
				<TextField id="params_1" label="Parameter 1 name" placeholder="Parameter 1 name" variant="standard" />
				<TextField
					sx={{ marginTop: "20px" }}
					id="params_2"
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
					variant="contained">
					Save
				</LoadingButton>
			</Box>
		</div>
	);
}
