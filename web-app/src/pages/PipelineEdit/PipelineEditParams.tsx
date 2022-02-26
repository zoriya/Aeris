import LoadingButton from "@mui/lab/LoadingButton";
import { Grid, TextField, Typography, Stack } from "@mui/material";
import { Save } from "@mui/icons-material";
import Box from "@mui/material/Box";
import { AppPipelineType, AppAREAType } from "../../utils/types";

interface PipelineEditParamsProps {
	pipelineData: AppPipelineType;
	AREA: AppAREAType;
	setParams: (area: AppAREAType) => any;
	handleQuit: () => any;
}

export default function PipelineEditParams({ pipelineData, AREA, setParams }: PipelineEditParamsProps) {
	return (
		<div>
			<Typography variant="h5" align="left">
				'{AREA.type}' Parameters
			</Typography>
			<Stack>
				{Object.entries(AREA.params.contents).map((param, key) => {
					return (
						<TextField
							key={key}
							sx={{ marginTop: "20px" }}
							label={param[0]}
							helperText={param[1].description}
							defaultValue={param[1].value}
							variant="standard"
						/>
					);
				})}
			</Stack>
			<Grid container justifyContent="center">
				<LoadingButton
					sx={{ marginTop: "30px" }}
					color="secondary"
					loading={false}
					loadingPosition="start"
					startIcon={<Save />}
					onClick={() =>
						setParams({
							...AREA,
						})
					}
					variant="contained">
					Save
				</LoadingButton>
			</Grid>
		</div>
	);
}
