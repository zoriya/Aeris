import LoadingButton from "@mui/lab/LoadingButton";
import { Grid, TextField, Typography, Stack } from "@mui/material";
import { Save } from "@mui/icons-material";
import Box from "@mui/material/Box";
import { AppPipelineType, AppAREAType, ParamsType } from "../../utils/types";
import { useState } from "react";

interface PipelineEditParamsProps {
	pipelineData: AppPipelineType;
	AREA: AppAREAType;
	setParams: (area: AppAREAType) => any;
	handleQuit: () => any;
}

export default function PipelineEditParams({ pipelineData, AREA, setParams }: PipelineEditParamsProps) {
	const [formData, setFormData] = useState<{ [key: string]: ParamsType }>({});

	return (
		<div>
			<Typography variant="h5" align="left">
				'{AREA.type}' Parameters
			</Typography>
			<Stack>
				{Object.entries(AREA.params).map((param, key) => {
					return (
						<TextField
							key={key}
							sx={{ marginTop: "20px" }}
							label={param[0]}
							required
							fullWidth
							helperText={param[1].description}
							defaultValue={param[1].value}
							onChange={(e: any) => {
								let paramToSave = formData;

								paramToSave[param[0]] = {
									...AREA.params[param[0]],
									value: e.target.value,
								};
								setFormData(paramToSave);
							}}
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
							params: formData,
						})
					}
					variant="contained">
					Save
				</LoadingButton>
			</Grid>
		</div>
	);
}
