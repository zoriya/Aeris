import { AppPipelineType, AppAREAType, ParamsType } from "../../utils/types";
import { Grid, TextField, Typography, Stack, Alert } from "@mui/material";
import LoadingButton from "@mui/lab/LoadingButton";
import { Info, Save } from "@mui/icons-material";
import { useState } from "react";
import { deepCopy } from "../../utils/utils";
import i18next from "i18next";

import { useTranslation } from "react-i18next";
import "../../i18n/config";

interface PipelineEditParamsProps {
	pipelineData: AppPipelineType;
	AREA: AppAREAType;
	isAction: boolean;
	setParams: (area: AppAREAType) => any;
	handleQuit: () => any;
}

export default function PipelineEditParams({ pipelineData, isAction, AREA, setParams }: PipelineEditParamsProps) {
	AREA = deepCopy(AREA);
	const [formData, setFormData] = useState<{ [key: string]: ParamsType }>(AREA.params);
	const { t } = useTranslation();
	const languageUid = i18next.resolvedLanguage;

	return (
		<div>
			<Typography variant="h5" align="left">
				'{AREA.label[languageUid]}' {t("parameters")}
			</Typography>
			{!isAction && Object.keys(AREA.params).length > 0 && (
				<Alert
					sx={{
						"& .MuiAlert-message": {
							width: "100%",
							fontSize: 12,
						},
					}}
					severity="info">
					{t("pipeline_edit_params_info_text")}
				</Alert>
			)}
			<form
				onSubmit={(e: any) => {
					e.preventDefault();

					let paramsToSave: { [key: string]: ParamsType } = deepCopy(AREA.params);
					for (let prop in paramsToSave) {
						if (!(prop in formData)) return;
						paramsToSave[prop].value = e.target[prop].value;
					}
					setParams({
						...AREA,
						params: paramsToSave,
					});
				}}>
				<Stack>
					{Object.entries(AREA.params).map((param, key) => {
						return (
							<TextField
								key={key}
								sx={{ marginTop: "20px" }}
								label={param[0]}
								name={param[0]}
								value={formData?.[param[0]]?.value ?? ""}
								required
								fullWidth
								helperText={param[1].description[languageUid]}
								onChange={(e: any) => {
									let paramToSave = formData;

									paramToSave[param[0]] = {
										...AREA.params[param[0]],
										value: e.target.value,
									};
									setFormData(deepCopy(paramToSave));
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
						type="submit"
						loading={false}
						loadingPosition="start"
						startIcon={<Save />}
						variant="contained">
						{t("save")}
					</LoadingButton>
				</Grid>
			</form>
		</div>
	);
}
