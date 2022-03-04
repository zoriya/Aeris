import { AppPipelineType, AppAREAType, ParamsType } from "../../utils/types";
import { Grid, TextField, Typography, Stack } from "@mui/material";
import LoadingButton from "@mui/lab/LoadingButton";
import { Info, Save } from "@mui/icons-material";
import { useState } from "react";
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
	const [formData, setFormData] = useState<{ [key: string]: ParamsType }>({});
	const { t } = useTranslation();
	const languageUid = i18next.resolvedLanguage;

	return (
		<div>
			<Typography variant="h5" align="left">
				'{AREA.label[languageUid]}' {t("parameters")}
			</Typography>
			{!isAction && Object.keys(AREA.params).length > 0 && (
				<div
					style={{
						backgroundColor: "#c8c8ff",
						borderRadius: "5px",
						padding: "5px",
					}}>
					<Info sx={{ marginRight: 1 }} fontSize="small" />

					<Typography variant="body2" fontStyle="italic">
						{t("pipeline_edit_params_info_text")}
					</Typography>
				</div>
			)}
			<Stack>
				{Object.entries(AREA.params).map((param, key) => {
					return (
						<TextField
							key={key}
							sx={{ marginTop: "20px" }}
							label={param[0]}
							required
							fullWidth
							helperText={param[1].description[languageUid]}
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
					{t("save")}
				</LoadingButton>
			</Grid>
		</div>
	);
}
