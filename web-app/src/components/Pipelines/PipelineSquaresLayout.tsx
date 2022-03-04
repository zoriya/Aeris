import { Grid, Typography, Chip, Avatar, Stack } from "@mui/material";
import { PipelineSquare, PipelineSquareProps } from "./PipelineSquare";
import theme from "../../Aeris.theme";
import { useTranslation } from "react-i18next";
import "../../i18n/config";
export interface PipelineSquaresLayout {
	data: Array<PipelineSquareProps>;
	style?: any;
}

export const PipelineSquaresLayout = ({ data, style }: PipelineSquaresLayout) => {
	const { t } = useTranslation();
	const enabledPipelines = data.filter((pd) => pd.pipelineData.data.enabled);
	const disabledPipelines = data.filter((pd) => !pd.pipelineData.data.enabled);

	return (
		<div
			style={{
				...style,
				overflow: "auto",
				width: "95vw",
				height: "100%",
				padding: "100px 20px 10px 20px",
			}}>
			{enabledPipelines.length > 0 && (
				<>
					<Stack direction={"row"} spacing={1} alignItems="center" style={{ margin: "20px" }}>
						<Typography variant="h3" align="left">
							{t("pipeline_layout_pipeline_section_title")}
						</Typography>
						<Avatar sx={{ float: "left", bgcolor: theme.palette.secondary.main, width: 30, height: 30, fontSize: 20 }}>
							{enabledPipelines.length}
						</Avatar>
					</Stack>
					<Grid container direction="row" spacing={2} justifyContent="flex-start" alignItems="flex-start">
						{enabledPipelines.map((el, index) => (
							<Grid item key={index}>
								<PipelineSquare {...el} />
							</Grid>
						))}
					</Grid>
				</>
			)}
			{disabledPipelines.length > 0 && (
				<>
					<Stack direction={"row"} spacing={1} alignItems="center" style={{ margin: "25px 0 10px 20px" }}>
						<Typography variant="h5" align="left">
							{t("pipeline_layout_disable_pipeline_section_title")}
						</Typography>
						<Avatar sx={{ float: "left", bgcolor: theme.palette.secondary.main, width: 24, height: 24, fontSize: 15 }}>
							{disabledPipelines.length}
						</Avatar>
					</Stack>
					<Grid container direction="row" spacing={2} justifyContent="flex-start" alignItems="flex-start">
						{disabledPipelines.map((el, index) => (
							<Grid item key={index}>
								<PipelineSquare {...el} />
							</Grid>
						))}
					</Grid>
				</>
			)}
		</div>
	);
};
