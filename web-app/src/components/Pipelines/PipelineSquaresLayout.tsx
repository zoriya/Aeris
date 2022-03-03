import { Grid, Typography, Chip, Avatar, Stack } from "@mui/material";
import { PipelineSquare, PipelineSquareProps } from "./PipelineSquare";
import theme from "../../Aeris.theme";
export interface PipelineSquaresLayout {
	data: Array<PipelineSquareProps>;
	style?: any;
}

export const PipelineSquaresLayout = ({ data, style }: PipelineSquaresLayout) => {
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
			<Stack direction={"row"} spacing={1} alignItems="center" style={{ margin: "20px" }}>
				<Typography variant="h3" align="left">
					My Pipelines
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
			<Stack direction={"row"} spacing={1} alignItems="center" style={{ margin: "25px 0 10px 20px" }}>
				<Typography variant="h5" align="left">
					Disabled pipelines
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
		</div>
	);
};
