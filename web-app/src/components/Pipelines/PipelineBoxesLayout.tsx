import { Group } from "@mui/icons-material";
import { Grid } from "@mui/material";
import PipelineBox, { PipelineBoxProps } from "./PipelineBox";

interface PipelineLayoutProps {
	data: Array<PipelineBoxProps>;
}

export default ({ data }: PipelineLayoutProps) => {
	return (
		<Grid container direction="row" spacing={4} justifyContent="space-around" alignItems="flex-start">
			{data.map((el, index) => (
				<Grid item sm={10} md={10} lg={5} xl={4} key={index}>
					<PipelineBox {...el} />
				</Grid>
			))}
		</Grid>
	);
};
