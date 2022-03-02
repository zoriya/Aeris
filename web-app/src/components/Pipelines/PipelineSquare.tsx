import {
	Card,
	CardHeader,
	CardContent,
	CardActionArea,
	Typography,
	CardMedia,
	Avatar,
	Stack,
	Box,
} from "@mui/material";
import { AppPipelineType } from "../../utils/types";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import TimerIcon from "@mui/icons-material/Timer";
import NumbersIcon from "@mui/icons-material/Numbers";
import CableIcon from "@mui/icons-material/Cable";

export interface PipelineSquareProps {
	pipelineData: AppPipelineType;
	onClick?: React.MouseEventHandler<HTMLButtonElement>;
}

export const PipelineSquare = ({ pipelineData, onClick }: PipelineSquareProps) => {
	return (
		<Card sx={{ width: "300px", height: "300px", borderRadius: "15px" }}>
			<CardActionArea onClick={onClick} sx={{ padding: "10px", width: "100%", height: "100%" }}>
				<div
					style={{
						width: "100%",
						height: "100%",
						display: "grid",
						gridTemplateColumns: "45% 10% 45%",
						gridTemplateRows: "30% 40% 20% 10%",
						gridTemplateAreas: `
                            'ActionLogoDisplay  Arrow            ReactionsLogoDisplay'
                            'PipelineTitle      PipelineTitle    PipelineTitle'
                            'PipelineStatus     PipelineStatus   PipelineStatus'
                            'PipelineInfo       PipelineInfo     PipelineInfo'
                        `,
						placeItems: "center center",
					}}>
					<CardMedia
						component="img"
						sx={{ ridArea: "ActionLogoDisplay", width: "70%" }}
						image={pipelineData.action.service.logo.imageSrc}
						alt={pipelineData.action.service.logo.altText}
					/>
					<ArrowForwardIcon sx={{ gridArea: "Arrow", height: 58, width: 58 }} />

					<CardMedia
						component="img"
						sx={{ ridArea: "ReactionsLogoDisplay", width: "70%" }}
						image={pipelineData.reactions[0].service.logo.imageSrc}
						alt={pipelineData.reactions[0].service.logo.altText}
					/>
					<div style={{ gridArea: "PipelineTitle", width: "100%", alignSelf: "start", justifySelf: "start" }}>
						<Typography sx={{ wordWrap: "break-word" }} align="left" variant="h3">
							{pipelineData.name.length < 19 ? pipelineData.name : pipelineData.name.substring(0, 18) + "..."}
						</Typography>
					</div>
					<div style={{ gridArea: "PipelineStatus", alignSelf: "start", justifySelf: "start" }}>
						<Typography align="left" variant="body1">
							{pipelineData.data.status}
						</Typography>
					</div>
					<div style={{ gridArea: "PipelineInfo", width: "100%", alignSelf: "start", justifySelf: "start" }}>
						<Stack direction={"row"} spacing={1}>
							<Box sx={{ display: "flex", flexFlow: "row", alignItems: "center" }}>
								<CableIcon color="secondary" />
								<Typography align="left" variant="subtitle2">
									{pipelineData.reactions.length}
								</Typography>
							</Box>
							<Box sx={{ display: "flex", flexFlow: "row", alignItems: "center" }}>
								<TimerIcon color="secondary" />
								<Typography align="left" variant="subtitle2">
									{pipelineData.data.lastTrigger.toDateString()}
								</Typography>
							</Box>
							<Box sx={{ display: "flex", flexFlow: "row", alignItems: "center" }}>
								<NumbersIcon color="secondary" />
								<Typography align="left" variant="subtitle2">
									{pipelineData.data.triggerCount}
								</Typography>
							</Box>
						</Stack>
					</div>
				</div>
			</CardActionArea>
		</Card>
	);
};
