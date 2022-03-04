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
	Alert
} from "@mui/material";
import { AppPipelineType, AlertLevel } from "../../utils/types";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import TimerIcon from "@mui/icons-material/Timer";
import LoopIcon from "@mui/icons-material/Loop";
import CableIcon from "@mui/icons-material/Cable";
import { useTranslation } from "react-i18next";
import "../../i18n/config";
import "./PipelineSquare.css";

export interface PipelineSquareProps {
	pipelineData: AppPipelineType;
	onClick?: React.MouseEventHandler<HTMLButtonElement>;
}

export const PipelineSquare = ({ pipelineData, onClick }: PipelineSquareProps) => {
	const { t } = useTranslation();
	const errorMode: boolean = pipelineData.data.alertLevel === AlertLevel.Error;
	const backgroundColor = pipelineData.data.enabled ? (errorMode ? "#ffdddd" : null) : "#464646";
	const textColor = pipelineData.data.enabled ? (errorMode ? "red" : null) : "#adadad";
	return (
		<Card
			sx={{
				width: "300px",
				height: "300px",
				borderRadius: "15px",
				backgroundColor: backgroundColor,
				color: textColor,
			}}>
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
						sx={{ ridArea: "ActionLogoDisplay", width: "60%" }}
						image={pipelineData.action.service.logo.imageSrc}
						alt={pipelineData.action.service.logo.altText}
					/>
					<ArrowForwardIcon sx={{ gridArea: "Arrow", height: 58, width: 58 }} />

					{pipelineData.reactions.length === 1 ? (
						<CardMedia
							component="img"
							sx={{ gridArea: "ReactionsLogoDisplay", width: "60%" }}
							image={pipelineData.reactions[0].service.logo.imageSrc}
							alt={pipelineData.reactions[0].service.logo.altText}
						/>
					) : (
						<div
							className="pipeline-square-square-box"
							style={{
								gridArea: "ReactionsLogoDisplay",
							}}>
							<div className="pipeline-square-square-content">
								{pipelineData.reactions.slice(0, 4).map((reac, idx, arr) => (
									<div
										key={idx}
										style={{
											float: "left",
											display: "flex",
											alignItems: "center",
											justifyItems: "center",
											width: "50%",
											height: arr.length === 2 ? "100%" : "50%",
										}}>
										<CardMedia
											component="img"
											sx={{ width: "90%" }}
											image={reac.service.logo.imageSrc}
											alt={reac.service.logo.altText}
										/>
									</div>
								))}
							</div>
						</div>
					)}

					<div style={{ gridArea: "PipelineTitle", width: "100%", alignSelf: "start", justifySelf: "start" }}>
						<Typography
							sx={{
								overflow: "hidden",
								textOverflow: "ellipsis",
								display: "-webkit-box",
								WebkitLineClamp: "2",
								WebkitBoxOrient: "vertical",
								maxHeight: "3",
							}}
							align="left"
							variant="h3">
							{pipelineData.name}
						</Typography>
					</div>
					<div style={{ gridArea: "PipelineStatus", alignSelf: "start", justifySelf: "start" }}>
						<Typography
							align="left"
							variant="body1"
							style={{
								lineHeight: "1.5em",
								overflow: "hidden",
								textOverflow: "ellipsis",
								display: "-webkit-box",
								WebkitLineClamp: "2",
								WebkitBoxOrient: "vertical",
								maxHeight: "3",
							}}>
							{pipelineData.data.enabled && errorMode && pipelineData.data.status}
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
								<LoopIcon color="secondary" />
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
