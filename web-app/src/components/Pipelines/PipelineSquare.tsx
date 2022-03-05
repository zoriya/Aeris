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
	Alert,
	AlertColor,
} from "@mui/material";
import { AppPipelineType, AlertLevel } from "../../utils/types";
import ArrowForwardIcon from "@mui/icons-material/ArrowForward";
import TimerIcon from "@mui/icons-material/Timer";
import LoopIcon from "@mui/icons-material/Loop";
import CableIcon from "@mui/icons-material/Cable";
import { useTranslation } from "react-i18next";
import "./PipelineSquare.css";
import moment from "moment";

export interface PipelineSquareProps {
	pipelineData: AppPipelineType;
	onClick?: React.MouseEventHandler<HTMLButtonElement>;
}

const getAlert = (alertLvl: AlertLevel, text: string, enabled: boolean) => {
	const brightness = "brightness(" + (enabled ? "100" : "80") + "%)";
	return (
		<Alert
			variant={enabled ? "outlined" : "filled"}
			sx={{
				gridArea: "PipelineStatus",
				width: "100%",
				padding: "0px 16px",
				filter: brightness,
				"& .MuiAlert-message": {
					width: "100%",
					textOverflow: "ellipsis",
					overflow: "hidden",
					whiteSpace: "nowrap",
					textAlign: "start",
				},
			}}
			severity={alertLvl as AlertColor}>
			{text}
		</Alert>
	);
};

export const PipelineSquare = ({ pipelineData, onClick }: PipelineSquareProps) => {
	const { t } = useTranslation();
	const pEnabled = pipelineData.data.enabled;
	const errorMode: boolean = pipelineData.data.alertLevel === AlertLevel.Error;
	const backgroundColor = pipelineData.data.enabled ? (errorMode ? "#ffdddd" : null) : "black";
	const textColor = pipelineData.data.enabled ? (errorMode ? "red" : null) : "#adadad";
	moment.locale("fr", {
		months: "janvier_février_mars_avril_mai_juin_juillet_août_septembre_octobre_novembre_décembre".split("_"),
		monthsShort: "janv._févr._mars_avr._mai_juin_juil._août_sept._oct._nov._déc.".split("_"),
		monthsParseExact: true,
		weekdays: "dimanche_lundi_mardi_mercredi_jeudi_vendredi_samedi".split("_"),
		weekdaysShort: "dim._lun._mar._mer._jeu._ven._sam.".split("_"),
		weekdaysMin: "Di_Lu_Ma_Me_Je_Ve_Sa".split("_"),
		weekdaysParseExact: true,
		longDateFormat: {
			LT: "HH:mm",
			LTS: "HH:mm:ss",
			L: "DD/MM/YYYY",
			LL: "D MMMM YYYY",
			LLL: "D MMMM YYYY HH:mm",
			LLLL: "dddd D MMMM YYYY HH:mm",
		},
		calendar: {
			sameDay: "[Aujourd’hui à] LT",
			nextDay: "[Demain à] LT",
			nextWeek: "dddd [à] LT",
			lastDay: "[Hier à] LT",
			lastWeek: "dddd [dernier à] LT",
			sameElse: "L",
		},
		relativeTime: {
			future: "dans %s",
			past: "il y a %s",
			s: "quelques secondes",
			m: "une minute",
			mm: "%d minutes",
			h: "une heure",
			hh: "%d heures",
			d: "un jour",
			dd: "%d jours",
			M: "un mois",
			MM: "%d mois",
			y: "un an",
			yy: "%d ans",
		},
		dayOfMonthOrdinalParse: /\d{1,2}(er|e)/,
		ordinal: function (number) {
			return number + (number === 1 ? "er" : "e");
		},
		meridiemParse: /PD|MD/,
		isPM: function (input) {
			return input.charAt(0) === "M";
		},
		// In case the meridiem units are not separated around 12, then implement
		// this function (look at locale/id.js for an example).
		// meridiemHour : function (hour, meridiem) {
		//     return /* 0-23 hour, given meridiem token and hour 1-12 */ ;
		// },
		meridiem: function (hours, minutes, isLower) {
			return hours < 12 ? "PD" : "MD";
		},
		week: {
			dow: 1, // Monday is the first day of the week.
			doy: 4, // Used to determine first week of the year.
		},
	});
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
								fontSize: "45px",
							}}
							align="left"
							variant="h3">
							{pipelineData.name}
						</Typography>
					</div>
					{pipelineData.data.alertLevel !== AlertLevel.None &&
						getAlert(pipelineData.data.alertLevel, pipelineData.data.status, pEnabled)}

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
									{pipelineData.data.lastTrigger !== undefined
										? moment(pipelineData.data.lastTrigger).fromNow()
										: "not yet triggered"}
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
