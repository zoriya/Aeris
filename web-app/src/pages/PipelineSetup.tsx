import {
	Google,
	GitHub,
	MusicNote,
	Twitter,
	YouTube,
} from "@mui/icons-material";

import ListItemIcon from "@mui/material/ListItemIcon";
import ListItemText from "@mui/material/ListItemText";
import Typography from "@mui/material/Typography";
import MenuItem from "@mui/material/MenuItem";
import Select from "@mui/material/Select";
import Grid from "@mui/material/Grid";
import Box from "@mui/material/Box";

interface PipelineTriggersProps {
	name: String;
	triggers: Array<String>;
}

interface PipelineSetupPageProps {
	actionName: String;
	data: Array<PipelineTriggersProps>;
}

export type { PipelineSetupPageProps, PipelineTriggersProps };

export default function PipelineSetupModal({
	actionName,
	data,
}: PipelineSetupPageProps) {
	const servicesItems = [
		{
			name: "Twitter",
			key: "twitter",
			icon: <Twitter />,
		},
		{
			name: "GitHub",
			key: "github",
			icon: <GitHub />,
		},
		{
			name: "Google",
			key: "google",
			icon: <Google />,
		},
		{
			name: "YouTube",
			key: "youtube",
			icon: <YouTube />,
		},
		{
			name: "Spotify",
			key: "spotify",
			icon: <MusicNote />,
		},
	];

	//TODO On line 63, need to change number 11 to number of available actions
	return (
		<div>
			<Box
				sx={{
					display: "flex",
					flexDirection: "row",
					alignItems: "center",
					justifyContent: "space-between",
					marginBottom: "100px",
				}}
			>
				<Typography variant="h2" noWrap align="left">
					Setup Action: {actionName}
				</Typography>
			</Box>
			<Box
				sx={{
					display: "flex",
					flexDirection: "row",
					alignItems: "right",
					justifyContent: "flex-end",
				}}
			>
				<Typography variant="h3" noWrap align="right" sx={{ mr: 4 }}>
					11 available actions for
				</Typography>
				<Select autoWidth variant="standard" defaultValue={"twitter"}>
					{servicesItems.map((item, index) => (
						<MenuItem value={item.key}>
							<img
								loading="lazy"
								width="20"
								src={`https://flagcdn.com/w20/ad.png`}
								srcSet={`https://flagcdn.com/w40/ad.png 2x`}
								alt={`Flag of Andorra`}
							/>
							<ListItemIcon>{item.icon}</ListItemIcon>
							<ListItemText primary={item.name} />
						</MenuItem>
					))}
				</Select>
			</Box>
			<Grid
				container
				direction="row"
				justifyContent="space-around"
				rowSpacing={4}
				alignItems="flex-start"
			>
				{data.map((el, index) => (
					<Grid item sm={10} md={10} lg={5} xl={4} key={0}>
						<Box alignItems="center" justifyContent="center">
							<Select autoWidth defaultValue={0}>
								{" "}
								{el.triggers.map((elem, index) => (
									<MenuItem value={index}>
										<ListItemText primary={elem} />
									</MenuItem>
								))}
							</Select>
						</Box>
					</Grid>
				))}
			</Grid>
		</div>
	);
}
