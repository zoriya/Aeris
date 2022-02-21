
import { Google, GitHub, MusicNote, Twitter, YouTube } from "@mui/icons-material";
import { InputLabel, FormHelperText, Avatar, ListItemAvatar } from "@mui/material"
import ListItemIcon from "@mui/material/ListItemIcon";
import ListItemText from "@mui/material/ListItemText";
import Typography from "@mui/material/Typography";
import MenuItem from "@mui/material/MenuItem";
import Select from "@mui/material/Select";
import Grid from "@mui/material/Grid";
import Box from "@mui/material/Box";
import { AppServiceType } from "../utils/globals";
import GenericButton, { GenericButtonProps } from "./../components/GenericButton";

interface PipelineSetupPageProps {
	name: string;
	services: Array<AppServiceType>,
	elements: {[key: string]: Array<GenericButtonProps> };
}

export type { PipelineSetupPageProps };

export default function PipelineSetupModal({ name, services, elements }: PipelineSetupPageProps) {

	//TODO On line 63, need to change number 11 to number of available actions
	return (
		<div>
			<Box
				sx={{
					width: "100%",
					display: "flex",
					flexDirection: "row",
					alignItems: "center",
				}}>
				<Typography variant="h4" noWrap align="left">
					Setup Action: {name}
				</Typography>
				
			</Box>
			<Box sx={{ float:"right" }} >
				<InputLabel id="pipeline-setup-select-label">Service</InputLabel>
				<Select
					labelId="pipeline-setup-select-label"
					defaultValue={"youtube"}
					label="Services" >
					{services.map((item) => (
						<MenuItem value={item.uid}>
							<Box sx={{ display:"flex", alignItems:"center", justifyContent:"space-between" }}>
								<img
									loading="lazy"
									width="20"
									src={item.logo.imageSrc}
									alt={item.logo.altText}
								/>
								{item.label}
							</Box>
						</MenuItem>
					))}
				</Select>
				<FormHelperText>11 actions disponibles</FormHelperText>
			</Box>
			
			<Grid container direction="row" justifyContent="space-around"  alignItems="flex-start">
				{ Object.entries(elements).map((el) => {

					if (el[0] !== "youtube")
						return null;
					return (
						<Grid item key={0}>
							<GenericButton {...el[1][0]} />
						</Grid>
						)
					}
					)
				}
			</Grid>
		</div>
	);
}
