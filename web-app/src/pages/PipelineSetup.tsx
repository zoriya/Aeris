
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
import { useState } from "react";

interface PipelineSetupPageProps {
	name: string;
	services: Array<AppServiceType>,
	elements: {[key: string]: Array<GenericButtonProps> };
}

export type { PipelineSetupPageProps };

export default function PipelineSetupModal({ name, services, elements }: PipelineSetupPageProps) {
	const [serviceToShow, setServiceToShow] = useState<string>(services[0].uid);

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
					onChange={newValue => setServiceToShow(newValue.target.value)}
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
				<FormHelperText>{elements[serviceToShow]?.length ?? "aucune" } actions disponibles</FormHelperText>
			</Box>
			<Grid container direction="row" justifyContent="flex-start" spacing={2} alignItems="flex-start">
				{ elements[serviceToShow]?.map((el, elIndex) => {
					return (
						<Grid item key={ elIndex }>
							<GenericButton {...el} />
						</Grid>
					);
						
				})
			}
			</Grid>
		</div>
	);
}
