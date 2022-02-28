import ElectricalServicesIcon from "@mui/icons-material/ElectricalServices";
import Typography from "@mui/material/Typography";
import IconButton from "@mui/material/IconButton";
import Cached from "@mui/icons-material/Cached";
import Toolbar from "@mui/material/Toolbar";
import AppBar from "@mui/material/AppBar";
import Box from "@mui/material/Box";
import React from "react";

interface AppBarProps {
	username: string;
	onClickOnServices?: React.MouseEventHandler<HTMLButtonElement>;
}

export type { AppBarProps };

export default function AerisAppbar({ username, onClickOnServices }: AppBarProps) {
	return (
		<React.Fragment>
			<AppBar position="fixed">
				<Toolbar variant="dense">
					<Box sx={{ flexGrow: 1 }} />
					<IconButton onClick={onClickOnServices}>
						<ElectricalServicesIcon />
					</IconButton>
					<Typography noWrap sx={{ margin: 1 }} variant="h5" align="right">
						{username}
					</Typography>
				</Toolbar>
			</AppBar>
		</React.Fragment>
	);
}
