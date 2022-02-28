import ElectricalServicesIcon from "@mui/icons-material/ElectricalServices";
import RefreshIcon from '@mui/icons-material/Refresh';
import Typography from "@mui/material/Typography";
import IconButton from "@mui/material/IconButton";
import Divider from "@mui/material/Divider";
import Toolbar from "@mui/material/Toolbar";
import AppBar from "@mui/material/AppBar";
import Box from "@mui/material/Box";
import React from "react";
import {makeStyles, Theme} from "@material-ui/core/styles";
import {Tooltip} from "@mui/material";

interface AppBarProps {
	username: string;
	onClickOnServices?: React.MouseEventHandler<HTMLButtonElement>;
	onClickRefresh?: React.MouseEventHandler<HTMLButtonElement>;
}

export type { AppBarProps };

export default function AerisAppbar({ username, onClickOnServices, onClickRefresh }: AppBarProps) {
	return (
		<React.Fragment>
			<AppBar position="fixed">
				<Toolbar variant="dense">
					<Box sx={{ flexGrow: 1 }} />
					<Tooltip title='Refresh'>
						<IconButton sx={{ color: "#ffffff" }} onClick={onClickRefresh}>
							<RefreshIcon />
						</IconButton>
					</Tooltip>
					<Tooltip title='Services'>
						<IconButton sx={{ color: "#ffffff" }} onClick={onClickOnServices}>
							<ElectricalServicesIcon />
						</IconButton>
					</Tooltip>
					<Divider sx={{ margin: 1, background: "#ffffff" }} orientation="vertical" variant="middle" flexItem />
					<Typography noWrap sx={{ margin: 1 }} variant="h5" align="right">
						{username}
					</Typography>
				</Toolbar>
			</AppBar>
		</React.Fragment>
	);
}
