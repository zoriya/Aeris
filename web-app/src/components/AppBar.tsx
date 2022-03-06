import ElectricalServicesIcon from "@mui/icons-material/ElectricalServices";
import RefreshIcon from "@mui/icons-material/Refresh";
import Typography from "@mui/material/Typography";
import IconButton from "@mui/material/IconButton";
import Logout from "@mui/icons-material/Logout";
import Divider from "@mui/material/Divider";
import Toolbar from "@mui/material/Toolbar";
import AppBar from "@mui/material/AppBar";
import { Button } from "@mui/material";
import React, { useState } from "react";
import Box from "@mui/material/Box";
import DownloadIcon from "@mui/icons-material/Download";

import { useNavigate } from "react-router-dom";
import { Tooltip } from "@mui/material";

import LanguageSelector from "./LanguageSelector";
import { useTranslation } from "react-i18next";
import "../i18n/config";

interface AppBarProps {
	username: string;
	onClickOnServices?: React.MouseEventHandler<HTMLButtonElement>;
	onClickRefresh?: React.MouseEventHandler<HTMLButtonElement>;
}

export type { AppBarProps };

export default function AerisAppbar({ username, onClickOnServices, onClickRefresh }: AppBarProps) {
	const { t } = useTranslation();
	const navigate = useNavigate();

	return (
		<React.Fragment>
			<AppBar position="fixed">
				<Toolbar variant="dense">
					<img src={require("../assets/Icon-App-76x76@2x.png")} width="40" height="40" />
					<Typography noWrap sx={{ margin: 1 }} variant="h5" align="right">
						Aeris
					</Typography>
					<Box sx={{ flexGrow: 1 }} />
					<Button
						sx={{ marginRight: "10px" }}
						variant="contained"
						startIcon={<DownloadIcon />}
						color="secondary"
						size="small"
						href={process.env.REACT_APP_MOBILE_APK_LOCATION ?? "/client.apk"}
						download>
						{t("get_mobile_app")}
					</Button>
					<LanguageSelector />
					<Tooltip title={t("refresh") as string}>
						<IconButton sx={{ color: "#ffffff" }} onClick={onClickRefresh}>
							<RefreshIcon />
						</IconButton>
					</Tooltip>
					<Tooltip title="Services">
						<IconButton sx={{ color: "#ffffff" }} onClick={onClickOnServices}>
							<ElectricalServicesIcon />
						</IconButton>
					</Tooltip>
					<Tooltip title={t("logout") as string}>
						<IconButton
							sx={{ color: "#ffffff" }}
							onClick={() => {
								document.cookie = "aeris_jwt=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;";
								navigate("/auth");
							}}>
							<Logout />
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
