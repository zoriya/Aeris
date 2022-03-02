import IconButton from "@mui/material/IconButton";
import CloseIcon from "@material-ui/icons/Close";
import { useTranslation } from "react-i18next";
import { Snackbar, Select, MenuItem, SelectChangeEvent, Typography } from "@mui/material";
import Button from "@material-ui/core/Button";
import React, { useState } from "react";
import "../i18n/config";
import { Tooltip } from "@mui/material";

export default function LanguageSelector() {
	const { t, i18n } = useTranslation();
	const [open, OpenSnackbar] = useState<boolean>(false);
	const [selectedLanguage, setSelectedLanguage] = useState<string>("en");

	const changeLanguage = (language: string) => {
		i18n.changeLanguage(language);
		OpenSnackbar(true);
	};

	const handleClose = (event: React.SyntheticEvent | Event, reason?: string) => {
		if (reason === "clickaway") return;
		OpenSnackbar(false);
	};

	const action = (
		<React.Fragment>
			<IconButton size="small" aria-label="close" color="inherit" onClick={handleClose}>
				<CloseIcon fontSize="small" />
			</IconButton>
		</React.Fragment>
	);

	return (
		<>
			<Select
				size="small"
                variant="standard"
				value={selectedLanguage}
				onChange={(e: SelectChangeEvent) => {
					changeLanguage(e.target.value);
					setSelectedLanguage(e.target.value);
				}}>
				<MenuItem value={"en"}>
					<img loading="lazy" width="20" src="https://flagicons.lipis.dev/flags/4x3/gb.svg" />
				</MenuItem>
				<MenuItem value={"fr"}>
					<img loading="lazy" width="20" src="https://flagicons.lipis.dev/flags/4x3/fr.svg" />
				</MenuItem>
			</Select>
			<Snackbar open={open} autoHideDuration={1000} onClose={handleClose} message={t("switch_language")} />
		</>
	);
}
