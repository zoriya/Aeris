import { Typography, Box, Button, Autocomplete, TextField } from "@mui/material";

import { useTranslation } from "react-i18next";
import "./i18n/config";
import DownloadIcon from "@mui/icons-material/Download";
import { Navigate, useNavigate } from "react-router-dom";
import { getCookie } from "./utils/utils";

export default function App() {
	const { t } = useTranslation();
	const navigate = useNavigate();

	const pushToLogin = () => {
		navigate("/auth");
	};

	if (getCookie("aeris_jwt")) return <Navigate to="/pipelines" replace />;

	const autoCompleteData = [
		{ label: "The Godfather", id: 1 },
		{ label: "Pulp Fiction", id: 2 },
	];

	return (
		<div className="App">
			<header className="App-header">
				<div id="container">
					<Box
						sx={{
							display: "flex",
							flexDirection: "column",
							alignItems: "center",
						}}>
						<Box component="img" alt="Aeris Logo" src={require("./assets/logo-white.png")} />
						<br />
						<Typography variant="h4" style={{ textAlign: "center", maxWidth: "75%" }}>
							{t("aerisDescription")}
						</Typography>
						<br />
						<Button
							id="toConnect"
							variant="contained"
							color="secondary"
							className="EndStartupBtn"
							onClick={pushToLogin}>
							{t("goToApp")}
						</Button>
						<Button variant="text" startIcon={<DownloadIcon />} href="/client.apk" download color="secondary">
							{t("get_mobile_app")}
						</Button>
						<div style={{ backgroundColor: "white", padding: "20px" }}>
							<Autocomplete
								disablePortal
								id="combo-box-demo"
								options={autoCompleteData}
								sx={{ width: 300 }}
								renderInput={(params) => <TextField {...params} label="Movie" />}
							/>
						</div>
					</Box>
				</div>
			</header>
		</div>
	);
}
