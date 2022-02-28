import { Typography, Box, Button } from "@mui/material";

import { useTranslation } from 'react-i18next';
import './i18n/config';

import { useNavigate } from "react-router-dom";

export default function App() {
	const { t } = useTranslation();
	const navigate = useNavigate();

	const pushToLogin = () => {
		navigate("/auth");
	};

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
							{t('aerisDescription')}
						</Typography>
						<br />
						<Button
							id="toConnect"
							variant="contained"
							color="secondary"
							className="EndStartupBtn"
							onClick={pushToLogin}>
							{t('goToApp')}
						</Button>
					</Box>
				</div>
			</header>
		</div>
	);
}
