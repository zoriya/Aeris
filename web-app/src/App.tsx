import { Typography, Box, Button, ButtonGroup } from "@mui/material";

import { Navigate, useNavigate } from "react-router-dom";
import { getCookie } from "./utils/utils";

export default function App() {
	const navigate = useNavigate();

	const pushToLogin = () => {
		navigate("/auth");
	};

	if (getCookie("aeris_jwt"))
		return <Navigate to="/pipelines" replace />;

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
							Professional, personnal action-reaction manager
						</Typography>
						<br />
						<Button variant="contained" color="secondary" onClick={pushToLogin}>
							Connect to Aeris
						</Button>
						<Button variant="text" href="/client.apk" download color="secondary">
							Get the App
						</Button>
					</Box>
				</div>
			</header>
		</div>
	);
}
