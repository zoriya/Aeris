import { StrictMode } from "react";
import ReactDOM from "react-dom";
import "./App.css";

import App from "./App";
import GithubAuth from "./components/Authorizations/GithubAuth";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import AuthComponent from "./pages/Login/LoginPage";
import PipelinePage from "./pages/HomePage";
import { ThemeProvider } from "@mui/material";

import theme from "./Aeris.theme";


export const API_ROUTE = process.env.API_ROUTE ?? "";

/**
 * Creates the routing tree.
 */
function AerisRouter() {
	return (
		<ThemeProvider theme={theme}>
			<div className="App">
				<header className="App-header">
					<BrowserRouter>
						<Routes>
							<Route path="/" element={<App />} />
							<Route path="/auth" element={<AuthComponent />} />
							<Route path="/pipelines" element={<PipelinePage />} />
							<Route path='/authorization/github' element={<GithubAuth authCode='codeExample' />} />
						</Routes>
					</BrowserRouter>
				</header>
			</div>
		</ThemeProvider>
	);
}

ReactDOM.render(
	<StrictMode>
		<AerisRouter />
	</StrictMode>,
	document.getElementById("root")
);
