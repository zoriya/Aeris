import * as React from "react";
import ReactDOM from "react-dom";
import "./App.css";

import App from "./App";
import reportWebVitals from "./reportWebVitals";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import AuthComponent from "./pages/Login/LoginPage";
import PipelinePage from "./pages/HomePage";
import { ThemeProvider } from "@mui/material";

import theme from "./Aeris.theme";

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
						</Routes>
					</BrowserRouter>
				</header>
			</div>
		</ThemeProvider>
	);
}

ReactDOM.render(
	<React.StrictMode>
		<AerisRouter />
	</React.StrictMode>,
	document.getElementById("root")
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
