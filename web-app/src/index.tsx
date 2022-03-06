import {StrictMode, useState} from "react";
import ReactDOM from "react-dom";
import "./App.css";

import App from "./App";
import ServiceAuth from "./components/Authorizations/ServiceAuth";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import AuthComponent from "./pages/Login/LoginPage";
import PipelinePage from "./pages/HomePage";
import { ThemeProvider } from "@mui/material";

import theme from "./Aeris.theme";
import {AppServices} from "./utils/globals";
import {AppServiceType} from "./utils/types";
import ServiceSignIn from "./components/Authorizations/ServiceSignIn";
import ServiceSignUp from "./components/Authorizations/ServiceSignUp";

/**
 * Creates the routing tree.
 */
function AerisRouter() {
	const [possibleServices, setServices] = useState<Array<AppServiceType>>(AppServices)

	return (
		<ThemeProvider theme={theme}>
			<div className="App">
				<header className="App-header">
					<BrowserRouter>
						<Routes>
							<Route path="/" element={<App />} />
							<Route path="/auth" element={<AuthComponent />} />
							<Route path="/pipelines" element={<PipelinePage />} />
							{possibleServices.map((elem, index) => {
								return (<Route path={`/authorization/${elem.uid}`} element={<ServiceAuth service={elem.uid} endpoint="" navigate_to="/pipelines" redirect_uri={`authorization/${elem.uid}`}/>} />);
							})}
							{possibleServices.map((elem, index) => {
								return (<Route path={`/signin/${elem.uid}`} element={<ServiceSignIn service={elem.uid} endpoint="/signin" navigate_to="/pipelines" redirect_uri={`singin/${elem.uid}`}/>} />);
							})}
							{possibleServices.map((elem, index) => {
								return (<Route path={`/signup/${elem.uid}`} element={<ServiceSignUp service={elem.uid} endpoint="/signup" navigate_to="/pipelines" redirect_uri={`singup/${elem.uid}`}/>} />);
							})}
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
