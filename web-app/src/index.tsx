import {StrictMode, useState} from "react";
import ReactDOM from "react-dom";
import "./App.css";

import App from "./App";
import ServiceAuth from "./components/Authorizations/ServiceAuth";
import GithubAuth from "./components/Authorizations/GithubAuth";
import SpotifyAuth from "./components/Authorizations/SpotifyAuth";
import GoogleAuth from "./components/Authorizations/YoutubeAuth";
import TwitterAuth from "./components/Authorizations/TwitterAuth";
import DiscordAuth from "./components/Authorizations/DiscordAuth";
import AnilistAuth from "./components/Authorizations/AnilistAuth";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import AuthComponent from "./pages/Login/LoginPage";
import PipelinePage from "./pages/HomePage";
import { ThemeProvider } from "@mui/material";

import theme from "./Aeris.theme";
import {AppServices} from "./utils/globals";
import {AppServiceType} from "./utils/types";

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
								return (<Route path={`/authorization/${elem.uid}`} element={<ServiceAuth service={elem.uid} navigate_to="/pipelines" redirect_uri={`authorization/${elem.uid}`}/>} />);
							})}
							{/*{possibleServices.map((elem, index) => {*/}
							{/*	return (<Route path={`/signin/${elem.uid}`} element={<ServiceAuth service={elem.uid} navigate_to="/pipelines" redirect_uri={`singin/${elem.uid}`}/>} />);*/}
							{/*})}*/}
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
