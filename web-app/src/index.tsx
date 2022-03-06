import { StrictMode } from "react";
import ReactDOM from "react-dom";
import "./App.css";

import App from "./App";
import GithubAuth from "./components/Authorizations/GithubAuth";
import SpotifyAuth from "./components/Authorizations/SpotifyAuth";
import GoogleAuth from "./components/Authorizations/YoutubeAuth";
import TwitterAuth from "./components/Authorizations/TwitterAuth";
import RedditAuth from "./components/Authorizations/RedditAuth";
import AnilistAuth from "./components/Authorizations/AnilistAuth";
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
							<Route path="/authorization/github" element={<GithubAuth />} />
							<Route path="/authorization/spotify" element={<SpotifyAuth />} />
							<Route path="/authorization/google" element={<GoogleAuth />} />
							<Route path="/authorization/twitter" element={<TwitterAuth />} />
							<Route path="/authorization/reddit" element={<RedditAuth />} />
							<Route path="/authorization/anilist" element={<AnilistAuth />} />
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
