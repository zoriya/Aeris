import React, { useState, useEffect } from "react";
import { makeStyles, Theme } from "@material-ui/core/styles";

import { useNavigate, Link as RouterLink } from "react-router-dom";

import { AccountCircle, Cookie, Lock } from "@mui/icons-material";
import {CardMedia, Divider, InputAdornment, Typography} from "@mui/material";

import {API_ROUTE, AppServices} from "../../utils/globals";

import CardContent from "@material-ui/core/CardContent";
import CardActions from "@material-ui/core/CardActions";
import TextField from "@material-ui/core/TextField";
import Button from "@material-ui/core/Button";
import Card from "@material-ui/core/Card";
import Box from "@mui/material/Box";

import aerisTheme from "../../Aeris.theme";
import { setCookie, getCookie } from "../../utils/utils";

import { useTranslation } from "react-i18next";
import '../../i18n/config';
import {AppServiceType} from "../../utils/types";

const useStyles = makeStyles((theme: Theme) => ({
	container: {
		display: "absolute",
		flex: 0.5,
		margin: `${theme.spacing(0)} auto`,
	},
	loginBtn: {
		display: "absolute",
		backgroundColor: aerisTheme.palette.secondary.main,
		color: aerisTheme.palette.primary.contrastText,
		minWidth: 150,
		margin: `${theme.spacing(0)} auto`,
		'&:hover': {
			backgroundColor: aerisTheme.palette.secondary.light
		}
	},
	switchBtn: {
		backgroundColor: aerisTheme.palette.primary.main,
		color: aerisTheme.palette.primary.contrastText,
		minWidth: 150,
		margin: `${theme.spacing(0)} auto`,
		'&:hover': {
			backgroundColor: aerisTheme.palette.primary.light
		}
	},
	media: {
		display: "absolute",
		justifyContent: "center",
		alignItems: "center",
		width: 354.75,
		height: 478.5,
		marginBottom: 5,
	},
	card: {
		display: "absolute",
		margin: `${theme.spacing(0)} auto`,
	},
}));

type AuthCompProps = {
	username: string;
	password: string;
	confirmedPassword: string;
	isButtonDisabled: boolean;
	helperText: string;
	isError: boolean;
	authMode: string;
	isConfirmButtonVisible: boolean;
};

const requestLogin = async (username: string, password: string, signup: boolean): Promise<boolean> => {
	const rawResponse = await fetch(API_ROUTE + "/auth/" + (signup ? "signup" : "login"), {
		method: "POST",
		headers: {
			Accept: "application/json",
			"Content-Type": "application/json",
		},
		body: JSON.stringify({ username: username, password: password }),
	});
	if (!rawResponse.ok) return false;
	if (signup) return requestLogin(username, password, false);
	let json = await rawResponse.json();
	setCookie("aeris_jwt", json["jwt"], 365);
	return true;
};

export default function AuthComponent() {
	const { t } = useTranslation();
	const classes = useStyles();
	const navigate = useNavigate();
	const [servicesData, setServicesData] = useState<Array<AppServiceType>>(AppServices);
	const [authData, setAuthData] = useState<AuthCompProps>({
		username: "",
		password: "",
		confirmedPassword: "",
		isButtonDisabled: true,
		helperText: "",
		isError: false,
		authMode: "login",
		isConfirmButtonVisible: false,
	});

	useEffect(() => {
		setAuthData((prevState) => {
			return {
				...prevState,
				isButtonDisabled: !(
					authData.username.trim() &&
					authData.password.trim() &&
					(!authData.isConfirmButtonVisible ||
						(authData.isConfirmButtonVisible && authData.confirmedPassword.trim() == authData.password.trim()))
				),
			};
		});
	}, [authData.username, authData.password, authData.confirmedPassword]);

	const handleLogin = async () => {
		if (await requestLogin(authData.username, authData.password, authData.authMode === "auth")) {
			setAuthData((prevState) => {
				return { ...prevState, isError: false, helperText: t('loginSuccess') };
			});
			window.location.href = "/pipelines";
		} else {
			setAuthData((prevState) => {
				return { ...prevState, isError: true, helperText: t('usernameOrPasswordIncorrect') };
			});
		}
	};

	const handleKeyPress = (event: React.KeyboardEvent) => {
		if (event.key === "Enter") {
			authData.isButtonDisabled || handleLogin();
		}
	};

	const handleUsernameChange: React.ChangeEventHandler<HTMLInputElement> = (event) => {
		setAuthData((prevState) => {
			return { ...prevState, username: event.target.value };
		});
	};

	const handlePasswordChange: React.ChangeEventHandler<HTMLInputElement> = (event) => {
		setAuthData((prevState) => {
			return { ...prevState, password: event.target.value };
		});
	};

	const handleConfirmedPasswordChange: React.ChangeEventHandler<HTMLInputElement> = (event) => {
		setAuthData((prevState) => {
			return { ...prevState, confirmedPassword: event.target.value };
		});
	};

	return (
		<div>
			<Box component="img" className={classes.media} alt="Aeris Logo" src={require("../../assets/logo-white.png")} />
			<form className={classes.container} noValidate autoComplete="on">
				<Card className={classes.card}>
					<CardContent>
						<div>
							<TextField
								error={authData.isError}
								required
								type="username"
								label={t("username") as string}
								placeholder={t("username") as string}
								margin="normal"
								variant="outlined"
								size="small"
								InputProps={{
									startAdornment: (
										<InputAdornment position="start">
											<AccountCircle />
										</InputAdornment>
									),
								}}
								onKeyPress={handleKeyPress}
								onChange={handleUsernameChange}
							/>
							<br />
							<TextField
								className="inputRounded"
								error={authData.isError}
								required
								type="password"
								label={t("password") as string}
								placeholder={t("password") as string}
								margin="normal"
								variant="outlined"
								size="small"
								helperText={authData.helperText}
								InputProps={{
									startAdornment: (
										<InputAdornment position="start">
											<Lock />
										</InputAdornment>
									),
								}}
								onKeyPress={handleKeyPress}
								onChange={handlePasswordChange}
							/>
							<br />
							{authData.isConfirmButtonVisible ? (
								<TextField
									className="inputRounded"
									error={authData.isError}
									required
									type="password"
									label={t('confirm_password') as string}
									placeholder={t("password") as string}
									margin="normal"
									variant="outlined"
									size="small"
									helperText={authData.helperText}
									InputProps={{
										startAdornment: (
											<InputAdornment position="start">
												<Lock />
											</InputAdornment>
										),
									}}
									onKeyPress={handleKeyPress}
									onChange={handleConfirmedPasswordChange}
								/>
							) : null}
						</div>
					</CardContent>
					<CardActions>
						<Button
							variant="contained"
							size="large"
							className={classes.loginBtn}
							onClick={handleLogin}
							disabled={authData.isButtonDisabled}>
							{authData.authMode === "login" ? t('connectToAeris') : t('signUp')}
						</Button>
						<Button
							onClick={() => {
								setAuthData((prevState) => {
									return {
										...prevState,
										isButtonDisabled: authData.authMode === "login" ? true : authData.isButtonDisabled,
										authMode: authData.authMode === "login" ? "auth" : "login",
										isConfirmButtonVisible: !authData.isConfirmButtonVisible,
									};
								});
							}}
							variant="text"
							size="large"
							className={classes.switchBtn}>
							{authData.authMode === "login" ? t('signUp') : t('connectToAeris')}
						</Button>
					</CardActions>
					<Divider variant="middle" sx={{ m: 1 }}/>
					<Typography variant="body2" sx={{ mb: 1}}>
						Or you can sign up with this services:
					</Typography>
					<CardActions>
						{servicesData.map((elem, index) => {
							if (elem.uid === "utils")
								return (<div/>);
							return (
								<Button
									onClick={() => (window.location.href = elem.urlAuth)}
									variant="text"
									style={{ borderRadius: "20px", width: "20px", height: "20px" }}
								>
									<img loading="lazy" width="20px" height="20px" src={elem.logo.imageSrc} alt={elem.logo.altText} />
								</Button>
							);
						})}
					</CardActions>
				</Card>
			</form>
		</div>
	);
}
