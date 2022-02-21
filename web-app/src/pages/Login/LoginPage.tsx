import React, { useReducer, useEffect } from "react";
import { makeStyles, Theme } from "@material-ui/core/styles";

import { useNavigate, Link as RouterLink } from "react-router-dom";

import { InputAdornment, Typography } from "@mui/material";
import { AccountCircle, Lock } from "@mui/icons-material";

import CardContent from "@material-ui/core/CardContent";
import CardActions from "@material-ui/core/CardActions";
import TextField from "@material-ui/core/TextField";
import Button from "@material-ui/core/Button";
import Card from "@material-ui/core/Card";
import Box from "@mui/material/Box";

import aerisTheme from "../../Aeris.theme";

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
	},
	switchBtn: {
		backgroundColor: aerisTheme.palette.primary.main,
		color: aerisTheme.palette.primary.contrastText,
		minWidth: 150,
		margin: `${theme.spacing(0)} auto`,
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

//state type

type State = {
	username: string;
	password: string;
	confirmedPassword: string;
	isButtonDisabled: boolean;
	helperText: string;
	isError: boolean;
	authMode: string;
	isConfirmButtonVisible: boolean;
};

const initialState: State = {
	username: "",
	password: "",
	confirmedPassword: "",
	isButtonDisabled: true,
	helperText: "",
	isError: false,
	authMode: "login",
	isConfirmButtonVisible: false,
};

type Action =
	| { type: "setUsername"; payload: string }
	| { type: "setPassword"; payload: string }
	| { type: "setConfirmedPassword"; payload: string }
	| { type: "setIsButtonDisabled"; payload: boolean }
	| { type: "loginSuccess"; payload: string }
	| { type: "loginFailed"; payload: string }
	| { type: "setIsError"; payload: boolean }
	| { type: "setAuthMode"; payload: string }
	| { type: "setIsConfirmButtonVisible"; payload: boolean };

const reducer = (state: State, action: Action): State => {
	switch (action.type) {
		case "setUsername":
			return {
				...state,
				username: action.payload,
			};
		case "setPassword":
			return {
				...state,
				password: action.payload,
			};
		case "setConfirmedPassword":
			return {
				...state,
				confirmedPassword: action.payload,
			};
		case "setIsButtonDisabled":
			return {
				...state,
				isButtonDisabled: action.payload,
			};
		case "loginSuccess":
			return {
				...state,
				helperText: action.payload,
				isError: false,
			};
		case "loginFailed":
			return {
				...state,
				helperText: action.payload,
				isError: true,
			};
		case "setIsError":
			return {
				...state,
				isError: action.payload,
			};
		case "setAuthMode":
			return {
				...state,
				authMode: action.payload,
			};
		case "setIsConfirmButtonVisible":
			return {
				...state,
				isConfirmButtonVisible: action.payload,
			};
	}
};

export default function AuthComponent(this: any) {
	const classes = useStyles();
	const navigate = useNavigate();
	const [state, dispatch] = useReducer(reducer, initialState);

	useEffect(() => {
		if (state.username.trim() && state.password.trim()) {
			if (!state.isConfirmButtonVisible) {
				dispatch({
					type: "setIsButtonDisabled",
					payload: false,
				});
			} else if (
				state.isConfirmButtonVisible &&
				state.password == state.confirmedPassword
			) {
				dispatch({
					type: "setIsButtonDisabled",
					payload: false,
				});
			} else {
				dispatch({
					type: "setIsButtonDisabled",
					payload: true,
				});
			}
		} else {
			dispatch({
				type: "setIsButtonDisabled",
				payload: true,
			});
		}
	}, [state.username, state.password]);

	const handleLogin = () => {
		if (state.username === "abc@email.com" && state.password === "password") {
			dispatch({
				type: "loginSuccess",
				payload: "Login Successfully",
			});
			navigate("/pipelines");
		} else {
			dispatch({
				type: "loginFailed",
				payload: "Incorrect username or password",
			});
		}
	};

	const handleKeyPress = (event: React.KeyboardEvent) => {
		if (event.keyCode === 13 || event.which === 13) {
			state.isButtonDisabled || handleLogin();
		}
	};

	const handleUsernameChange: React.ChangeEventHandler<HTMLInputElement> = (
		event
	) => {
		dispatch({
			type: "setUsername",
			payload: event.target.value,
		});
	};

	const handlePasswordChange: React.ChangeEventHandler<HTMLInputElement> = (
		event
	) => {
		dispatch({
			type: "setPassword",
			payload: event.target.value,
		});
	};

	const handleConfirmedPasswordChange: React.ChangeEventHandler<
		HTMLInputElement
	> = (event) => {
		dispatch({
			type: "setConfirmedPassword",
			payload: event.target.value,
		});
	};

	return (
		<div>
			<Box
				component="img"
				className={classes.media}
				alt="Aeris Logo"
				src={require("../../assets/logo-white.png")}
			/>
			<form className={classes.container} noValidate autoComplete="on">
				<Card className={classes.card}>
					<CardContent>
						<div>
							<TextField
								error={state.isError}
								required
								id="email"
								type="email"
								label="Email"
								placeholder="Email"
								margin="normal"
								variant="outlined"
								size="small"
								onChange={handleUsernameChange}
								InputProps={{
									startAdornment: (
										<InputAdornment position="start">
											<AccountCircle />
										</InputAdornment>
									),
								}}
								onKeyPress={handleKeyPress}
							/>
							<br />
							<TextField
								className="inputRounded"
								error={state.isError}
								required
								id="password"
								type="password"
								label="Password"
								placeholder="Password"
								margin="normal"
								variant="outlined"
								size="small"
								helperText={state.helperText}
								onChange={handlePasswordChange}
								InputProps={{
									startAdornment: (
										<InputAdornment position="start">
											<Lock />
										</InputAdornment>
									),
								}}
								onKeyPress={handleKeyPress}
							/>
							<br />
							{state.isConfirmButtonVisible ? (
								<TextField
									className="inputRounded"
									error={state.isError}
									required
									id="confirm_password"
									type="password"
									label="Confirm password"
									placeholder="Password"
									margin="normal"
									variant="outlined"
									size="small"
									helperText={state.helperText}
									onChange={handleConfirmedPasswordChange}
									InputProps={{
										startAdornment: (
											<InputAdornment position="start">
												<Lock />
											</InputAdornment>
										),
									}}
									onKeyPress={handleKeyPress}
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
							disabled={state.isButtonDisabled}
						>
							{state.authMode === "login" ? "Connection" : "Signup"}
						</Button>
						<Button
							variant="text"
							size="large"
							className={classes.switchBtn}
							onClick={() => {
								dispatch({
									type: "setIsButtonDisabled",
									payload:
										state.authMode === "login" ? true : state.isButtonDisabled,
								});
								dispatch({
									type: "setAuthMode",
									payload: state.authMode === "login" ? "auth" : "login",
								});
								dispatch({
									type: "setIsConfirmButtonVisible",
									payload: !state.isConfirmButtonVisible,
								});
							}}
						>
							{state.authMode === "login" ? "Signup" : "Connection"}
						</Button>
					</CardActions>
					<CardActions
						style={{ justifyContent: "center", alignContent: "center" }}
					>
						<Button
							disableFocusRipple
							disableRipple
							style={{ textTransform: "none" }}
							variant="text"
							color="primary"
							onClick={() => {
								navigate("/forget");
							}}
						>
							Forgot password ?
						</Button>
					</CardActions>
				</Card>
			</form>
		</div>
	);
}
