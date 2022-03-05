import { useNavigate, useSearchParams } from "react-router-dom";
import {getCookie, sendServiceAuthToken, setCookie, signInService} from "../../utils/utils";
import React, {useEffect, useState} from "react";
import Box from "@mui/material/Box";
import Card from "@material-ui/core/Card";
import CardContent from "@material-ui/core/CardContent";
import TextField from "@material-ui/core/TextField";
import {Divider, InputAdornment, Typography} from "@mui/material";
import {AccountCircle, Lock} from "@mui/icons-material";
import CardActions from "@material-ui/core/CardActions";
import Button from "@material-ui/core/Button";
import {makeStyles, Theme} from "@material-ui/core/styles";
import aerisTheme from "../../Aeris.theme";
import {t} from "i18next";
import {API_ROUTE} from "../../utils/globals";

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

interface ServiceSignUpProps {
    service: string
    endpoint: string
    redirect_uri: string
    navigate_to: string
}

type SignUpFormData = {
    username: string;
    password: string;
    confirmedPassword: string;
    isButtonDisabled: boolean;
    helperText: string;
    isError: boolean;
}

const requestSignUpWithService = async (username: string, password: string, service: string, authToken: string): Promise<boolean> => {
    const response = await fetch(`${API_ROUTE}/auth/${service}/signup?code=${authToken}`, {
        method: 'POST',
        headers: {
            Accept: "application/json",
            "Content-Type": "application/json"
        },
        body: JSON.stringify({ username: username, password: password })
    });

    if (!response.ok) return false;
    let json = await response.json();
    setCookie("aeris_jwt", json["jwt"], 365);
    return true;
}

export default function ServiceSignUp({ service, endpoint, redirect_uri, navigate_to }: ServiceSignUpProps) {
    const [searchParams] = useSearchParams();
    const navigate = useNavigate();
    const classes = useStyles();

    const [signUpData, setSignUpData] = useState<SignUpFormData>({
        username: "",
        password: "",
        confirmedPassword: "",
        isButtonDisabled: true,
        helperText: "",
        isError: false
    });

    const authCode = searchParams.get("code") as string;

    useEffect(() => {
        setSignUpData((prevState) => {
            return {
                ...prevState,
                isButtonDisabled: !(
                    signUpData.username.trim() &&
                    signUpData.password.trim() &&
                    (signUpData.confirmedPassword.trim() == signUpData.password.trim())
                ),
            };
        });
    }, [signUpData.username, signUpData.password, signUpData.confirmedPassword]);


    const handleSignUp = async () => {
        if (await requestSignUpWithService(signUpData.username, signUpData.password, service, authCode)) {
            setSignUpData((prevState) => {
                return { ...prevState, isError: false, helperText: t('loginSuccess') };
            });
            window.location.href = "/pipelines";
        } else {
            setSignUpData((prevState) => {
                return { ...prevState, isError: true, helperText: t('usernameOrPasswordIncorrect') };
            });
        }
    };

    const handleKeyPress = (event: React.KeyboardEvent) => {
        if (event.key === "Enter") {
            signUpData.isButtonDisabled || handleSignUp();
        }
    };

    const handleUsernameChange: React.ChangeEventHandler<HTMLInputElement> = (event) => {
        setSignUpData((prevState) => {
            return { ...prevState, username: event.target.value };
        });
    };

    const handlePasswordChange: React.ChangeEventHandler<HTMLInputElement> = (event) => {
        setSignUpData((prevState) => {
            return { ...prevState, password: event.target.value };
        });
    };

    const handleConfirmedPasswordChange: React.ChangeEventHandler<HTMLInputElement> = (event) => {
        setSignUpData((prevState) => {
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
                                error={signUpData.isError}
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
                                error={signUpData.isError}
                                required
                                type="password"
                                label={t("password") as string}
                                placeholder={t("password") as string}
                                margin="normal"
                                variant="outlined"
                                size="small"
                                helperText={signUpData.helperText}
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
                            <TextField
                                className="inputRounded"
                                error={signUpData.isError}
                                required
                                type="password"
                                label={t('confirm_password') as string}
                                placeholder={t("password") as string}
                                margin="normal"
                                variant="outlined"
                                size="small"
                                helperText={signUpData.helperText}
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
                        </div>
                    </CardContent>
                    <CardActions>
                        <Button
                            variant="contained"
                            size="large"
                            className={classes.loginBtn}
                            onClick={handleSignUp}
                            disabled={signUpData.isButtonDisabled}>
                            {t('signUp')}
                        </Button>
                    </CardActions>
                </Card>
            </form>
        </div>
    );
}