import React, { useReducer, useEffect } from 'react';
import { createStyles, makeStyles, Theme } from '@material-ui/core/styles';

import { useNavigate } from "react-router-dom";

import CardContent from '@material-ui/core/CardContent';
import CardActions from '@material-ui/core/CardActions';
import TextField from '@material-ui/core/TextField';
import CardMedia from '@material-ui/core/CardMedia';
import Button from '@material-ui/core/Button';
import Card from '@material-ui/core/Card';

import "./LoginPage.css";

const useStyles = makeStyles((theme: Theme) =>
    createStyles({
        container: {
            display: 'absolute',
            flex: 0.7,
            margin: `${theme.spacing(0)} auto`
        },
        loginBtn: {
            marginTop: theme.spacing(2),
            flexGrow: 1,
            borderRadius: 20
        },
        media: {
            height: 0,
            paddingTop: '100%', // 16:9,
            marginTop: '30'
        },
        card: {
            marginTop: theme.spacing(10)
        }
    })
);

//state type

type State = {
    username: string
    password: string
    isButtonDisabled: boolean
    helperText: string
    isError: boolean
};

const initialState: State = {
    username: '',
    password: '',
    isButtonDisabled: true,
    helperText: '',
    isError: false
};

type Action = { type: 'setUsername', payload: string }
    | { type: 'setPassword', payload: string }
    | { type: 'setIsButtonDisabled', payload: boolean }
    | { type: 'loginSuccess', payload: string }
    | { type: 'loginFailed', payload: string }
    | { type: 'setIsError', payload: boolean };

const reducer = (state: State, action: Action): State => {
    switch (action.type) {
        case 'setUsername':
            return {
                ...state,
                username: action.payload
            };
        case 'setPassword':
            return {
                ...state,
                password: action.payload
            };
        case 'setIsButtonDisabled':
            return {
                ...state,
                isButtonDisabled: action.payload
            };
        case 'loginSuccess':
            return {
                ...state,
                helperText: action.payload,
                isError: false
            };
        case 'loginFailed':
            return {
                ...state,
                helperText: action.payload,
                isError: true
            };
        case 'setIsError':
            return {
                ...state,
                isError: action.payload
            };
    }
}

export default function LoginComponent() {
    const classes = useStyles();
    const navigate = useNavigate();
    const [state, dispatch] = useReducer(reducer, initialState);

    useEffect(() => {
        if (state.username.trim() && state.password.trim()) {
            dispatch({
                type: 'setIsButtonDisabled',
                payload: false
            });
        } else {
            dispatch({
                type: 'setIsButtonDisabled',
                payload: true
            });
        }
    }, [state.username, state.password]);

    const handleLogin = () => {
        if (state.username === 'abc@email.com' && state.password === 'password') {
            dispatch({
                type: 'loginSuccess',
                payload: 'Login Successfully'
            });
            console.log("Login successful!");
        } else {
            dispatch({
                type: 'loginFailed',
                payload: 'Incorrect username or password'
            });
        }
    };

    const handleKeyPress = (event: React.KeyboardEvent) => {
        if (event.keyCode === 13 || event.which === 13) {
            state.isButtonDisabled || handleLogin();
        }
    };

    const handleUsernameChange: React.ChangeEventHandler<HTMLInputElement> =
        (event) => {
            dispatch({
                type: 'setUsername',
                payload: event.target.value
            });
        };

    const handlePasswordChange: React.ChangeEventHandler<HTMLInputElement> =
        (event) => {
            dispatch({
                type: 'setPassword',
                payload: event.target.value
            });
        }

    return (
        <form className={classes.container} noValidate autoComplete="off">
            <Card className={classes.card}>
                <CardMedia
                    className={classes.media}
                    image={require("../../assets/logo-black.png")}
                    title="Aeris Logo"
                />
                <CardContent>
                    <div>
                        <TextField
                            className="inputRounded"
                            error={state.isError}
                            fullWidth
                            id="username"
                            type="email"
                            label="Username"
                            placeholder="Username"
                            margin="normal"
                            variant='outlined'
                            size="small"
                            inputProps={{min: 0, style: { textAlign: 'center' }}}
                            onChange={handleUsernameChange}
                            onKeyPress={handleKeyPress}
                        />
                        <TextField
                            className="inputRounded"
                            error={state.isError}
                            fullWidth
                            id="password"
                            type="password"
                            label="Password"
                            placeholder="Password"
                            margin="normal"
                            variant='outlined'
                            size="small"
                            helperText={state.helperText}
                            inputProps={{min: 0, style: { textAlign: 'center' }}}
                            onChange={handlePasswordChange}
                            onKeyPress={handleKeyPress}
                        />
                    </div>
                </CardContent>
                <CardActions>
                    <Button
                        variant="contained"
                        size="large"
                        color="secondary"
                        className={classes.loginBtn}
                        onClick={handleLogin}
                        disabled={state.isButtonDisabled}>
                        Login
                    </Button>
                </CardActions>
            </Card>
        </form>
    );
}