import React from 'react';
import './App.css';

import { useNavigate } from "react-router-dom";

import Box from "@mui/material/Box";
import Button from '@mui/material/Button';
import Typography from "@mui/material/Typography";

export default function App() {

    const navigate = useNavigate();

    const pushToLogin = () => {
        navigate("/login");
    }

  return (
    <div className="App">
      <header className="App-header">
          <Box
              component="img"
              alt="Aeris Logo"
              src={require("./assets/logo-white.png")}
          />
          <br/>
          <Typography
              variant="h4"
              style={{ textAlign: 'center', maxWidth: '75%' }}
          >
              Aeris est le meilleur AREA de Nantes! Prenez le contrôle de vos réseaux sociaux avec Aeris, la nouvelle application de pipeline!
          </Typography>
          <br/>
          <Button
              id="toConnect"
              variant="contained"
              color="secondary"
              className="EndStartupBtn"
              onClick={pushToLogin}
          >
              Commencer à utiliser Aeris
          </Button>
      </header>
    </div>
  );
}
