import logo from './logo.svg';
import './App.css';
import type { PipelineBoxProps } from './components/Pipelines/PipelineBox';
import type { ServiceProps } from './components/types'
import PipelineBoxesLayout from './components/Pipelines/PipelineBoxesLayout';
import pB from "./components/Pipelines/PipelineBox"
import PipelineModal from './components/Pipelines/PipelineModal';
import PipelineBox from './components/Pipelines/PipelineBox';
import GenericButton, { GenericButtonProps }  from './components/GenericButton';
import PipelineCreation from './components/Pipelines/PipelineCreation';
import { List, ListItem, ListItemAvatar, ListItemText, Avatar, IconButton, Grid, Typography, Box, FormGroup, FormControlLabel, Switch, Button } from '@mui/material'
import LoadingButton from '@mui/lab/LoadingButton';
import AddBoxIcon from '@mui/icons-material/AddBox';
import FolderIcon from '@mui/icons-material/Folder';
import DeleteIcon from '@mui/icons-material/Delete';
import PipelineActionList from './components/PipelineActionList';
import { styled } from '@mui/material/styles';
import ArrowForwardIcon from '@mui/icons-material/ArrowForward';
import PipelineEditPage from './pages/PipelineEditPage';


import { useNavigate } from "react-router-dom";
  /*
  <PipelineBoxesLayout data={data} />
          <PipelineModal height="500" width="500"> 
            <PipelineBoxesLayout data={data} />
          </PipelineModal>
  */



export default function App() {

   
  let svc: ServiceProps = {
    altText: "youTube", imageSrc: "https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg" 
  }
  let svc2: ServiceProps = {
    altText: "Spotify", imageSrc: "https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg" 
  }

  let data:Array<PipelineBoxProps> = [
    {
      title: 'My super action',
      statusText: "Last: 2d ago", 
      service1: svc ,
      service2: svc2
    },
    {
      title: 'Lorem ipsum behm uit\'s long',
      statusText: "Lego Star Wars: The Skywalker Saga is an upcoming Lego-themed action-adventure game developed by Traveller's Tales and published by Warner Bros. Interactive Entertainment. It will be the sixth entry in TT Games' Lego Star Wars series of video games and the successor to Lego Star Wars: The Force", 
      service1: svc2 ,
      service2: svc
    },
    {
      title: 'My super action',
      statusText: "Last: 2d ago", 
      service1: svc ,
      service2: svc2
    },
    {
      title: 'Lorem ipsum behm uit\'s long',
      statusText: "Vive la france !", 
      service1: svc2 ,
      service2: svc
    },
    {
      title: 'My super action',
      statusText: "Last: 2d ago", 
      service1: svc ,
      service2: svc2
    },
    {
      title: 'Lorem ipsum behm uit\'s long',
      statusText: "Vive la france !", 
      service1: svc2 ,
      service2: svc
    },
    {
      title: 'My super action',
      statusText: "Last: 2d ago", 
      service1: svc ,
      service2: svc2
    },
    {
      title: 'Lorem ipsum behm uit\'s long',
      statusText: "Vive la france !", 
      service1: svc2 ,
      service2: svc
    },
    {
      title: 'My super action',
      statusText: "Last: 2d ago", 
      service1: svc ,
      service2: svc2
    },
  ]


  let actions: Array<GenericButtonProps> = [
    {
      title: "Une vidéo à été rg erg ergr rgrg  publiée",
      service: svc
    },
    {
      title: "Riz aux oignons",
      service: svc2
    },
  ]


    const navigate = useNavigate();

    const pushToLogin = () => {
        navigate("/auth");
    }

  return (
    <div className="App">
      <header className="App-header">
        <div id='container'>

          <PipelineModal>

          <PipelineEditPage 
            title="whaooo" 
            trigger={ {
              title: "Playlist jouée",
              service: svc2
            } }
            actions={
              actions
            }
          />
          </PipelineModal>
          <Box
              component="img"
              sx={{
                  width: 730.5,
                  height: 510
              }}
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
          </div>
      </header>
    </div>
  );
}
