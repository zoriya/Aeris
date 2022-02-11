import React from 'react';
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
import { List, ListItem, ListItemAvatar, ListItemText, Avatar, IconButton, Grid, Typography, Box } from '@mui/material'
import FolderIcon from '@mui/icons-material/Folder';
import DeleteIcon from '@mui/icons-material/Delete';
import PipelineActionList from './components/PipelineActionList';

function App() {
 
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
  /*
  <PipelineBoxesLayout data={data} />
          <PipelineModal height="500" width="500"> 
            <PipelineBoxesLayout data={data} />
          </PipelineModal>
  */

  return (
    <div className="App">
      <header className="App-header">
        <div id='container'>
          <Box  sx={{ display: 'flex', flexDirection: 'row'}} >
            <PipelineActionList title="Actions" actions={actions} />
            <PipelineActionList title="Réactions" actions={actions} />
          </Box>
        </div>
      </header>
    </div>
  );
}

export default App;
