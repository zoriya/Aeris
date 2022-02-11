import React from 'react';
import logo from './logo.svg';
import './App.css';
import type { PipelineBoxProps } from './components/Pipelines/PipelineBox';
import type { ServiceProps } from './components/types'
import PipelineBoxesLayout from './components/Pipelines/PipelineBoxesLayout';
import pB from "./components/Pipelines/PipelineBox"
import PipelineModal from './components/Pipelines/PipelineModal';
import PipelineBox from './components/Pipelines/PipelineBox';
import GenericButton from './components/GenericButton';
import PipelineCreation from './components/Pipelines/PipelineCreation';

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
          <GenericButton title='salut' service={svc} />
          <GenericButton title='salut' service={svc2} />
        </div>
      </header>
    </div>
  );
}

export default App;
