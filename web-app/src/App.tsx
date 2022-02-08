import React from 'react';
import logo from './logo.svg';
import './App.css';
import pB from "./components/PipelineBox"
import PipelineBox from './components/PipelineBox';
import type { PipelineBoxProps, ServiceProps } from './components/PipelineBox';
import PipelineCreation from './components/PipelineCreation';
import PipelineBoxesLayout from './components/PipelineBoxesLayout';

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
  ]

  return (
    <div className="App">
      <header className="App-header">
          <PipelineBoxesLayout data={data} />
      </header>
    </div>
  );

  /*
      <PipelineBox 
          title='My super action' 
          statusText="Last: 2d ago" 
          service1={ {altText: "youTube", imageSrc: "https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg" } }
          service2={ {altText: "Spotify", imageSrc: "https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg" }  }
        />
  */
}

export default App;
