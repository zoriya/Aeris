import React from 'react';
import logo from './logo.svg';
import './App.css';
import pB from "./components/PipelineBox"
import PipelineBox from './components/PipelineBox';
import PipelineCreation from './components/PipelineCreation';

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <PipelineBox 
          title='My super action' 
          statusText="Last: 2d ago" 
          service1={ {altText: "youTube", imageSrc: "https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg" } }
          service2={ {altText: "Spotify", imageSrc: "https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg" }  }
        />
      </header>
    </div>
  );
}

export default App;
