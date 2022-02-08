import React from 'react';
import logo from './logo.svg';
import './App.css';
import Login from './components/Login/LoginPage';
import pB from "./components/Pipelines/PipelineBox"
import PipelineModal from './components/Pipelines/PipelineModal';
import PipelineBox from './components/Pipelines/PipelineBox';
import PipelineCreation from './components/Pipelines/PipelineCreation';

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <Login />
      </header>
    </div>
  );
}

{/* <PipelineModal/>
<PipelineCreation />
<PipelineBox 
  title='My super action' 
  statusText="Last: 2d ago" 
  service1={ {altText: "youTube", imageSrc: "https://upload.wikimedia.org/wikipedia/commons/0/09/YouTube_full-color_icon_%282017%29.svg" } }
  service2={ {altText: "Spotify", imageSrc: "https://upload.wikimedia.org/wikipedia/commons/8/84/Spotify_icon.svg" }  }
/> */}

export default App;
