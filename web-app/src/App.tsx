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
        <PipelineCreation />
        <PipelineBox title='Like me' lastExecutionTime={"10 min"} />
      </header>
    </div>
  );
}

export default App;
