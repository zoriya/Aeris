import React from 'react';
import logo from './logo.svg';
import './App.css';
import pB from "./components/PipelineBox"
import PipelineBox from './components/PipelineBox';

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <PipelineBox num={34} />
      </header>
    </div>
  );
}

export default App;
