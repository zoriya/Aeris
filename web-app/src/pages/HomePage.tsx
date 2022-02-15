import PipelineBoxesLayout from '../components/Pipelines/PipelineBoxesLayout';
import type { PipelineBoxProps } from '../components/Pipelines/PipelineBox';
import PipelineModal from '../components/Pipelines/PipelineModal';
import { GenericButtonProps }  from '../components/GenericButton';
import type { ServiceProps } from '../components/types';
import PipelineEditPage from './PipelineEditPage';

import { makeStyles } from "@material-ui/core/styles";
import { MoreVert } from "@mui/icons-material";
import { useState } from 'react';
import PipelineNameSetup from "../components/Pipelines/PipelineNameSetup";

const useStyles = makeStyles(theme => ({
    divHomePage: {
      display: "contents",
    },
  }));


export default function HomePage() {
    const classes = useStyles();
    const [isModalOpen, setIsModalOpen] = useState(false);

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
          service2: svc2,
          onClickCallback: () => {
            setIsModalOpen(!isModalOpen);
          } 
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
          service: svc,
          trailingIcon: <MoreVert/>
        },
        {
          title: "Riz aux oignons",
          service: svc2,
          trailingIcon: <MoreVert/>
        },
      ]

      return (
          <div className={classes.divHomePage} >
            <PipelineBoxesLayout data={data} />
            <div> {"test " + isModalOpen} </div>
            <PipelineModal isOpen={isModalOpen} handleClose={ () => setIsModalOpen(false) } >
                <PipelineEditPage
                    title="whaooo" 
                    trigger={{
                        title: "Playlist jouée",
                        service: svc2,
                        trailingIcon: <MoreVert/>
                    }}
                    actions={
                    actions
                    }
                />
            </PipelineModal>
            <PipelineModal isOpen={isModalOpen} handleClose={ () => setIsModalOpen(false) } >
                 <PipelineNameSetup title="Playlist jouée" actions={actions}/>
            </PipelineModal>
        </div>
      );

}