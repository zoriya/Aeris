import { useState } from 'react';
import Modal from '@mui/material/Modal';
import Button from '@mui/material/Button';
import IconButton from '@mui/material/IconButton';
import CloseIcon from "@material-ui/icons/Close";
import Backdrop from "@material-ui/core/Backdrop";
import Fade from "@material-ui/core/Fade";
import { makeStyles } from "@material-ui/core/styles";

import { useTheme } from '@mui/material/styles';

const useStyles = makeStyles(theme => ({
    modal: {
      display: "flex",
      alignItems: "center",
      justifyContent: "center"
    },
    paper: {
      backgroundColor: theme.palette.background.paper,
      borderRadius: 14,
      border: "2px solid gray",
      boxShadow: theme.shadows[5],
      padding: theme.spacing(2, 4, 3)
    }
  }));

interface PipelineModalProps {
    isOpen: boolean,
    children: React.ReactNode,
    handleClose: () => void
}

export default function PipelineModal({ isOpen, children, handleClose } : PipelineModalProps ) {
    const classes = useStyles();
    const theme = useTheme();
    //             <Button onClick={handleOpen}>Show pipeline</Button>

    return (
        <div>
            <Modal
                className={classes.modal}
                aria-labelledby="simple-modal-title"
                aria-describedby="simple-modal-description"
                open={isOpen}
                onClose={handleClose}
                closeAfterTransition
                BackdropComponent={Backdrop}
                BackdropProps={{
                  timeout: 500
                }}
            >
                <Fade in={isOpen}>
                    <div className={classes.paper} >
                        <IconButton
                            onClick={handleClose}
                            style={{cursor:'pointer', float:'right', marginTop: '5px', width: '20px'}}
                        >
                            <CloseIcon/>
                        </IconButton>
                        { children }
                    </div>
                </Fade>
            </Modal>
        </div>
    )
}