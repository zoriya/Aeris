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

export default function PipelineModal(props :any) {
    const classes = useStyles();
    const theme = useTheme();
    const [open, setOpen] = useState(false);

    const handleOpen = () => {
        setOpen(true);
    };
    const handleClose = () => {
        setOpen(false);
    };


    const divStyle: string = "width: " + props.width + "; height:" + props.height + ";"

    return (
        <div>
            <Button onClick={handleOpen}>Show pipeline</Button>
            <Modal
                className={classes.modal}
                aria-labelledby="simple-modal-title"
                aria-describedby="simple-modal-description"
                open={open}
                onClose={handleClose}
                closeAfterTransition
                BackdropComponent={Backdrop}
                BackdropProps={{
                  timeout: 500
                }}
            >
                <Fade in={open}>
                    <div className={classes.paper} >
                        <IconButton
                            onClick={handleClose}
                            style={{cursor:'pointer', float:'right', marginTop: '5px', width: '20px'}}
                        >
                            <CloseIcon/>
                        </IconButton>
                        { props.children }
                    </div>
                </Fade>
            </Modal>
        </div>
    )
}