import IconButton from "@mui/material/IconButton";
import CloseIcon from "@material-ui/icons/Close";
import { useTranslation } from 'react-i18next';
import Snackbar from '@mui/material/Snackbar';
import Button from "@material-ui/core/Button";
import React, {useState} from "react";
import '../i18n/config';

export default function LanguageSelector() {
    const { t, i18n } = useTranslation();
    const [open, OpenSnackbar] = useState<boolean>(false);

    const changeLanguage = (language: string) => {
        i18n.changeLanguage(language);
        OpenSnackbar(true);
    }

    const handleClose = (event: React.SyntheticEvent | Event, reason?: string) => {
        if (reason === 'clickaway') return;
        OpenSnackbar(false);
    };

    const action = (
        <React.Fragment>
            <IconButton
                size="small"
                aria-label="close"
                color="inherit"
                onClick={handleClose}
            >
                <CloseIcon fontSize="small" />
            </IconButton>
        </React.Fragment>
);

    return (
        <div>
            <Button onClick={() => {
                changeLanguage('fr');
            }}>
                <img loading="lazy" width="20"
                 src="https://upload.wikimedia.org/wikipedia/commons/c/c3/Flag_of_France.svg"/>
            </Button>
            <Button onClick={() => {
                changeLanguage('en');
            }}>
                <img loading="lazy" width="20"
                     src="https://upload.wikimedia.org/wikipedia/commons/a/ae/Flag_of_the_United_Kingdom.svg"/>
            </Button>
            <Snackbar
                open={open}
                autoHideDuration={1000}
                onClose={handleClose}
                message={t('switch_language')}
            />
        </div>
    )
}